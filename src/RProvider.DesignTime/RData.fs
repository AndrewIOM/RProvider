namespace RProvider.DesignTime

open System.IO
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

open RProvider
open RProvider.Common
open RProvider.Runtime
open RProvider.Abstractions
open RProvider.Common.InteropServer

[<TypeProvider>]
type public RDataProvider(cfg: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg)

    // NOTE: No need to register 'AssemblyResolve' event handler
    // here, because this is already done in static constructor of RProvider.

    /// Helper for writing InvokeCode
    let (|Singleton|) =
        function
        | [ v ] -> v
        | _ -> failwith "Expected one argument."

    let resolve resolutionFolder path =
        if Path.IsPathRooted path then path
        elif not (System.String.IsNullOrWhiteSpace resolutionFolder) then
            Path.Combine(resolutionFolder, path)
        else
            Path.Combine(cfg.ResolutionFolder, path)

    /// Given a file name, generate static type inherited from REnv
    let generateTypes asm typeName (args: obj []) =
        LogFile.logf "Generating type for %s" typeName
        // Load the environment and generate the type
        let fileName = args.[0] :?> string
        let resolutionDir = args.[1] :?> string

        let longFileName = resolve resolutionDir fileName
        if not <| File.Exists longFileName then
            let msg =
                sprintf "RProvider: The RData file '%s' does not exist. \
                        Resolved path: '%s'. \
                        Set ResolutionFolder = __SOURCE_DIRECTORY__ if using a relative path."
                    fileName longFileName
            raise (FileNotFoundException msg)

        let resTy = ProvidedTypeDefinition(asm, "RProvider", typeName, Some typeof<RData>)

        let ctor = ProvidedConstructor(parameters = [], invokeCode = fun _ -> <@@ IRInteropRuntime.loadRDataFile longFileName @@> )
        resTy.AddMember(ctor)

        let ctor =
            ProvidedConstructor(
                parameters = [ ProvidedParameter("fileName", typeof<string>) ],
                invokeCode = fun (Singleton fn) -> <@@ IRInteropRuntime.loadRDataFile (%%fn : string) @@>
            )

        resTy.AddMember(ctor)

        // For each key in the environment, provide a property..
        let response : (string * option<System.Type>)[] = RInteropClient.server.Value.Call (ServerRequest.GetRDataSymbols longFileName)
        LogFile.logf "Got response from server: %A" response
        for name, typ in response do
            LogFile.logf "Adding member %s" name

            match typ with
            | None ->
                // Generate property of type 'SymbolicExpression'
                ProvidedProperty(
                    name,
                    typeof<RExpr>,
                    getterCode = fun (Singleton self) -> <@@ IRInteropRuntime.getRDataSymbol ((%%self : RData)) name @@>
                )
                |> resTy.AddMember
            | Some typ ->
                // If there is a default convertor for the type, then generate
                // property of the statically known type (e.g. Frame<string, string>)
                // (otherwise, `Value` will throw)
                let miGetTyped =
                    typeof<IRInteropRuntime>.GetMethod "getRDataSymbolTyped"
                    |> fun mi -> mi.MakeGenericMethod [| typ |]

                ProvidedProperty(
                    name,
                    typ,
                    getterCode = fun (Singleton self) ->
                        Quotations.Expr.Call(
                            miGetTyped,
                            [ Quotations.Expr.Coerce(self, typeof<RData>)
                              Quotations.Expr.Value(name) ]
                        )
                )
                |> resTy.AddMember

        LogFile.logf "Finished generating types for %s" longFileName
        resTy

    // Register the main (parameterized) type with F# compiler
    // Provide tye 'RProvider.RData<FileName>' type
    let asm = Assembly.LoadFrom cfg.RuntimeAssembly

    let rdata = ProvidedTypeDefinition(asm, "RProvider", "RData", Some(typeof<obj>))
    let parameters = [
        ProvidedStaticParameter("FileName", typeof<string>)
        ProvidedStaticParameter("ResolutionFolder", typeof<string>, "")
    ]

    let helpText =
        """<summary>Typed representation of an .rdata file.</summary>
           <param name='FileName'>Location of an .rdata file from which to infer structure.</param>
           <param name='ResolutionFolder'>If using a relative path, the folder from which to resolve the relative path.</param>"""

    do rdata.AddXmlDoc helpText

    do
        rdata.DefineStaticParameters(parameters, generateTypes asm)
        LogFile.logf "Defined static Parameters %O" parameters

    do
        this.AddNamespace("RProvider", [ rdata ])
        LogFile.logf "RData added namespace %s" rdata.FullName
