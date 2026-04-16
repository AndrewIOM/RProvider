namespace RProvider.DesignTime

open System.Collections.Generic
open ProviderImplementation.ProvidedTypes
open RProvider.Common
open RProvider.Common.InteropServer
open System.Collections.Concurrent
open RProvider.Common.Serialisation
open System.Reflection
open RProvider.Abstractions

module internal RTypeBuilder =

    /// Replace dots in R names with underscores, making
    /// them safe for use as .NET type names.
    let makeSafeName (name: string) = name.Replace("_", "__").Replace(".", "_")

    type PackageMetadata =
        {
            Load : Lazy<unit>
            Bindings : Lazy<(string * string)[]>
            Titles : Lazy<Map<string,string>>
            XmlDoc : Lazy<string>
        }

    let packageCache = ConcurrentDictionary<string, PackageMetadata>()

    /// Lazily loads a package and gets the bindings and descriptions for functions
    /// within the package.
    let getPackageMetadata (server: RInteropClient.RawPipeClient) package =
        packageCache.GetOrAdd(package, fun _ ->
            {
                Load = lazy (
                        LogFile.logf "[DesignTime] Loading package: %s" package
                        server.Call <| ServerRequest.LoadPackage package)
                Bindings = lazy (
                        LogFile.logf "[DesignTime] Getting bindings for: %s" package
                        server.Call <| ServerRequest.GetBindings package)
                Titles = lazy (
                    LogFile.logf "[DesignTime] Getting titles for: %s" package
                    server.Call<(string * string)[]> (ServerRequest.GetFunctionDescriptions package)
                    |> Map.ofSeq
                )
                XmlDoc = lazy (
                    LogFile.logf "[DesignTime] Getting XML docs for: %s" package
                    server.Call <| ServerRequest.GetPackageDescription package)
            }
        )

    let buildNamedArgsExpr (pairs: (string * Quotations.Expr) list) =
        let ctor = typeof<Dictionary<string,RExpr>>.GetConstructor [||]
        let add  = typeof<Dictionary<string,RExpr>>.GetMethod "Add"
        let dictExpr = Quotations.Expr.NewObject(ctor, [])
        for name, argExpr in pairs do
            Quotations.Expr.Call(dictExpr, add,
                [ Quotations.Expr.Value name; <@@ RExpr.wrap %%argExpr @@> ])
            |> ignore
        <@@ RExpr.wrap %%dictExpr @@>

    let buildVarArgsExpr (varArgExpr: Quotations.Expr) =
        <@@
            let arr = (%%varArgExpr : obj[]) |> Array.map RExpr.wrap
            RExpr.wrap arr
        @@>


    /// Assuming initialization worked correctly, generate the types using R engine
    let generateTypes ns asm =
            [ // Expose all available packages as namespaces
              LogFile.logf "[DesignTime] Fetching package list."
              let packages =
                  [ yield "base", ns
                    for package in
                        RInteropClient.server.Value.Call GetPackages do
                        yield package, ns + "." + makeSafeName package ]

              LogFile.logf "[DesignTime] Packages found: %A" packages

              for package, pns in packages do
                  let pty = ProvidedTypeDefinition(asm, pns, "R", Some(typeof<obj>))

                  pty.AddXmlDocDelayed
                  <| fun () ->
                      try
                        let md = getPackageMetadata RInteropClient.server.Value package
                        md.XmlDoc.Force()
                      with ex ->
                        LogFile.logf "XmlDoc error for package %s: %O" package ex
                        sprintf "RProvider: documentation for package '%s' is unavailable (%s)" package ex.Message

                  pty.AddMembersDelayed (fun () ->
                    
                    LogFile.logf "[DesignTime] Computing delayed member (%s)" package
                    try
                        [ 
                            // We get the function descriptions for R the first time they are needed
                            let md = getPackageMetadata RInteropClient.server.Value package
                            let bindings = md.Bindings.Force()
                            let titles = md.Titles.Force()
                            LogFile.logf "Bindings were %A" bindings
                            LogFile.logf "Titles were %A" titles

                            for name, serializedRVal in bindings do
                                let memberName = makeSafeName name

                                match deserializeRValue serializedRVal with
                                | RValue.Function (paramList, hasVarArgs) ->
                                    LogFile.logf "-- Function -- %s (%A)" memberName paramList
                                    let paramList =
                                        [ 
                                            for p in paramList ->
                                                ProvidedParameter(makeSafeName p, typeof<obj>, optionalValue = null)

                                            if hasVarArgs then
                                                yield
                                                    ProvidedParameter(
                                                        "paramArray",
                                                        typeof<obj []>,
                                                        optionalValue = null,
                                                        IsParamArray = true
                                                    ) ]

                                    let paramCount = paramList.Length
                                    LogFile.logf "Param count = %i" paramCount
                                    let pm =
                                        ProvidedMethod(
                                            methodName = memberName,
                                            parameters = paramList,
                                            returnType = typeof<RExpr>,
                                            isStatic = true,
                                            invokeCode =
                                                fun args ->
                                                    if args.Length <> paramCount then
                                                        failwithf
                                                            "Expected %d arguments and received %d"
                                                            paramCount
                                                            args.Length

                                                    if hasVarArgs then

                                                        let fixedArgs  = args |> List.take (paramCount - 1)
                                                        let fixedNames = paramList |> List.take (paramCount - 1) |> List.map (fun p -> p.Name)
                                                        let namedPairs = List.zip fixedNames fixedArgs
                                                        let varArgsExpr = buildVarArgsExpr args.[paramCount - 1]

                                                        <@@
                                                            let namedArgs = %%(buildNamedArgsExpr namedPairs) : RExpr
                                                            let globEnv = IRInteropRuntime.globalEnvironment()
                                                            IRInteropRuntime.call
                                                                globEnv
                                                                package
                                                                name
                                                                serializedRVal
                                                                namedArgs
                                                                %%varArgsExpr @@>
                                                    else
                                                        // All args are positional named args
                                                        let argNames = paramList |> List.map (fun p -> p.Name)
                                                        let namedPairs = List.zip argNames args

                                                        let namedArgsExpr = buildNamedArgsExpr namedPairs
                                                        let emptyVarArgsExpr = <@@ RExpr.wrap ([||] : RExpr[]) @@>

                                                        <@@ let globEnv = IRInteropRuntime.globalEnvironment()
                                                            IRInteropRuntime.call
                                                                globEnv
                                                                package
                                                                name
                                                                serializedRVal
                                                                %%namedArgsExpr
                                                                %%emptyVarArgsExpr @@>
                                        )
                                    LogFile.logf "Made provided method %A" pm
                                    pm.AddXmlDocDelayed
                                        (fun () ->
                                            match titles.TryFind name with
                                            | Some docs -> docs
                                            | None -> "No documentation available")
                                    LogFile.logf "Added XML"

                                    yield pm :> MemberInfo                                        
                                    LogFile.logf "Yielded"

                                    let byName t q =
                                        ProvidedMethod(
                                            methodName = memberName,
                                            parameters =
                                                [ ProvidedParameter(
                                                        "paramsByName",
                                                        t
                                                    ) ],
                                            returnType = typeof<RExpr>,
                                            isStatic = true,
                                            invokeCode =
                                                fun args ->
                                                    if args.Length <> 1 then
                                                        failwithf "Expected 1 argument and received %d" args.Length
                                                    let argsByName = args.[0]
                                                    q argsByName
                                        )

                                    // Yield an additional overload that takes a Dictionary<string, object>
                                    // This variant is more flexible for constructing lists, data frames etc.
                                    let pdm =
                                        byName typeof<IDictionary<string, obj>> (fun argsByName -> 
                                            <@@ let vals : IDictionary<string,obj> = %%argsByName
                                                let namedArgs = RExprInterop.buildNamedArgsFromDict vals
                                                let globEnv = IRInteropRuntime.globalEnvironment()
                                                IRInteropRuntime.callFuncByName globEnv package name namedArgs RExprInterop.emptyVarArgs @@> )

                                    yield pdm :> MemberInfo

                                    // Yield alternative overload that takes a list of string * obj.
                                    // This option requires less boilerplate (i.e. no namedParams).
                                    let plm =
                                        byName (typeof<(string * obj) list>) (fun argsByName -> 
                                            <@@ 
                                                
                                                let vals : (string * obj) list = %%argsByName
                                                let duplicates =
                                                    vals
                                                    |> List.groupBy fst
                                                    |> List.filter (fun (_, set) -> set.Length > 1)

                                                if duplicates |> List.isEmpty |> not then failwithf "Recieved duplicate arguments: %A" (duplicates |> List.map fst)
                                                let pairs =
                                                    vals |> List.map (fun (k,v) -> k, Quotations.Expr.Value v)

                                                let namedArgs = RExprInterop.buildNamedArgsFromList vals
                                                let globEnv = IRInteropRuntime.globalEnvironment()
                                                IRInteropRuntime.callFuncByName globEnv package name namedArgs RExprInterop.emptyVarArgs @@> )
                                                
                                    yield plm :> MemberInfo

                                | RValue.Value ->
                                    LogFile.logf "-- Value -- %s" memberName
                                    yield
                                        ProvidedProperty(
                                            propertyName = memberName,
                                            propertyType = typeof<RExpr>,
                                            isStatic = true,
                                            getterCode =
                                                fun _ -> <@@
                                                        let globEnv = IRInteropRuntime.globalEnvironment()
                                                        IRInteropRuntime.call globEnv package name serializedRVal
                                                            (RExprInterop.buildNamedArgsFromList []) RExprInterop.emptyVarArgs @@>
                                        )
                                        :> MemberInfo ]
                    with ex ->
                        LogFile.logf "Members error for package %s: %O" package ex
                        []
                    )
                  yield pns, [ pty ] ]

    /// Check if R is installed - if no, generate type with properties displaying
    /// the error message, otherwise go ahead and use 'generateTypes'!
    let initAndGenerate providerAssembly =
        [ // Get the assembly and namespace used to house the provided types
          LogFile.logf "initAndGenerate: starting"
          let ns = "RProvider"
          match RInteropClient.tryGetInitializationError () with
          | "" ->
            LogFile.logf "[DesignTime] R server found. Generating types."
            yield! generateTypes ns providerAssembly
          | error ->
              // add an error static property (shown when typing `R.`)
              let pty = ProvidedTypeDefinition(providerAssembly, ns, "R", Some(typeof<obj>))

              let prop =
                  ProvidedProperty("<Error>", typeof<string>, isStatic = true, getterCode = fun _ -> <@@ error @@>)

              prop.AddXmlDoc error
              pty.AddMember prop
              yield ns, [ pty ]
              // add an error namespace (shown when typing `open RProvider.`)
              yield ns + ".Error: " + error, [ pty ]

          LogFile.logf "initAndGenerate: finished" ]
