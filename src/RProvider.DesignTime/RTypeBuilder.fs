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

                                                        let namedArgs =
                                                            Array.sub (Array.ofList args) 0 (paramCount - 1)
                                                            |> List.ofArray

                                                        let nameValueTuples =
                                                            (paramList |> List.take (paramCount - 1), namedArgs)
                                                            ||> List.map2 (fun p argExpr ->
                                                                Quotations.Expr.NewTuple [
                                                                    Quotations.Expr.Value p.Name
                                                                    argExpr 
                                                                ] )

                                                        let namedArgsArray = Quotations.Expr.NewArray(typeof<string * obj>, nameValueTuples)
                                                        let varArgs = args.[paramCount - 1]

                                                        <@@
                                                            // let namedArgs = %%(buildNamedArgsExpr namedPairs) : RExpr
                                                            let globEnv = RProvider.Runtime.IRInteropRuntime.globalEnvironment()
                                                            RProvider.Runtime.IRInteropRuntime.callFuncByName
                                                                globEnv
                                                                package
                                                                name
                                                                %%namedArgsArray
                                                                %%varArgs @@>
                                                    else
                                                        // All args are positional named args
                                                        let nameValueTuples =
                                                            (paramList, args)
                                                            ||> List.map2 (fun p argExpr ->
                                                                Quotations.Expr.NewTuple [
                                                                    Quotations.Expr.Value p.Name
                                                                    argExpr 
                                                                ] )

                                                        let namedArgsArray = Quotations.Expr.NewArray(typeof<string * obj>, nameValueTuples)
                                                        let emptyVarArgs = Quotations.Expr.NewArray(typeof<obj>, [])

                                                        <@@ let globEnv = RProvider.Runtime.IRInteropRuntime.globalEnvironment()
                                                            RProvider.Runtime.IRInteropRuntime.callFuncByName
                                                                globEnv
                                                                package
                                                                name
                                                                %%namedArgsArray
                                                                %%emptyVarArgs @@>
                                        )
                                    LogFile.logf "Made provided method %A" pm
                                    
                                    let addDoc () =
                                        match titles.TryFind name with
                                        | Some docs -> docs
                                        | None -> "No documentation available"
                                    
                                    pm.AddXmlDocDelayed addDoc
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
                                                let namedArgs : (string * obj)[] = vals |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toArray
                                                let varArgs : obj[] = [||]
                                                let globEnv = RProvider.Runtime.IRInteropRuntime.globalEnvironment()
                                                RProvider.Runtime.IRInteropRuntime.callFuncByName globEnv package name namedArgs varArgs @@> )
                                    pdm.AddXmlDocDelayed addDoc

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
                                                let namedArgs : (string * obj)[] = vals |> List.toArray
                                                let varArgs : obj[] = [||]
                                                let globEnv = RProvider.Runtime.IRInteropRuntime.globalEnvironment()
                                                RProvider.Runtime.IRInteropRuntime.callFuncByName globEnv package name namedArgs varArgs @@> )
                                                
                                    plm.AddXmlDocDelayed addDoc
                                    yield plm :> MemberInfo

                                | RValue.Value ->
                                    LogFile.logf "-- Value -- %s" memberName
                                    yield
                                        ProvidedProperty(
                                            propertyName = memberName,
                                            propertyType = typeof<RExpr>,
                                            isStatic = true,
                                            getterCode =
                                                fun _ ->
                                                    <@@
                                                        let globEnv = RProvider.Runtime.IRInteropRuntime.globalEnvironment()
                                                        RProvider.Runtime.IRInteropRuntime.call globEnv package name serializedRVal Array.empty Array.empty @@>
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
