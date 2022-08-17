namespace RProvider

open System.Collections.Generic
open System.Reflection
open ProviderImplementation.ProvidedTypes
open RProvider
open RProvider.Internal
open RInterop
open RInteropClient
open PipeMethodCalls

module internal RTypeBuilder =

    let generateForPackage asm package pns = async {
        let pty = ProvidedTypeDefinition(asm, pns, "R", Some(typeof<obj>))

        pty.AddXmlDocDelayed
        <| fun () ->
            withServer
            <| fun serverDelayed ->
                serverDelayed.InvokeAsync(fun s -> s.GetPackageDescription package)
                |> Async.AwaitTask
            |> Async.RunSynchronously

        pty.AddMembersDelayed(fun () ->
            withServer(fun serverDelayed -> async {
                do! serverDelayed.InvokeAsync(fun s -> s.LoadPackage package) |> Async.AwaitTask
                let! bindings = serverDelayed.InvokeAsync(fun s -> s.GetBindings package) |> Async.AwaitTask

                // We get the function descriptions for R the first time they are needed
                let titles =
                    lazy
                        (Map.ofSeq (
                            withServer
                                (fun s -> s.InvokeAsync(fun s -> s.GetFunctionDescriptions package) |> Async.AwaitTask)
                            |> Async.RunSynchronously
                        ))
                
                return bindings
                |> Array.toList
                |> List.collect(fun (name, serializedRVal) -> [

                        let memberName = makeSafeName name

                        match RInterop.deserializeRValue serializedRVal with
                        | RValue.Function (paramList, hasVarArgs) ->
                            let paramList =
                                paramList
                                |> List.collect(fun p ->
                                    [ 
                                        yield ProvidedParameter(makeSafeName p, typeof<obj>, optionalValue = null)
                                        if hasVarArgs then
                                            yield ProvidedParameter(
                                                        "paramArray",
                                                        typeof<obj []>,
                                                        optionalValue = null,
                                                        IsParamArray = true
                                                    )]) 
                                
                            let paramCount = paramList.Length

                            let pm =
                                ProvidedMethod(
                                    methodName = memberName,
                                    parameters = paramList,
                                    returnType = typeof<RDotNet.SymbolicExpression>,
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

                                                let namedArgs =
                                                    Quotations.Expr.NewArray(typeof<obj>, namedArgs)

                                                let varArgs = args.[paramCount - 1]

                                                <@@ RInterop.call
                                                        package
                                                        name
                                                        serializedRVal
                                                        %%namedArgs
                                                        %%varArgs @@>
                                            else
                                                let namedArgs = Quotations.Expr.NewArray(typeof<obj>, args)

                                                <@@ RInterop.call
                                                        package
                                                        name
                                                        serializedRVal
                                                        %%namedArgs
                                                        [||] @@>
                                )

                            pm.AddXmlDocDelayed
                                (fun () ->
                                    match titles.Value.TryFind name with
                                    | Some docs -> docs
                                    | None -> "No documentation available")

                            yield pm :> MemberInfo

                            // Yield an additional overload that takes a Dictionary<string, object>
                            // This variant is more flexible for constructing lists, data frames etc.
                            let pdm =
                                ProvidedMethod(
                                    methodName = memberName,
                                    parameters =
                                        [ ProvidedParameter(
                                            "paramsByName",
                                            typeof<IDictionary<string, obj>>
                                        ) ],
                                    returnType = typeof<RDotNet.SymbolicExpression>,
                                    isStatic = true,
                                    invokeCode =
                                        fun args ->
                                            if args.Length <> 1 then
                                                failwithf "Expected 1 argument and received %d" args.Length

                                            let argsByName = args.[0]

                                            <@@ let vals: IDictionary<string, obj> = %%argsByName
                                                let valSeq = vals :> seq<KeyValuePair<string, obj>>
                                                RInterop.callFunc package name valSeq null @@>
                                )

                            yield pdm :> MemberInfo
                        | RValue.Value ->
                            yield ProvidedProperty(
                                propertyName = memberName,
                                propertyType = typeof<RDotNet.SymbolicExpression>,
                                isStatic = true,
                                getterCode =
                                    fun _ -> <@@ RInterop.call package name serializedRVal [||] [||] @@>
                            )
                            :> MemberInfo
                        ])
            }) |> Async.RunSynchronously)

        return (pns, [ pty ])
    }


    /// Assuming initialization worked correctly, generate the types using R engine
    let generateTypes ns asm =
        withServer
        <| fun server -> async {
            let! rPackages = server.InvokeAsync(fun s -> s.GetPackages()) |> Async.AwaitTask
            let packages = 
                ("base", ns) :: 
                (rPackages |> Array.map(fun package -> package, ns + "." + makeSafeName package) |> Array.toList)
            return packages |> List.map(fun (package, pns) -> generateForPackage asm package pns |> Async.RunSynchronously) // TODO remove RunSynch
        }

    /// Check if R is installed - if no, generate type with properties displaying
    /// the error message, otherwise go ahead and use 'generateTypes'!
    let initAndGenerate providerAssembly localConfig =
        async {
            // Get the assembly and namespace used to house the provided types
            let ns = "RProvider"
            let! error = tryGetInitializationError ()
            match error with
            | null -> 
                // Setup local R package store if required
                if localConfig.LocalCache.IsSome then 
                    do! withServer <| fun server ->
                        server.InvokeAsync(fun s -> s.AddPackagePath localConfig.LocalCache.Value) |> Async.AwaitTask
                
                // Check that R install has packages user has requested in config file
                if localConfig.RequiredPackages.Length > 0 then 
                    do! withServer <| fun server -> async {
                        let! installedPackages = server.InvokeAsync(fun s -> s.GetPackages()) |> Async.AwaitTask
                        if Set.ofList (localConfig.RequiredPackages |> List.map fst)
                            |> Set.isSubset (Set.ofArray installedPackages)
                            |> not
                        then 
                            if localConfig.Strict then failwith "Packages are required that are not installed."
                            else
                                Logging.logf "Packages are required that are not installed. Missing packages are %A"
                                    (Set.ofList localConfig.RequiredPackages |> Set.filter (fun p -> installedPackages |> Array.contains (fst p)))
                        else
                            // Check package version constraints
                            let! packageVersions = server.InvokeAsync(fun s -> s.GetPackageVersions()) |> Async.AwaitTask
                            let mismatches =
                                localConfig.RequiredPackages
                                |> Seq.choose(fun (p,v) ->
                                    packageVersions
                                    |> Seq.find (fun (s,_) -> s = p)
                                    |> (fun (_,v2) -> 
                                        if RConfig.matchesVersion v2 v
                                        then None
                                        else Some (p, v2, v)))
                            if mismatches |> Seq.isEmpty |> not then
                                if localConfig.Strict then failwithf "Packages are required that are not installed: %A" mismatches
                                else
                                    Logging.logf "Package version mismatch between r.dependencies and installed for: %A"
                                        (Set.ofList localConfig.RequiredPackages |> Set.filter (fun p -> installedPackages |> Array.contains (fst p)))
                    }

                return! generateTypes ns providerAssembly
            | error ->
                // add an error static property (shown when typing `R.`)
                let pty = ProvidedTypeDefinition(providerAssembly, ns, "R", Some(typeof<obj>))

                let prop =
                    ProvidedProperty("<Error>", typeof<string>, isStatic = true, getterCode = fun _ -> <@@ error @@>)

                prop.AddXmlDoc error
                pty.AddMember prop
                return [ 
                    ns, [ pty ]
                    // add an error namespace (shown when typing `open RProvider.`)
                    ns + ".Error: " + error, [ pty ] ]
        }