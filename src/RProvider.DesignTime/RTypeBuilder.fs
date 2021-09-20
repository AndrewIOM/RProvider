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
    
    /// Assuming initialization worked correctly, generate the types using R engine
    let generateTypes ns asm = withServer <| fun server ->
      [ // Expose all available packages as namespaces
        Logging.logf "generateTypes: getting packages"
        let getPackages = server.InvokeAsync(fun s -> s.GetPackages()) |> Async.AwaitTask
        let packages = 
          [ yield "base", ns
            for package in Async.RunSynchronously(getPackages, timeout = 30000) do yield package, ns + "." + package ]
        for package, pns in packages do
            let pty = ProvidedTypeDefinition(asm, pns, "R", Some(typeof<obj>))    

            pty.AddXmlDocDelayed <| fun () -> withServer <| fun serverDelayed -> 
              let getDesc = serverDelayed.InvokeAsync(fun s -> s.GetPackageDescription package) |> Async.AwaitTask
              Async.RunSynchronously(getDesc, timeout = 30000)
            pty.AddMembersDelayed( fun () -> 
              withServer <| fun serverDelayed ->
              [ let loadPackage = serverDelayed.InvokeAsync(fun s -> s.LoadPackage package) |> Async.AwaitTask
                Async.RunSynchronously(loadPackage, timeout = 30000)
                let getBindings = serverDelayed.InvokeAsync(fun s -> s.GetBindings package) |> Async.AwaitTask
                let bindings = Async.RunSynchronously(getBindings, timeout = 30000)

                // We get the function descriptions for R the first time they are needed
                let titles = lazy Map.ofSeq (withServer (fun s -> 
                  let getDesc = s.InvokeAsync(fun s -> s.GetFunctionDescriptions package) |> Async.AwaitTask
                  Async.RunSynchronously(getDesc, timeout = 30000)))

                for name, serializedRVal in bindings do
                    let memberName = makeSafeName name
                    match RInterop.deserializeRValue serializedRVal with
                    | RValue.Function(paramList, hasVarArgs) ->
                        let paramList = [ for p in paramList -> 
                                                ProvidedParameter(makeSafeName p,  typeof<obj>, optionalValue=null)

                                          if hasVarArgs then
                                            yield ProvidedParameter("paramArray", typeof<obj[]>, optionalValue=null, IsParamArray=true)
                                        ]
                        
                        let paramCount = paramList.Length
                        
                        let pm = ProvidedMethod(
                                      methodName = memberName,
                                      parameters = paramList,
                                      returnType = typeof<RDotNet.SymbolicExpression>,
                                      isStatic = true,
                                      invokeCode = fun args -> if args.Length <> paramCount then
                                                                 failwithf "Expected %d arguments and received %d" paramCount args.Length
                                                               if hasVarArgs then
                                                                 let namedArgs = 
                                                                     Array.sub (Array.ofList args) 0 (paramCount-1)
                                                                     |> List.ofArray
                                                                 let namedArgs = Quotations.Expr.NewArray(typeof<obj>, namedArgs)
                                                                 let varArgs = args.[paramCount-1]
                                                                 <@@ RInterop.call package name serializedRVal %%namedArgs %%varArgs @@>                                                 
                                                               else
                                                                 let namedArgs = Quotations.Expr.NewArray(typeof<obj>, args)                                            
                                                                 <@@ RInterop.call package name serializedRVal %%namedArgs [||] @@> )

                        pm.AddXmlDocDelayed (fun () -> match titles.Value.TryFind name with 
                                                        | Some docs -> docs 
                                                        | None -> "No documentation available")                                    
                        
                        yield pm :> MemberInfo

                        // Yield an additional overload that takes a Dictionary<string, object>
                        // This variant is more flexible for constructing lists, data frames etc.
                        let pdm = ProvidedMethod(
                                      methodName = memberName,
                                      parameters = [ ProvidedParameter("paramsByName",  typeof<IDictionary<string,obj>>) ],
                                      returnType = typeof<RDotNet.SymbolicExpression>,
                                      isStatic = true,
                                      invokeCode = fun args -> if args.Length <> 1 then
                                                                 failwithf "Expected 1 argument and received %d" args.Length
                                                               let argsByName = args.[0]
                                                               <@@  let vals = %%argsByName: IDictionary<string,obj>
                                                                    let valSeq = vals :> seq<KeyValuePair<string, obj>>
                                                                    RInterop.callFunc package name valSeq null @@> )
                        yield pdm :> MemberInfo                                    
                    | RValue.Value ->
                        yield ProvidedProperty(
                                propertyName = memberName,
                                propertyType = typeof<RDotNet.SymbolicExpression>,
                                isStatic = true,
                                getterCode = fun _ -> <@@ RInterop.call package name serializedRVal [| |] [| |] @@>) :> MemberInfo  ] )
                      
            yield pns, [ pty ] ]
    
    /// Check if R is installed - if no, generate type with properties displaying
    /// the error message, otherwise go ahead and use 'generateTypes'!
    let initAndGenerate providerAssembly = 
       [  // Get the assembly and namespace used to house the provided types
          Logging.logf "initAndGenerate: starting"
          let ns = "RProvider"
          failwith "Fail 2"
          match Async.RunSynchronously(tryGetInitializationError(), timeout = 30000) with
          | null -> 
              yield! generateTypes ns providerAssembly
          | error ->
              // add an error static property (shown when typing `R.`)
              let pty = ProvidedTypeDefinition(providerAssembly, ns, "R", Some(typeof<obj>))
              let prop = ProvidedProperty("<Error>", typeof<string>, isStatic = true, getterCode = fun _ -> <@@ error @@>)
              prop.AddXmlDoc error
              pty.AddMember prop
              yield ns, [ pty ]
              // add an error namespace (shown when typing `open RProvider.`)
              yield ns + ".Error: " + error, [ pty ]
          Logging.logf "initAndGenerate: finished" ]