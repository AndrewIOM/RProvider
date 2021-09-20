﻿namespace RProvider

open System
open System.IO
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open RProvider
open RProvider.Internal.Configuration
open RProvider.Internal

[<TypeProvider>]
type public RProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg)

    let useReflectionOnly = false//true
        
    let runtimeAssembly =
        Logging.logf $"Looking for assembly..."
        if useReflectionOnly
        then
            let coreAssembly = typeof<obj>.Assembly
            let resolver = PathAssemblyResolver([ cfg.RuntimeAssembly; coreAssembly.Location ])
            use mlc = new MetadataLoadContext(resolver, coreAssemblyName = coreAssembly.GetName().Name)
            mlc.LoadFromAssemblyPath cfg.RuntimeAssembly
        else 
          Logging.logf "Loading assembly %s" cfg.RuntimeAssembly
          Assembly.LoadFrom cfg.RuntimeAssembly

    static do 
      Logging.logf $"Debug Constructor 2"
      // When RProvider is installed via NuGet, the RDotNet assembly and plugins
      // will appear typically in "../../*/lib/net40". To support this, we look at
      // RProvider.dll.config which has this pattern in custom key "ProbingLocations".
      // Here, we resolve assemblies by looking into the specified search paths.
      AppDomain.CurrentDomain.add_AssemblyResolve(fun source args ->
        resolveReferencedAssembly args.Name)
      
    // Generate all the types and log potential errors
    let buildTypes () =
        Logging.logf "Building types"
        try 
          for ns, types in RTypeBuilder.initAndGenerate(runtimeAssembly) do
            //failwith "Fail B. NS = %s . Logs are %A" ns (Logging.debugLogs)
            this.AddNamespace(ns, types)
          Logging.logf $"RProvider constructor succeeded"
          //failwithf "Fail A. Logs are %A" (Logging.debugLogs)
        with e ->
          Logging.logf $"RProvider constructor failed: {e}"
          reraise()
    do buildTypes ()
