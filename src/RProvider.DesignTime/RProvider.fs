namespace RProvider.DesignTime

open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open RProvider
open RProvider.Common

[<TypeProvider>]
type public RProvider(cfg: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg,
        assemblyReplacementMap = [ "RProvider.DesignTime", "RProvider.Runtime" ])

    let useReflectionOnly = false //true

    let runtimeAssembly =
        if useReflectionOnly then
            let coreAssembly = typeof<obj>.Assembly
            let resolver = PathAssemblyResolver([ cfg.RuntimeAssembly; coreAssembly.Location ])
            use mlc = new MetadataLoadContext(resolver, coreAssemblyName = coreAssembly.GetName().Name)
            LogFile.logf "Loading runtime assembly %O" mlc
            mlc.LoadFromAssemblyPath cfg.RuntimeAssembly
        else
            Assembly.LoadFrom cfg.RuntimeAssembly

    // Generate all the types and log potential errors
    let buildTypes () =
        try
            LogFile.logf "Starting build types."

            // for ns, types in RTypeBuilder.initAndGenerate (runtimeAssembly) do
            //     this.AddNamespace(ns, types)

            LogFile.logf "RProvider constructor succeeded"
        with
        | e ->
            LogFile.logf "RProvider constructor failed: %O" e
            reraise ()

    do buildTypes ()
