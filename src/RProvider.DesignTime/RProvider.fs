namespace RProvider.DesignTime

open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open RProvider
open RProvider.Common
open System
open System.Threading
open System.IO.Pipes
open System.IO
open System.Text

[<TypeProvider>]
type public RProvider(cfg: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg,
        assemblyReplacementMap = [ "RProvider.DesignTime", "RProvider" ])

    let runtimeAssembly = Assembly.LoadFrom cfg.RuntimeAssembly

    // Generate all the types and log potential errors
    let buildTypes () =
        try
            LogFile.logf "[DesignTime] RProvider constructor started"

            for ns, types in RTypeBuilder.initAndGenerate  runtimeAssembly do
                this.AddNamespace(ns, types)

            LogFile.logf "[DesignTime] RProvider constructor succeeded"
        with
        | e ->
            LogFile.logf "[DesignTime] RProvider constructor failed: %O" e
            reraise ()

    do buildTypes ()
