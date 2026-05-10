namespace RProvider.Runtime

open RBridge
open RProvider.Common
open RBridge.Logging

/// Global singletons used by RProvider.
module Singletons =

    /// Global variable keeping track of loaded packages in R.
    let internal loadedPackages = System.Collections.Generic.HashSet<string>()

    /// Lazily initialized value that, find the R location or fails and returns RInitError
    let rLocation =
        lazy
            (None // TODO used to be tryHomeFromConfig (that home root rprovider.config file)
             |> Option.map EngineHost.tryFromRHome
             |> Option.defaultWith EngineHost.tryFindSystemR)

    let internal rBridge =
        { debug = fun s -> LogFile.logf "[RBridge] {Debug} %s" s
          info = fun s -> LogFile.logf "[RBridge] {Info} %s" s }

    /// Lazily initialized R engine.
    let internal engine =
        lazy
            (
                LogFile.logf "engine: Creating and initializing instance (sizeof<IntPtr>=%d)" System.IntPtr.Size
                match rLocation.Force() with
                | Some res -> RInterop.initialiseAt res rBridge
                | None -> failwith "Error: could not locate an R install"
            )
