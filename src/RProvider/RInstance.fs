namespace RProvider.Runtime

open RBridge

/// Global singletons used by RProvider.
module Singletons =

    /// Global interceptor that captures R console output
    let mutable internal characterDevice = Devices.Intercept.create Devices.Console.defaultDevice

    /// Global variable keeping track of loaded packages in R.
    let internal loadedPackages = System.Collections.Generic.HashSet<string>()

    /// Lazily initialized value that, find the R location or fails and returns RInitError
    let rLocation =
        lazy (
            None // TODO used to be tryHomeFromConfig (that home root rprovider.config file)
            |> Option.map EngineHost.tryFromRHome
            |> Option.defaultWith EngineHost.tryFindSystemR
        )

    /// Lazily initialized R engine.
    let internal engine =
        lazy (
            Logging.logf "engine: Creating and initializing instance (sizeof<IntPtr>=%d)" System.IntPtr.Size
            match rLocation.Force() with
            | Some res ->
                match RInterop.initialiseAt res with
                | NativeApi.Running r -> r
                | NativeApi.NotRunning err -> failwithf "Error: could not start R; %s" err
            | None -> failwith "Error: could not locate an R install"
        )
