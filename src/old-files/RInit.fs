/// [omit]
module RProvider.Internal.RInit

open System
open RBridge

/// Represents R value used in initialization or information about failure
type RInitResult<'T> =
    | RInitResult of 'T
    | RInitError of string

let configFileName = ".rprovider.conf"

let tryHomeFromConfig () =
    let res = Configuration.getRProviderConfValue "R_HOME"
    Logging.logf "Checking '~/.rprovider.conf' for R_HOME value: %A" res
    res


/// Global singletons used by RProvider.
module Singletons =

    /// Global interceptor that captures R console output
    let mutable internal characterDevice = RProvider.Devices.Intercept.create Devices.Console.defaultDevice

    /// Global variable keeping track of loaded packages in R.
    let internal loadedPackages = System.Collections.Generic.HashSet<string>()

    /// Lazily initialized value that, find the R location or fails and returns RInitError
    let rLocation =
        lazy (
            tryHomeFromConfig ()
            |> Option.map EngineHost.tryFromRHome
            |> Option.defaultWith EngineHost.tryFindSystemR
        )

    /// Lazily initialized R engine.
    let internal engine =
        lazy (
            Logging.logf "engine: Creating and initializing instance (sizeof<IntPtr>=%d)" IntPtr.Size
            match rLocation.Force() with
            | Some res ->
                match RInterop.initialiseAt res with
                | NativeApi.Running r -> r
                | NativeApi.NotRunning err -> failwithf "Error: could not start R; %s" err
            | None -> failwith "Error: could not locate an R install"
        )
