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
        lazy (
            None // TODO used to be tryHomeFromConfig (that home root rprovider.config file)
            |> Option.map EngineHost.tryFromRHome
            |> Option.defaultWith EngineHost.tryFindSystemR
        )

    open System

    /// TODO This has been copied here from RBridge.
    /// fsac locks up when used directly from RBridge, but
    /// not sure of the reason why.
    let initialiseAt (loc: EngineHost.RLocation) (logger: Logging.Logger) : NativeApi.REngine =

        // set R_HOME if not already
        if String.IsNullOrEmpty(Environment.GetEnvironmentVariable "R_HOME") then
            Environment.SetEnvironmentVariable("R_HOME", loc.RHome)

        // ensure the library/bin directory is on PATH
        let oldPath =
            Environment.GetEnvironmentVariable "PATH"

        let sep =
            if Environment.OSVersion.Platform = PlatformID.Win32NT then
                ";"
            else
                ":"

        let newPath =
            loc.RBin
            + sep
            + (if isNull oldPath then "" else oldPath)

        Environment.SetEnvironmentVariable("PATH", newPath)

        // load the R API and cache delegates
        // logger.debug <| sprintf "loading native R library at %s" loc.DllPath
        let engine = NativeApi.loadApi loc.DllPath

        // initialise the embedded R runtime (argc/argv may be empty)
        // follow rdotnet's sequence: set start time then call Rf_initialize_R.
        NativeApi.setStartTime engine

        // logger.debug <| sprintf  "R_HOME=%s" (Environment.GetEnvironmentVariable "R_HOME")
        // logger.debug <| sprintf  "PATH=%s" (Environment.GetEnvironmentVariable "PATH")
        Environment.SetEnvironmentVariable("LC_NUMERIC", "C")

        // R expects argv[0] to be program name; supply a dummy value
        // build a minimal argv similar to rdotnet's BuildRArgv
        let args: string [] =
            [| "REmbeddedFSharpBridge"
               "--quiet"
               "--gui=none"
               "--no-save"
               "--no-restore-data"
               "--no-site-file"
               "--no-init-file" |]

        // let savedLocale = Locale.saveLocaleEnv ()
        // logger.debug <| sprintf  "Saved locale: %A" savedLocale
        logger.debug <| sprintf "Culture before restore = %A" System.Globalization.CultureInfo.CurrentCulture
        logger.debug <| sprintf  "Starting embedded R"
        let status = NativeApi.startEmbeddedR args engine
        logger.debug <| sprintf  "Rf_initEmbeddedR returned %d" status
        logger.debug <| sprintf "Culture after restore = %A" System.Globalization.CultureInfo.CurrentCulture
        // RBridge.RInterop.Locale.setEnvironment savedLocale

        // update nilValue, globalEnv and other global vars now that R has been initialised;
        // previous reads returned 0 because the globals hadn't been set yet.
        logger.debug "refreshing environment values"

        NativeApi.refreshEnvironmentValues engine
        |> NativeApi.Running

    let internal rBridge = {
        debug = fun s -> LogFile.logf "[RBridge] {Debug} %s" s
        info = fun s -> LogFile.logf "[RBridge] {Info} %s" s
    }

    /// Lazily initialized R engine.
    let internal engine =
        lazy (
            LogFile.logf "engine: Creating and initializing instance (sizeof<IntPtr>=%d)" System.IntPtr.Size
            match rLocation.Force() with
            | Some res ->
                match initialiseAt res rBridge with
                | NativeApi.Running r -> r
                | NativeApi.NotRunning err -> failwithf "Error: could not start R; %s" err
            | None -> failwith "Error: could not locate an R install"
        )
