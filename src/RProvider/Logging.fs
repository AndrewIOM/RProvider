namespace RProvider.Runtime

module Logging =

    open RProvider.Common
    open RBridge.Logging

    let rBridge = {
        debug = fun s -> LogFile.logf "[RBridge] {Debug} %s" s
        info = fun s -> LogFile.logf "[RBridge] {Info} %s" s
    }

    /// Run the specified function and log potential exceptions, as well
    /// as the output written to console (unless characterDevice.IsCapturing).
    /// TODO Doesn't work in new refactor; requires reworking.
    let internal logWithOutput (characterDevice: Devices.Intercept.Intercepted) f =
        // if LogFile.loggingEnabled then
        //     try
        //         // If the device is capturing stuff for someone else, then
        //         // we do not want to touch it (it is a minor limitation only)
        //         let capturing = characterDevice.IsCapturing

        //         try
        //             if not capturing then
        //                 let dev = Devices.Intercept.beginCapture characterDevice
        //                 f ()
        //             else f ()
        //         finally
        //             let out = if not capturing then Devices.Intercept.endCapture characterDevice |> fst else "(could not be captured)"
        //             LogFile.logf "Output: %s" out
        //     with
        //     | e ->
        //         LogFile.logf "Operation failed:\r\n  {%O}" e
        //         reraise ()
        // else
            f ()
