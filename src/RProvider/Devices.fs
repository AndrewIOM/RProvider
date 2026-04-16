namespace RProvider.Runtime

open System.Text
open RBridge.Devices

module Devices =

    module Intercept =

        type Intercepted =
            | Capturing of CharacterDevice * StringBuilder
            | NotCapturing of CharacterDevice

        let private writeFn innerWrite (buffer:StringBuilder option) (str: string) =
            match buffer with
            | None -> innerWrite str
            | Some sb -> sb.Append str |> ignore

        let create device =
            NotCapturing { device with WriteConsole = writeFn device.WriteConsole None }

        let beginCapture (device:Intercepted) =
            match device with
            | Capturing _ -> device
            | NotCapturing d ->
                let sb = new StringBuilder ()
                Capturing ({ d with WriteConsole = writeFn d.WriteConsole (Some sb) }, sb)

        let endCapture device =
            match device with
            | Capturing (dev, sb) -> sb.ToString(), create dev
            | NotCapturing dev -> "", create dev