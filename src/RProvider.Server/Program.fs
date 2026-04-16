namespace RProvider.Server

open System
open System.Diagnostics
open System.IO
open System.IO.Pipes
open RProvider.Common
open RProvider.Common.InteropServer
open System.Text

module Main =

    /// Process.WaitForExit does not seem to be working reliably
    /// on Mono, so instead we loop asynchronously until the process is gone
    let rec asyncWaitForExit pid =
        async {
            let parentProcess =
                try
                    let p = Process.GetProcessById(pid)

                    match p.HasExited with
                    | true -> None
                    | false -> Some p
                with
                | _ -> None

            match parentProcess with
            | Some _ ->
                do! Async.Sleep(1000)
                return! asyncWaitForExit pid
            | None -> ()
        }

    let dispatch (server: IRInteropServer) (req: ServerRequest) : ServerResponse =
        try
            match req with
            | InitializationErrorMessage ->
                server.InitializationErrorMessage() |> InitializationErrorMessageResult

            | GetPackages ->
                server.GetPackages() |> Packages

            | LoadPackage pkg ->
                server.LoadPackage pkg
                UnitResult

            | GetBindings pkg ->
                server.GetBindings pkg |> Bindings

            | GetFunctionDescriptions pkg ->
                server.GetFunctionDescriptions pkg |> FunctionDescriptions

            | GetPackageDescription pkg ->
                server.GetPackageDescription pkg |> PackageDescription

            | GetRDataSymbols path ->
                server.GetRDataSymbols path |> RDataSymbols

        with e ->
            ServerError e.Message

    /// Start the server using the specified channel name (which
    /// contains the parent PID) and delete tempFile once we're running
    let startServer (pipeName: string) tempFile =

        let server =
            new NamedPipeServerStream(
                pipeName,
                PipeDirection.InOut,
                1,
                PipeTransmissionMode.Byte,
                PipeOptions.None
            )

        // Start parent‑process watcher (same as before)
        let parentPid = pipeName.Split('_').[1] |> int
        let parentProcess = Process.GetProcessById(parentPid)
        LogFile.logf "Waiting for parent process pid=%d (%A)" parentPid parentProcess

        async {
            do! asyncWaitForExit parentPid
            LogFile.logf "Posting Stop command"
            EventLoop.queue.Add Stop
        }
        |> Async.Start

        // Connection + RPC loop on a background async
        async {
            LogFile.logf "Ready for connections.."

            do! server.WaitForConnectionAsync() |> Async.AwaitTask
            LogFile.logf "Server: connected."

            use reader = new BinaryReader(server, Encoding.UTF8)
            use writer = new BinaryWriter(server, Encoding.UTF8)

            let impl = RInteropServer() :> IRInteropServer

            let rec loop () = async {
                let len =
                    try reader.ReadInt32()
                    with _ -> 0

                if len = 0 then
                    LogFile.logf "Server: pipe closed, exiting loop."
                else
                    let bytes = reader.ReadBytes len
                    let req =
                        use ms = new MemoryStream(bytes)
                        use br = new BinaryReader(ms, Encoding.UTF8)
                        Request.read br
                    LogFile.logf "Server: recieved request %A" req

                    let resp = dispatch impl req
                    LogFile.logf "Server: sending response %A" resp
                    let respBytes =
                        use ms = new MemoryStream()
                        use bw = new BinaryWriter(ms, Encoding.UTF8)
                        Response.write bw resp
                        bw.Flush()
                        ms.ToArray()

                    writer.Write respBytes.Length
                    writer.Write respBytes
                    writer.Flush()

                    return! loop ()
            }

            do! loop ()
        }
        |> Async.StartAsTask |> ignore

        File.Delete tempFile


    [<STAThread>]
    [<EntryPoint>]
    let main argv =
        try
            LogFile.logf "Starting 'RProvider.Server' with arguments '%A'" argv

            // The first argument is the pipe server name to create; The second argument
            // is a temp file that we delete, once we setup the pipe to
            // signal back that we are ready (in a Unix-compatible low-tech way)
            if argv.Length <> 2 then failwith "Expected usage: RProvider.Server.exe <ipc channel> <temp file name>"
            if not (File.Exists(argv.[1])) then failwith "File passed as the second argument must exist!"

            // Expose the server object via pipe server
            startServer argv.[0] argv.[1]
            LogFile.logf "Server started, running event loop"

            // Run Event Loop until the parent process stops
            EventLoop.startEventLoop ()
            LogFile.logf "Event loop finished, shutting down"
            0
        with
        | e ->
            LogFile.logf "RProvider.Server' failed: %A" e
            reraise ()
