namespace RProvider.DesignTime

open System
open System.IO
open System.IO.Pipes
open System.Reflection
open System.Diagnostics
open System.Threading
open System.Runtime.InteropServices
open System.Text
open RProvider.Common
open RProvider.Common.InteropServer

module RInteropClient =

    type RawPipeClient(pipeName: string) =

        // Connect synchronously (safe for type providers)
        let stream =
            new NamedPipeClientStream(
                ".",
                pipeName,
                PipeDirection.InOut,
                PipeOptions.Asynchronous
            )

        do
            stream.Connect(5000) // 5 second timeout
            LogFile.logf "RawPipeClient: connected to '%s'" pipeName

        let reader = new BinaryReader(stream, Encoding.UTF8)
        let writer = new BinaryWriter(stream, Encoding.UTF8)

        member _.Call<'T>(req: ServerRequest) : 'T =

            let reqBytes =
                use ms = new MemoryStream()
                use bw = new BinaryWriter(ms, Encoding.UTF8)
                Request.write bw req
                bw.Flush ()
                ms.ToArray ()

            writer.Write reqBytes.Length
            writer.Write reqBytes
            writer.Flush ()

            // Read response
            let len = reader.ReadInt32()
            let respBytes = reader.ReadBytes(len)
            let resp =
                use ms = new MemoryStream(respBytes)
                use br = new BinaryReader(ms, Encoding.UTF8)
                Response.read br

            match resp with
            | InitializationErrorMessageResult s -> s :> obj :?> 'T
            | Packages pkgs -> pkgs :> obj :?> 'T
            | UnitResult -> Unchecked.defaultof<'T>
            | Bindings b -> b :> obj :?> 'T
            | FunctionDescriptions f -> f :> obj :?> 'T
            | PackageDescription d -> d :> obj :?> 'T
            | RDataSymbols syms -> syms :> obj :?> 'T
            | ServerError msg -> failwith msg

    [<Literal>]
    let Server = "RProvider.Server"

    /// Thrown when we want to show the specified string as a friendly error message to the user
    exception RInitializationException of string

    let waitUntilFileDeleted file timeout =
        let dt = DateTime.Now

        while File.Exists(file) && (DateTime.Now - dt).TotalMilliseconds < timeout do
            Thread.Sleep(10)

        not (File.Exists(file))

    /// Creates a new channel name in the format: RInteropServer_<pid>_<time>_<random>
    let newChannelName () =
        let randomSalt = Random()
        let pid = Process.GetCurrentProcess().Id
        let salt = randomSalt.Next()
        let tick = Environment.TickCount
        sprintf "RInteropServer_%d_%d_%d" pid tick salt

    // Global variables for remembering the current server
    let mutable lastServer: RawPipeClient option = None
    let serverLock = obj ()

    /// Returns the real assembly location - when shadow copying is enabled, this
    /// returns the original assembly location (which may contain other files we need)
    let getAssemblyLocation (assem: Assembly) =
        if AppDomain.CurrentDomain.ShadowCopyFiles then (Uri(assem.Location)).LocalPath else assem.Location

    let startNewServer () : RawPipeClient =
        LogFile.logf "Starting new connection to server from client"
        let channelName = newChannelName ()
        let tempFile = Path.GetTempFileName()

        // Find the location of RProvider.Server.exe (based on non-shadow-copied path!)
        let assem = Assembly.GetExecutingAssembly()
        let assemblyLocation = assem |> getAssemblyLocation
        let pkgRoot = Directory.GetParent(assemblyLocation).Parent.Parent.Parent.FullName
        let serverRoot = Path.Combine(pkgRoot, "tools", "server")
        let arguments = channelName + " \"" + tempFile + "\""

        // Find RProvider.Server relevant platform-specific self-contained executable
        let exePath =
            let architecture =
                if RuntimeInformation.OSArchitecture = Architecture.Arm64 then "arm64"
                else if RuntimeInformation.OSArchitecture = Architecture.X64 then "x64"
                else failwithf "Your platform (%s) is not currently supported by RProvider." (RuntimeInformation.OSArchitecture.ToString())
            if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
                Path.Combine(Path.GetDirectoryName(serverRoot), "server", sprintf "osx-%s" architecture, "publish", Server)
            else if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
                Path.Combine(Path.GetDirectoryName(serverRoot), "server", sprintf "linux-%s" architecture, "publish", Server)
            else if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                Path.Combine(Path.GetDirectoryName(serverRoot), "server", sprintf "win-%s" architecture, "publish", Server + ".dll")
            else
                failwithf "Your OS (%s) is not currently supported by RProvider." RuntimeInformation.FrameworkDescription

        // Log some information about the process first
        LogFile.logf "Starting server '%s' with arguments '%s' (exists=%b)" exePath arguments (File.Exists(exePath))

        let startInfo =
            ProcessStartInfo(
                UseShellExecute = false,
                CreateNoWindow = true,
                FileName = exePath,
                Arguments = arguments,
                WindowStyle = ProcessWindowStyle.Hidden,
                WorkingDirectory = Path.GetDirectoryName(assemblyLocation)
            )

        // Start the process and wait until it is initialized
        // (after initialization, the process deletes the temp file)
        let p = Process.Start startInfo

        if not (waitUntilFileDeleted tempFile (10. * 1000.)) then
            failwith (
                "Failed to start the R.NET server within ten seconds."
                + "To enable LogFile set RPROVIDER_LOG to an existing file name."
            )

        if not <| isNull p then
            p.EnableRaisingEvents <- true
            p.Exited.Add(fun _ -> lastServer <- None)

        LogFile.logf "Attempting to connect via inter-process communication"
        let pipeClient =
            try
                new RawPipeClient(channelName)
            with e ->
                LogFile.logf "RawPipeClient connection failed: %O" e
                reraise()

        LogFile.logf "Made pipe client."
        pipeClient


    /// Returns an instance of `RInteropServer` started via IPC
    /// in a separate `RProvider.Server.dll` process (or if the server
    /// is already running, returns an existing instance)
    let server : Lazy<RawPipeClient> =
        lazy
            LogFile.logf "Starting R server (design-time)..."
            let s =
                startNewServer ()
            LogFile.logf "R server started."
            s

    /// Returns Some("...") when there is an 'expected' kind of error that we want
    /// to show in the IntelliSense in a pleasant way (R is not installed, registry
    /// key is missing or .rprovider.conf is missing)
    let tryGetInitializationError () =
        try server.Value.Call<string> ServerRequest.InitializationErrorMessage
        with
        | RInitializationException err -> err
