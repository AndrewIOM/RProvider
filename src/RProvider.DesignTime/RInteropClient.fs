namespace RProvider.DesignTime

open System
open System.IO.Pipes
open System.Reflection
open System.IO
open System.Diagnostics
open System.Threading
open PipeMethodCalls
open RProvider.Common
open System.Runtime.InteropServices

module RInteropClient =

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
    let mutable lastServer: PipeClient<IRInteropServer> option = None
    let serverLock = obj ()

    /// Returns the real assembly location - when shadow copying is enabled, this
    /// returns the original assembly location (which may contain other files we need)
    let getAssemblyLocation (assem: Assembly) =
        if AppDomain.CurrentDomain.ShadowCopyFiles then (Uri(assem.Location)).LocalPath else assem.Location

    let startNewServerAsync () : Async<PipeClient<IRInteropServer>> =
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

        // If this is Mac or Linux, we try to run "chmod" to make the server executable
        if Environment.OSVersion.Platform = PlatformID.Unix || Environment.OSVersion.Platform = PlatformID.MacOSX then
            LogFile.logf "Setting execute permission on '%s'" exePath

            try
                Process.Start("chmod", "+x '" + exePath + "'").WaitForExit()
            with
            | _ -> ()

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

        // TODO Do we need to set R_HOME here any more?
        // Can we pull it in from environment?
        // if startInfo.EnvironmentVariables.ContainsKey("R_HOME") |> not then
        //     LogFile.logf "R_HOME not set"

        //     match RProvider.Internal.RInit.Singletons.rLocation.Force() with
        //     | Some config ->
        //         LogFile.logf "Setting R_HOME as %s" config.RHome
        //         startInfo.EnvironmentVariables.Add("R_HOME", config.RHome)
        //     | None ->
        //         LogFile.logf "Starting server process: Could not find R"
        //         ()

        // LogFile.logf "R_HOME set as %O" startInfo.EnvironmentVariables.["R_HOME"]

        // Start the process and wait until it is initialized
        // (after initialization, the process deletes the temp file)
        let p = Process.Start(startInfo)

        if not (waitUntilFileDeleted tempFile (20. * 1000.)) then
            failwith (
                "Failed to start the R.NET server within 20 seconds."
                + "To enable LogFile set RPROVIDER_LOG to an existing file name."
            )

        if not <| isNull p then
            p.EnableRaisingEvents <- true
            p.Exited.Add(fun _ -> lastServer <- None)

        LogFile.logf "Attempting to connect via inter-process communication"
        let rawPipeStream = new NamedPipeClientStream(".", channelName, PipeDirection.InOut, PipeOptions.Asynchronous)
        let pipeClient = new PipeClient<IRInteropServer>(Serialisation.Settings.NewtonsoftJsonPipeSerializer(), rawPipeStream)
        LogFile.logf "Made pipe client with state: %A" pipeClient.State

        async {
            LogFile.logf "Attempting to connect pipe client..."
            pipeClient.SetLogger(fun a -> LogFile.logf "[Client Pipe log]: %O" a)
            do! pipeClient.ConnectAsync() |> Async.AwaitTask
            return pipeClient
        }

    /// Returns an instance of `RInteropServer` started via IPC
    /// in a separate `RProvider.Server.dll` process (or if the server
    /// is already running, returns an existing instance)
    let getServer () =
        LogFile.logf "[Get server]"

        lock
            serverLock
            (fun () ->
                LogFile.logf "[Check last server]"

                match lastServer with
                | Some s ->
                    LogFile.logf "[Found lastServer]"
                    s
                | None ->
                    LogFile.logf "[Make new server]"
                    // TODO Remove RunSynchronously
                    let serverInstance = startNewServerAsync () |> Async.RunSynchronously
                    lastServer <- Some serverInstance
                    LogFile.logf "Got some server"
                    serverInstance)

    /// Returns Some("...") when there is an 'expected' kind of error that we want
    /// to show in the IntelliSense in a pleasant way (R is not installed, registry
    /// key is missing or .rprovider.conf is missing)
    let tryGetInitializationError () =
        try
            let server = getServer ()
            LogFile.logf "Sending command: get init error message..."
            server.InvokeAsync(fun s -> s.InitializationErrorMessage()) |> Async.AwaitTask
        with
        | RInitializationException err -> async { return err }

    let withServer f =
        lock serverLock
        <| fun () ->
            let serverInstance = getServer ()
            f serverInstance
