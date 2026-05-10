namespace RProvider.Common

open System
open System.IO
open System.Diagnostics

module LogFile =

    let internal logEnvVar = "RPROVIDER_LOG"

    /// The logging is enabled by setting a log file in the RPROVIDER_LOG environment variable.
    /// Set this constant to 'true' and logs will be
    /// saved in the default location (see below).
    let private loggingEnabled =
        match Environment.GetEnvironmentVariable logEnvVar with
        | null -> false
        | v when v.Equals("true", StringComparison.OrdinalIgnoreCase) -> true
        | v when v = "1" -> true
        | v when v.Equals("on", StringComparison.OrdinalIgnoreCase) -> true
        | _ -> false

    let private isWindows =
        Environment.OSVersion.Platform = PlatformID.Win32NT

    let private isMacOS =
        Environment.OSVersion.Platform = PlatformID.MacOSX ||
            (Environment.OSVersion.Platform = PlatformID.Unix && Directory.Exists "/Library/Logs")

    let private isLinux =
        Environment.OSVersion.Platform = PlatformID.Unix && not isMacOS

    /// Resolve the correct platform-specific log directory.
    let private getLogDirectory () =
        if isWindows then
            let baseDir = Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData
            Path.Combine(baseDir, "RProvider")
        elif isMacOS then
            let home = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
            Path.Combine(home, "Library", "Logs", "RProvider")
        elif isLinux then
            let xdg = Environment.GetEnvironmentVariable "XDG_STATE_HOME"
            let baseDir =
                if String.IsNullOrWhiteSpace xdg then
                    let home = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
                    Path.Combine(home, ".local", "state")
                else xdg
            Path.Combine(baseDir, "RProvider")
        else failwith "Unsupported platform"

    /// Log file - if the RPROVIDER_LOGF variable is set, a logfile named
    /// RProvider/rprovider.log is created in the OS's default log location.
    /// Windows: %LOCALAPPDATA%\
    /// macOS: ~/Library/Logs/
    /// linux: ~/.local/state/
    let private logFile =
        try
            if loggingEnabled then
                let logFolder = getLogDirectory ()
                if not (Directory.Exists logFolder) then Directory.CreateDirectory logFolder |> ignore
                Path.Combine(logFolder, "rprovider.log") |> Some
            else None
        with
        | _ -> (*silently ignoring logging errors*) None

    /// Append string to a log file
    let private writeString str =
        try
            match logFile with
            | Some lf ->
                // This serializes all writes to the log file (from multiple processes)
                use fs = new FileStream(lf, FileMode.Append, FileAccess.Write, FileShare.Write, 4096, FileOptions.None)
                use writer = new StreamWriter(fs)
                writer.AutoFlush <- true

                let pid = Process.GetCurrentProcess().Id
                let tid = System.Threading.Thread.CurrentThread.ManagedThreadId
                let apid = System.AppDomain.CurrentDomain.Id
                writer.WriteLine(sprintf "[%s] [Pid:%d, Tid:%d, Apid:%d] %s" (DateTime.Now.ToString("G")) pid tid apid str)
            | None -> ()
        with
        | _ -> (*silently ignoring logging errors*) ()

    /// Log formatted string to a log file
    let logf fmt =
        let f = if loggingEnabled then writeString else ignore
        Printf.kprintf f fmt

