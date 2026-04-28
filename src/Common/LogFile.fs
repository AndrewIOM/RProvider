namespace RProvider.Common

open System
open System.IO
open System.Diagnostics

module LogFile =

    let internal logEnvVar = "RPROVIDER_LOG"

    /// The logging is enabled by setting the RPROVIDER_LOG environment variable
    /// Alternatively, just change this constant to 'true' and logs will be
    /// saved in the default location (see below)
    let internal loggingEnabled = true //Environment.GetEnvironmentVariable(logEnvVar) <> null

    /// Log file - if the RPROVIDER_LOG variable is not set, the default on
    /// Windows is "C:\Users\<user>\AppData\Roaming\RLogs\log.txt" and on Mac
    /// this is in "/User/<user>/.config/RLogs/log.txt")
    let private logFile =
        try
            let var = Environment.GetEnvironmentVariable(logEnvVar)

            if var <> null then
                var
            else
                let appData = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
                if not (Directory.Exists(appData + "/RLogs")) then Directory.CreateDirectory(appData + "/RLogs") |> ignore
                appData + "/RLogs/log.txt"
        with
        | _ -> (* Silently ignoring logging errors *) null

    /// Append string to a log file
    let private writeString str =
        try
            // This serializes all writes to the log file (from multiple processes)
            use fs = new FileStream(logFile, FileMode.Append, FileAccess.Write, FileShare.Write, 4096, FileOptions.None)
            use writer = new StreamWriter(fs)
            writer.AutoFlush <- true

            let pid = Process.GetCurrentProcess().Id
            let tid = System.Threading.Thread.CurrentThread.ManagedThreadId
            let apid = System.AppDomain.CurrentDomain.Id
            writer.WriteLine(sprintf "[%s] [Pid:%d, Tid:%d, Apid:%d] %s" (DateTime.Now.ToString("G")) pid tid apid str)
        with
        | _ -> (*silently ignoring logging errors*) ()

    /// Log formatted string to a log file
    let logf fmt =
        let f = if loggingEnabled then writeString else ignore
        Printf.kprintf f fmt

