namespace RProvider.Internal

/// Represents an optional configuration file for RProvider
/// containing user setings for type generation (e.g., local package location).
type ConfigFile = {
    RVersion: (VersionNumber * VersionPin) option
    LocalCache: string option
    RequiredPackages: (string * (VersionNumber * VersionPin) option) list
    /// Throw an error if required packages are missing
    Strict: bool
    Restore: bool
    PackageMirror: string
} with

    /// The configuration used by RProvider if no user-defined
    /// configuration is present.
    static member Default = {
        RVersion = None
        LocalCache = None
        RequiredPackages = []
        Strict = false
        Restore = false
        PackageMirror = "https://mran?"
    }

and VersionNumber =
    | Major of int
    | Minor of int * int
    | Patch of int * int * int

and VersionPin =
    | Exact
    | LatestPatch
    | LatestMinor

module RConfig =

    open System.Text.RegularExpressions

    let tryParse lines =
        { 
        RVersion = None
        LocalCache = None
        RequiredPackages = [ "ggplot2", Some (Minor (1,1), LatestPatch); "dplyr", None ]
        Strict = true
        Restore = false
        PackageMirror = "https://mran?" } |> Some

    /// Tests if an R package version string matches a version number
    /// and pin constraint given in a configuration file.
    let matchesVersion versionString v =
        match v with
        | None -> true
        | Some v ->
            match fst v with
            | Major maj -> 
                match snd v with
                | Exact -> versionString = sprintf "^%i" maj
                | LatestPatch -> Regex.IsMatch(versionString, sprintf "^%i.0*" maj)
                | LatestMinor -> Regex.IsMatch(versionString, sprintf "^%i.*" maj)
            | Minor(maj, min) ->
                match snd v with
                | Exact -> versionString = sprintf "^%i.%i" maj min
                | LatestPatch -> Regex.IsMatch(versionString, sprintf "^%i.%i*" maj min)
                | LatestMinor -> Regex.IsMatch(versionString, sprintf "^%i.*" maj)
            | Patch(maj, min, pat) ->
                match snd v with
                | Exact -> versionString = sprintf "^%i.%i.%i" maj min pat
                | LatestPatch -> Regex.IsMatch(versionString, sprintf "^%i.%i*" maj min)
                | LatestMinor -> Regex.IsMatch(versionString, sprintf "^%i.*" maj)
