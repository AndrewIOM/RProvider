namespace RProvider.Common

open System.Text
open Newtonsoft.Json
open PipeMethodCalls

/// Serialisation of R values and functions to and from strings,
/// for use in communication between the type provider and the server.
module Serialisation =

    module Settings =

        let private settings = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)

        /// We are using Newtonsoft rather than System.Text.Json because
        /// the latter does not support System.Type.
        type NewtonsoftJsonPipeSerializer() =
            interface IPipeSerializer with
                member this.Deserialize(data, ``type``) =
                    JsonConvert.DeserializeObject(Encoding.UTF8.GetString(data), ``type``, settings)

                member this.Serialize(o) =
                    let json = JsonConvert.SerializeObject(o, settings)
                    Encoding.UTF8.GetBytes(json)

    type RParameter = string
    type HasVarArgs = bool

    type RValue =
        | Function of RParameter list * HasVarArgs
        | Value

    /// Turn an `RValue` (which captures type information of a value or function)
    /// into a serialized string that can be spliced in a quotation
    let serializeRValue =
        function
        | RValue.Value -> ""
        | RValue.Function (pars, hasVar) ->
            let prefix = if hasVar then "1" else "0"
            prefix + if List.isEmpty pars then "" else ";" + (String.concat ";" pars)

    /// Given a string produced by `serializeRValue`, reconstruct the original RValue object
    let deserializeRValue serialized =
        if isNull serialized then
            invalidArg "serialized" "Unexpected null string"
        elif serialized = "" then
            RValue.Value
        else
            let hasVar =
                match serialized.[0] with
                | '1' -> true
                | '0' -> false
                | _ -> invalidArg "serialized" "Should start with a flag"

            let args = if serialized.Length = 1 then [] else List.ofSeq (serialized.Substring(2).Split(';'))
            RValue.Function(args, hasVar)

