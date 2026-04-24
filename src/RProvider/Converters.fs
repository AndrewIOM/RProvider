namespace RProvider.Runtime

open RBridge
open RBridge.Extensions
open RBridge.Extensions.ActivePatterns
open RProvider.Runtime.RTypes
open RProvider.Common

/// Contains conversion functions to convert from .NET types
/// to values in R, and from R to .NET types. Functions allow
/// extraction of R data to .NET memory, or expression of R objects
/// in .NET semantic wrappers.
module Convert =

    /// Default conversions from R to .NET primitive types or
    /// simple .NET representations provided by RBridge (for example,
    /// for RComplex). The values are extracted into .NET memory space.
    let tryFromRStructural<'outType> (engine: NativeApi.RunningEngine) (sexp: SymbolicExpression) : 'outType option =

        let retype (x: 'b) : Option<'outType> = x |> box |> unbox<'outType> |> Some
        let at = typeof<'outType>

        match sexp with

        // To array:
        | IntegerVector engine xs when at = typeof<int []> -> xs |> Extract.extractIntArray engine |> retype |> unbox
        | RealVector engine xs when at = typeof<float []> -> xs |> Extract.extractFloatArray engine |> retype |> unbox
        | LogicalVector engine xs when at = typeof<bool option []> ->
            xs |> Extract.extractLogicalArray engine |> retype |> unbox
        | CharacterVector engine xs when at = typeof<string []> ->
            xs |> Extract.extractStringArray engine |> retype |> unbox
        | ComplexVector engine xs when at = typeof<RComplex []> ->
            xs |> Extract.extractComplexArray engine |> retype |> unbox
        | RealVector engine xs when Dates.isDate engine xs && at = typeof<System.DateOnly[]> ->
            Extract.extractDateArray engine xs |> Array.map RDate.toDateOnly |> retype |> unbox
        | RealVector engine xs when Dates.isPosixDateTime engine xs && at = typeof<RDateTime[]> ->
            Extract.extractDateTimeArray engine xs |> Array.map RDateTime.toDateTimeUtc |> retype |> unbox

        // To list:
        | IntegerVector engine xs when at = typeof<int list> ->
            xs |> Extract.extractIntArray engine |> Array.toList |> retype |> unbox
        | RealVector engine xs when at = typeof<float list> ->
            xs |> Extract.extractFloatArray engine |> Array.toList |> retype |> unbox
        | LogicalVector engine xs when at = typeof<bool option list> ->
            xs |> Extract.extractLogicalArray engine |> Array.toList |> retype |> unbox
        | CharacterVector engine xs when at = typeof<string list> ->
            xs |> Extract.extractStringArray engine |> Array.toList |> retype |> unbox
        | ComplexVector engine xs when at = typeof<RComplex list> ->
            xs |> Extract.extractComplexArray engine |> Array.toList |> retype |> unbox
        | RealVector engine xs when Dates.isDate engine xs && at = typeof<RDate list> ->
            Extract.extractDateArray engine xs |> Array.map RDate.toDateOnly |> Array.toList |> retype |> unbox
        | RealVector engine xs when Dates.isPosixDateTime engine xs && at = typeof<RDateTime list> ->
            Extract.extractDateTimeArray engine xs |> Array.map RDateTime.toDateTimeUtc |> Array.toList |> retype |> unbox

        // To atomic:
        | IntegerVector engine xs when at = typeof<int> ->
            xs |> Extract.extractIntArray engine |> Array.head |> retype |> unbox
        | RealVector engine xs when at = typeof<float> ->
            xs |> Extract.extractFloatArray engine |> Array.head |> retype |> unbox
        | LogicalVector engine xs when at = typeof<bool option> ->
            xs |> Extract.extractLogicalArray engine |> Array.head |> retype |> unbox
        | CharacterVector engine xs when at = typeof<string> ->
            xs |> Extract.extractStringArray engine |> Array.head |> retype |> unbox
        | ComplexVector engine xs when at = typeof<RComplex> ->
            xs |> Extract.extractComplexArray engine |> Array.head |> retype |> unbox
        | RealVector engine xs when Dates.isDate engine xs && at = typeof<RDate> ->
            Extract.extractDateArray engine xs |> Array.map RDate.toDateOnly |> Array.head |> retype |> unbox
        | RealVector engine xs when Dates.isPosixDateTime engine xs && at = typeof<RDateTime> ->
            Extract.extractDateTimeArray engine xs |> Array.map RDateTime.toDateTimeUtc |> Array.head |> retype |> unbox

        // To matrix:
        | CharacterMatrix engine v when at = typeof<string [,]> ->
            v |> Extract.extractStringMatrix engine |> retype |> unbox
        | IntegerMatrix engine v when at = typeof<int [,]> -> v |> Extract.extractIntMatrix engine |> retype |> unbox
        | LogicalMatrix engine v when at = typeof<bool option [,]> ->
            v |> Extract.extractLogicalMatrix engine |> retype |> unbox

        // Empty vectors in R are represented as null
        | Null engine _ when at = typeof<string list> -> retype <| List.empty<string>
        | Null engine _ when at = typeof<string []> -> retype <| Array.empty<string>
        | Null engine _ when at = typeof<RComplex list> -> retype <| List.empty<RComplex>
        | Null engine _ when at = typeof<RComplex []> -> retype <| Array.empty<RComplex>
        | Null engine _ when at = typeof<int list> -> retype <| List.empty<int>
        | Null engine _ when at = typeof<int []> -> retype <| Array.empty<int>
        | Null engine _ when at = typeof<bool option list> -> retype <| List.empty<bool>
        | Null engine _ when at = typeof<bool option []> -> retype <| Array.empty<bool>
        | Null engine _ when at = typeof<double list> -> retype <| List.empty<double>
        | Null engine _ when at = typeof<double []> -> retype <| Array.empty<double>

        | IntegerVector engine xs when at = typeof<obj> -> xs |> Extract.extractIntArray engine |> retype |> unbox
        | RealVector engine xs when at = typeof<obj> -> xs |> Extract.extractFloatArray engine |> retype |> unbox
        | LogicalVector engine xs when at = typeof<obj> -> xs |> Extract.extractLogicalArray engine |> retype |> unbox
        | CharacterVector engine xs when at = typeof<obj> -> xs |> Extract.extractStringArray engine |> retype |> unbox
        | ComplexVector engine xs when at = typeof<obj> -> xs |> Extract.extractComplexArray engine |> retype |> unbox
        | CharacterMatrix engine v when at = typeof<obj> -> v |> Extract.extractStringMatrix engine |> retype |> unbox
        | IntegerMatrix engine v when at = typeof<obj> -> v |> Extract.extractIntMatrix engine |> retype |> unbox
        | LogicalMatrix engine v when at = typeof<obj> -> v |> Extract.extractLogicalMatrix engine |> retype |> unbox
        | Null engine _ when at = typeof<obj> -> List.empty |> retype |> unbox

        | _ ->
            LogFile.logf
                "Cannot convert SexpType %A to %s"
                (SymbolicExpression.getType engine sexp)
                typeof<'outType>.FullName

            None

    /// Convert a value from a symbolic expression into a wrapped
    /// R type included in RProvider. The types are shaped so as to
    /// be usable directly by statistics libraries that support them,
    /// without the need for a plugin converter system.
    /// Conversion between basic numeric types and containers (e.g.
    /// data frame) should be done using explicit conversion functions.
    let tryAsRTyped engine sexp : RTypes.RSemantic<'u> option =
        LogFile.logf "Classified as %A" (classify engine sexp)
        match classify engine sexp with
        | FactorType -> Factor.tryFromExpression sexp |> Option.map FactorInR
        | DataFrameType -> DataFrame.tryFromExpression sexp |> Option.map DataFrameInR
        | VectorType -> GenericVector.tryFromExpression sexp |> Option.map VectorInR        
        | ScalarType -> GenericScalar.tryFromExpression sexp |> Option.map ScalarInR
        | ListType -> HeterogeneousList.tryFromExpression sexp |> Option.map ListInR
        | MatrixType
        | ArrayType
        | FunctionType
        | EnvironmentType
        | S3ObjectType
        | S4ObjectType
        | R6ObjectType -> None

    let private toDateTimeVector (eng: NativeApi.RunningEngine) (dates: System.DateTime seq) =
        let converted =
            dates |> Seq.map(fun d ->        
                match d.Kind with
                | System.DateTimeKind.Utc -> d, Some "UTC"
                | System.DateTimeKind.Local -> d.ToUniversalTime(), Some System.TimeZoneInfo.Local.Id
                | System.DateTimeKind.Unspecified -> System.DateTime.SpecifyKind(d, System.DateTimeKind.Utc), Some "UTC"
                | _ -> failwith "Unexpected date time kind %i" d.Kind
            )
            |> Seq.toArray
        let seconds = converted |> Array.map(fun (utc,_) -> (utc - System.DateTime(1970,1,1,0,0,0,System.DateTimeKind.Utc)).TotalSeconds)
        let timezones = converted |> Array.map snd |> Array.distinct
        let timezone =
            match timezones with
            | [| tz |] -> tz
            | _ -> failwith "Cannot mix timezones or pass an empty date list as a date-time vector to R."
        Create.dateTimeVector eng seconds timezone

    let private toRDateVector engine (dates: System.DateOnly seq) =
        dates
        |> Seq.map (fun d -> d.DayNumber - RDate.unixEpochDayNumber)
        |> Create.dateVector engine

    let toR (eng: NativeApi.RunningEngine) (value: obj) : SymbolicExpression =
        match value with

        // Pass-through of basic R expression values:
        | :? SymbolicExpression as s -> s
        | :? RProvider.Abstractions.RExpr as s -> RExprWrapper.toRBridge s
        | :? RBridge.Extensions.REnvironment as s -> s.AsSymbolicExpression

        // .NET primitives:
        | null -> { ptr = eng.Api.nilValue }
        | :? string as s -> Create.stringVector eng [| s |]
        | :? array<string> as xs -> Create.stringVector eng xs
        | :? list<string> as xs -> Create.stringVector eng xs
        | :? int as i -> Create.intVector eng [| i |]
        | :? array<int> as xs -> Create.intVector eng xs
        | :? list<int> as xs -> Create.intVector eng xs
        | :? float as f -> Create.realVector eng [| f |]
        | :? array<float> as xs -> Create.realVector eng xs
        | :? list<float> as xs -> Create.realVector eng xs
        | :? bool as b -> Create.logicalVector eng [| b |]
        | :? array<bool> as xs -> Create.logicalVector eng xs
        | :? list<bool> as xs -> Create.logicalVector eng xs
        | :? RComplex as c -> Create.complexVector eng [| c |]
        | :? array<RComplex> as cs -> Create.complexVector eng cs
        | :? list<RComplex> as cs -> Create.complexVector eng cs
        | :? System.DateOnly as d -> toRDateVector eng [| d |]
        | :? array<System.DateOnly> as d -> toRDateVector eng d
        | :? list<System.DateOnly> as d -> toRDateVector eng d
        | :? System.DateTime as d -> toDateTimeVector eng [ d ]
        | :? array<System.DateTime> as d -> toDateTimeVector eng d
        | :? list<System.DateTime> as d -> toDateTimeVector eng d

        // Pass-through of RProvider semantic types:
        | :? RSemantic<_> as sem -> sem.Sexp
        | :? RVector<_> as v -> v.Sexp
        | :? RScalar<_> as s -> s.Sexp
        | :? DataFrame.RFrame as df -> df.Sexp
        | :? DataFrame.Column.Column as col -> DataFrame.Column.getSexp col
        | :? Factor.RFactor as f -> f.Sexp
        | :? HeterogeneousList.HList as hlist -> hlist.Sexp
        | :? Real.Vector.RRealVector<_> as v -> v.Inner.Sexp
        | :? VectorBase.RVectorBase<_> as v -> v.Sexp
        | :? Real.Scalar.RRealScalar<_> as s -> s.Sexp

        | _ ->
            failwithf
                "The value you passed cannot be converted to an R object.\n\
                Type: %s\n\
                Value: %A\n\
                Hint: RProvider only supports built-in conversion of primitive types, arrays, and lists.\n\
                Tuples of length > 2 are not automatically converted. Consider converting it manually. Note: F# lists are seperated with semi-colons, not commas."
                (value.GetType().Name)
                value
