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

    /// Represents a value extracted from R into a .NET
    /// type, in a standard form that uses (1) arrays, and
    /// (2) options to represent NAs.
    type internal ExtractedFromR =
        | Int of int option[]
        | Float of float option[]
        | Bool of bool option[]
        | String of string option[]
        | Complex of RComplex option[]
        | Date of System.DateOnly option[]
        | DateTime of System.DateTime option[]
        | IntMatrix of int option[,]
        | FloatMatrix of float option[,]
        | BoolMatrix of bool option[,]
        | StringMatrix of string option[,]
        | Empty
        | NoConverter

    /// Converts an extracted value into an obj, where
    /// the obj contains an option wrapping the internal
    /// representation.
    let internal extractedToObj = function
        | Int v -> v |> box |> Some
        | Float v -> v |> box |> Some
        | Bool v -> v |> box |> Some
        | String v -> v |> box |> Some
        | Complex v -> v |> box |> Some
        | Date v -> v |> box |> Some
        | DateTime v -> v |> box |> Some
        | IntMatrix v -> v |> box |> Some
        | FloatMatrix v -> v |> box |> Some
        | BoolMatrix v -> v |> box |> Some
        | StringMatrix v -> v |> box |> Some
        | Empty -> None
        | NoConverter -> None

    /// Extract an R value to a standard form
    /// (an array of option types) for an appropriate
    /// .NET equivalent type.
    let internal extractFromR engine sexp =
        match sexp with
        | Null engine sexp -> Empty
        | IntegerMatrix engine v -> Extract.extractIntMatrix engine v |> IntMatrix
        | LogicalMatrix engine v -> Extract.extractLogicalMatrix engine v |> BoolMatrix
        | CharacterMatrix engine v -> Extract.extractStringMatrix engine v |> StringMatrix
        | RealMatrix engine v -> Extract.extractDoubleMatrix engine v |> FloatMatrix
        | IntegerVector engine xs -> Extract.extractIntArray engine xs |> Int
        | LogicalVector engine xs -> Extract.extractLogicalArray engine xs |> Bool
        | CharacterVector engine xs -> Extract.extractStringArray engine xs |> String
        | ComplexVector engine xs -> Extract.extractComplexArray engine xs |> Complex
        | RealVector engine xs when Dates.isDate engine xs ->
            Extract.extractDateArray engine xs
            |> Array.map (Option.map RDate.toDateOnly)
            |> Date
        | RealVector engine xs when Dates.isPosixDateTime engine xs ->
            Extract.extractDateTimeArray engine xs
            |> Array.map (Option.map RDateTime.toDateTimeUtc)
            |> DateTime
        | RealVector engine xs -> Extract.extractFloatArray engine xs |> Float
        | _ -> NoConverter

    let private isList (t:System.Type) = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>>
    let private isOption (t:System.Type) = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    /// Reshape the standard 'T option [] form of an
    /// extracted R value into the desired output container type.
    let internal reshape<'outType> extractedVal : 'outType option =
        let t = typeof<'outType>
        let retype (x: 'b) : Option<'outType> = x |> box |> unbox<'outType> |> Some

        let inline shapeArray (arr: 'a option[]) =
            if t.IsArray then
                let elem = t.GetElementType()
                if isOption elem
                then
                    let inner = elem.GetGenericArguments().[0]
                    if inner = typeof<'a> then arr |> retype
                    else None
                else
                    if Array.exists Option.isNone arr then None
                    else arr |> Array.map Option.get |> retype

            elif isList t then
                let elem = t.GetGenericArguments().[0]
                if isOption elem then
                    let inner = elem.GetGenericArguments().[0]
                    if inner = typeof<'a> then arr |> Array.toList |> retype
                    else None
                else
                    if Array.exists Option.isNone arr then None
                    else arr |> Array.map Option.get |> Array.toList |> retype

            else
                // Scalar value requested.
                match arr, isOption t with
                | [| v |], true -> v |> retype
                | [| Some v |], false -> v |> retype
                | _ -> None

        let inline shapeMatrix (mat: 'a option[,]) =
            if t.IsArray && t.GetArrayRank() = 2 then
                let elem = t.GetElementType()
                if isOption elem then mat |> retype
                else
                    let hasNA = mat |> Seq.cast<option<_>> |> Seq.exists Option.isNone
                    if hasNA then None
                    else
                        let rows = mat.GetLength 0
                        let cols = mat.GetLength 1
                        let m2 = Array2D.init rows cols (fun i j -> Option.get mat.[i,j])
                        m2 |> retype
            else None

        match extractedVal with
        | NoConverter -> None
        | Int xs -> shapeArray xs
        | Float xs -> shapeArray xs
        | Bool xs -> shapeArray xs
        | String xs -> shapeArray xs
        | Complex xs -> shapeArray xs
        | Date xs -> shapeArray xs
        | DateTime xs -> shapeArray xs
        | IntMatrix m -> shapeMatrix m
        | FloatMatrix m -> shapeMatrix m
        | BoolMatrix m -> shapeMatrix m
        | StringMatrix m -> shapeMatrix m
        | Empty ->
            if t.IsArray then
                let elem = t.GetElementType()
                let empty = System.Array.CreateInstance(elem, 0)
                empty |> retype

            elif isList t then
                let elem = t.GetGenericArguments().[0]
                let empty =
                    let listType = typedefof<list<_>>.MakeGenericType(elem)
                    System.Activator.CreateInstance(listType, true)
                empty |> retype

            else
                None

    /// Default conversions from R to .NET primitive types or
    /// simple .NET representations provided by RBridge (for example,
    /// for RComplex). The values are extracted into .NET memory space.
    /// If 'outType is obj, then returns an array-based representation.
    let tryFromRStructural<'outType> (engine: NativeApi.RunningEngine) (sexp: SymbolicExpression) : 'outType option =
        if typeof<'outType> = typeof<obj>
        then extractFromR engine sexp |> extractedToObj |> unbox
        else extractFromR engine sexp |> reshape<'outType>

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

    let toR (eng: NativeApi.RunningEngine) (value: obj) : SymbolicExpression =
        match value with

        // Pass-through of basic R expression values:
        | :? SymbolicExpression as s -> s
        | :? RProvider.Abstractions.RExpr as s -> RExprWrapper.toRBridge s
        | :? RBridge.Extensions.REnvironment as s -> s.AsSymbolicExpression

        // .NET primitives:
        | null -> { ptr = eng.Api.nilValue }
        | :? string as s -> Create.stringVector eng [| Some s |]
        | :? array<string> as xs -> Create.stringVector eng (Array.map Some xs)
        | :? list<string> as xs -> Create.stringVector eng (List.map Some xs)
        | :? array<string option> as xs -> Create.stringVector eng xs
        | :? list<string option> as xs -> Create.stringVector eng xs
        | :? int as i -> Create.intVector eng [| Some i |]
        | :? array<int> as xs -> Create.intVector eng (Array.map Some xs)
        | :? list<int> as xs -> Create.intVector eng (List.map Some xs)
        | :? array<int option> as xs -> Create.intVector eng xs
        | :? list<int option> as xs -> Create.intVector eng xs
        | :? float as f -> Create.realVector eng [| Some f |]
        | :? array<float> as xs -> Create.realVector eng (Array.map Some xs)
        | :? list<float> as xs -> Create.realVector eng (List.map Some xs)
        | :? array<float option> as xs -> Create.realVector eng xs
        | :? list<float option> as xs -> Create.realVector eng xs
        | :? bool as b -> Create.logicalVector eng [| Some b |]
        | :? array<bool> as xs -> Create.logicalVector eng (Array.map Some xs)
        | :? list<bool> as xs -> Create.logicalVector eng (List.map Some xs)
        | :? array<bool option> as xs -> Create.logicalVector eng xs
        | :? list<bool option> as xs -> Create.logicalVector eng xs
        | :? System.DateOnly as d -> Create.dateVector eng [| d |> RDate.fromDateOnly |> Some |]
        | :? array<System.DateOnly> as d -> Create.dateVector eng (Array.map(RDate.fromDateOnly >> Some) d)
        | :? list<System.DateOnly> as d -> Create.dateVector eng (List.map(RDate.fromDateOnly >> Some) d)
        | :? array<System.DateOnly option> as d -> Create.dateVector eng (Array.map(Option.map RDate.fromDateOnly) d)
        | :? list<System.DateOnly option> as d -> Create.dateVector eng (List.map(Option.map RDate.fromDateOnly) d)
        | :? System.DateTime as d -> Create.dateTimeVector eng [ d |> RDateTime.fromDateTime |> Some ]
        | :? array<System.DateTime> as d -> Create.dateTimeVector eng (Array.map(RDateTime.fromDateTime >> Some) d)
        | :? list<System.DateTime> as d -> Create.dateTimeVector eng (List.map(RDateTime.fromDateTime >> Some) d)
        | :? array<System.DateTime option> as d -> Create.dateTimeVector eng (Array.map(Option.map RDateTime.fromDateTime) d)
        | :? list<System.DateTime option> as d -> Create.dateTimeVector eng (List.map(Option.map RDateTime.fromDateTime) d)

        // R structural types:
        | :? RComplex as c -> Create.complexVector eng [| Some c |]
        | :? array<RComplex> as cs -> Create.complexVector eng (Array.map Some cs)
        | :? list<RComplex> as cs -> Create.complexVector eng (List.map Some cs)
        | :? array<RComplex option> as cs -> Create.complexVector eng cs
        | :? list<RComplex option> as cs -> Create.complexVector eng cs
        | :? RDate as d -> Create.dateVector eng [| Some d |]
        | :? array<RDate> as d -> Create.dateVector eng (Array.map Some d)
        | :? list<RDate> as d -> Create.dateVector eng (List.map Some d)
        | :? array<RDate option> as d -> Create.dateVector eng d
        | :? list<RDate option> as d -> Create.dateVector eng d
        | :? RDateTime as d -> Create.dateTimeVector eng [ Some d ]
        | :? array<RDateTime> as d -> Create.dateTimeVector eng (Array.map Some d)
        | :? list<RDateTime> as d -> Create.dateTimeVector eng (List.map Some d)
        | :? array<RDateTime option> as d -> Create.dateTimeVector eng d
        | :? list<RDateTime option> as d -> Create.dateTimeVector eng d

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
