namespace RProvider.Runtime

open RBridge
open RBridge.Extensions

/// Convert between user-facing RExpr and the internal
/// RBridge symbolic expression type.
module internal RExprWrapper =

    open RProvider.Abstractions

    let toRBridge (ex: RExpr) : RBridge.SymbolicExpression = { ptr = (RExpr.unwrap ex).ptr }

    let toRProvider (ex: RBridge.SymbolicExpression) : RExpr = RExpr.wrap { ptr = ex.ptr }

/// Types representing common R types, and
/// functions for working with them.
module RTypes =

    /// Represents user-facing types of expressions in R.
    /// Expressions are labelled with these types, for example
    /// in FSI output.
    type RSemanticType =
        | ScalarType
        | VectorType
        | ListType
        | FactorType
        | MatrixType
        | ArrayType
        | DataFrameType
        | FunctionType
        | EnvironmentType
        | S3ObjectType
        | S4ObjectType
        | R6ObjectType

    let private scalarOrVector engine sexp =
        match SymbolicExpression.length engine sexp with
        | 1 -> ScalarType
        | _ -> VectorType

    /// Classify a symbolic expression into one of the semantic
    /// types provided by RProvider.
    let classify engine sexp =
        match sexp with
        | ActivePatterns.S4Object engine _ -> S4ObjectType
        | ActivePatterns.DataFrame engine _ -> DataFrameType
        | ActivePatterns.Factor engine _ -> FactorType
        | _ when Extract.getDimension engine sexp = 2 -> MatrixType
        | _ when Extract.getDimension engine sexp > 2 -> ArrayType
        | ActivePatterns.S3Object engine _ -> S3ObjectType
        | ActivePatterns.RealVector engine _
        | ActivePatterns.ComplexVector engine _
        | ActivePatterns.IntegerVector engine _
        | ActivePatterns.LogicalVector engine _
        | ActivePatterns.CharacterVector engine _
        | ActivePatterns.RawVector engine _ -> scalarOrVector engine sexp
        | ActivePatterns.List engine _ -> ListType
        | ActivePatterns.Function engine _ -> FunctionType
        | ActivePatterns.Environment engine _ -> EnvironmentType
        | _ ->
            RProvider.Common.LogFile.logf "No typed conversion was possible for sexp: %A" (SymbolicExpression.getType engine sexp)
            failwith "Could not classify expression as a semantic type."


    /// Functions for accessing R functions within typed
    /// R wrappers.
    module private R =

        let passThrough _ (v: obj) = v :?> SymbolicExpression

        let baseOp (fn: string) (a: SymbolicExpression) tryMake =
            let rEnv = REnvironment.globalEnv Singletons.engine.Value
            let sexp = Call.callFuncByName passThrough rEnv "base" fn Seq.empty [| a |]

            match sexp with
            | Ok sexp -> tryMake sexp |> Option.get
            | Error e -> failwith e

        let baseOp2 (fn: string) (a: SymbolicExpression) (b: SymbolicExpression) tryMake =
            let rEnv = REnvironment.globalEnv Singletons.engine.Value
            let sexp = Call.callFuncByName passThrough rEnv "base" fn Seq.empty [| a; b |]

            match sexp with
            | Ok sexp -> tryMake sexp |> Option.get
            | Error e -> failwith e

    module VectorBase =

        type RVectorBase<'T> =
            internal { Sexp: SymbolicExpression }

            /// If a vector is a named vector, returns a list of the names
            /// associated with the vector. Otherwise, returns a blank array.
            member this.Names = Vector.tryNames Singletons.engine.Value this.Sexp |> Option.defaultValue [||]

            /// Get a scalar item by R index (1..n based, not zero based).
            member this.Item(i: int, mk) : 'T =
                let idxSexp = Create.intVector Singletons.engine.Value [| i + 1 |]
                R.baseOp2 "[[" this.Sexp idxSexp mk

            /// If a vector is named, get the named item from the vector.
            member this.Item(name: string, mk) : 'T =
                let idx = this.Names |> Array.findIndex ((=) name)
                this.[idx, mk]

            member this.AsRExpr = this.Sexp |> RExprWrapper.toRProvider

    /// Types for expressing real numbers that are within
    /// an R environment. Operations are zero-copy within R.
    module Real =

        module Scalar =

            /// A scalar value currently residing in R's memory space.
            type RRealScalar<[<Measure>] 'u> = internal { Sexp: SymbolicExpression }

            let tryFromExpression (sexp: SymbolicExpression) =
                match classify Singletons.engine.Value sexp with
                | ScalarType ->
                    if SymbolicExpression.length Singletons.engine.Value sexp = 1 then Some { Sexp = sexp } else None
                | _ -> None

            /// Enforces that a scalar is a vector of a single element,
            /// to be used before any operation.
            let internal enforceShape (num: RRealScalar<'u>) =
                if SymbolicExpression.length Singletons.engine.Value num.Sexp = 1 then
                    num
                else
                    failwith "A scalar R value was mutated and is no longer scalar."

            let extractScalarFloat (scalar: RRealScalar<'u>) =
                enforceShape scalar
                |> fun s -> s.Sexp
                |> Extract.extractFloatArray Singletons.engine.Value
                |> Array.head
                |> (*) (LanguagePrimitives.FloatWithMeasure<'u> 1.)

            let fromFloat (value: float<'u>) : RRealScalar<'u> option =
                Create.realVector Singletons.engine.Value [| value |] |> tryFromExpression

            type RRealScalar<'u> with
                static member Add (a: RRealScalar<'u>) (b: RRealScalar<'u>) : RRealScalar<'u> =
                    R.baseOp2 "+" a.Sexp b.Sexp tryFromExpression

                static member Sub (a: RRealScalar<'u>) (b: RRealScalar<'u>) : RRealScalar<'u> =
                    R.baseOp2 "-" a.Sexp b.Sexp tryFromExpression

                static member Mul (a: RRealScalar<'u>) (b: RRealScalar<'v>) : RRealScalar<'u 'v> =
                    R.baseOp2 "*" a.Sexp b.Sexp tryFromExpression

                static member Div (a: RRealScalar<'u>) (b: RRealScalar<'v>) : RRealScalar<'u / 'v> =
                    R.baseOp2 "/" a.Sexp b.Sexp tryFromExpression

                static member Log a = R.baseOp "log" a.Sexp tryFromExpression
                static member Exp a = R.baseOp "exp" a.Sexp tryFromExpression

                static member Scale (a: RRealScalar<'u>) (s: RRealScalar<1>) : RRealScalar<'u> =
                    R.baseOp2 "*" a.Sexp s.Sexp tryFromExpression

                static member ToFloat a = extractScalarFloat a
                static member FromFloat f = fromFloat f

                static member (+)(a, b) = RRealScalar.Add a b
                static member (-)(a, b) = RRealScalar.Sub a b
                static member (*)(a, b) = RRealScalar.Mul a b
                static member (/)(a, b) = RRealScalar.Div a b

                member this.AsRExpr = this.Sexp |> RExprWrapper.toRProvider


        module Vector =

            type RRealVector<[<Measure>] 'u> = { Inner: VectorBase.RVectorBase<Scalar.RRealScalar<'u>> }

            let tryFromExpression sexp = { Inner = { Sexp = sexp } } |> Some

            type RRealVector<'u> with
                static member Add (a: RRealVector<'u>) (b: RRealVector<'u>) : RRealVector<'u> =
                    R.baseOp2 "+" a.Inner.Sexp b.Inner.Sexp tryFromExpression

                static member Mean(a: RRealVector<'u>) = R.baseOp "mean" a.Inner.Sexp Scalar.tryFromExpression
                static member (+)(a, b) = RRealVector.Add a b
                member this.Item(i: int) = this.Inner.[i, Scalar.tryFromExpression]
                member this.Item(name: string) = this.Inner.[name, Scalar.tryFromExpression]
                member this.Length = R.baseOp "length" this.Inner.Sexp Scalar.tryFromExpression


    type RVector<[<Measure>] 'u> =
        | NumericV of Real.Vector.RRealVector<'u>
        | IntegerV of VectorBase.RVectorBase<int>
        | LogicalV of VectorBase.RVectorBase<bool>
        | CharacterV of VectorBase.RVectorBase<string>
        | ComplexV of VectorBase.RVectorBase<Extensions.RComplex>
        | RawV of VectorBase.RVectorBase<byte>

    with
        member this.AsReal() =
            match this with
            | NumericV s -> s
            | _ -> failwith "Expression was not a vector of real numbers"

        member internal this.Sexp =
            match this with
            | NumericV v -> v.Inner.AsRExpr |> RExprWrapper.toRBridge
            | IntegerV v -> v.AsRExpr |> RExprWrapper.toRBridge
            | LogicalV v -> v.AsRExpr |> RExprWrapper.toRBridge
            | CharacterV v -> v.AsRExpr |> RExprWrapper.toRBridge
            | ComplexV v -> v.AsRExpr |> RExprWrapper.toRBridge
            | RawV v -> v.AsRExpr |> RExprWrapper.toRBridge


    module GenericVector =

        let tryFromExpression sexp =
            match sexp with
            | ActivePatterns.RealVector Singletons.engine.Value v -> Real.Vector.tryFromExpression v |> Option.map NumericV
            | _ ->
                None

    /// Represents R lists, which may contain elements of any type.
    module HeterogeneousList =

        /// An R heterogeneous list.
        type HList = private { sexp: SymbolicExpression }

        let tryFromExpression sexp =
            match classify Singletons.engine.Value sexp with
            | ListType -> Some { sexp = sexp }
            | _ -> None

        type HList with
            member internal this.Sexp = this.sexp
            member internal this.AsRExpr = this.sexp |> RExprWrapper.toRProvider
            
            member this.Item(i: int) =
                SymbolicExpression.getVectorElement Singletons.engine.Value this.sexp i
                |> RExprWrapper.toRProvider

            member this.Item(name: string) =
                SymbolicExpression.getListItemByName Singletons.engine.Value name this.sexp
                |> RExprWrapper.toRProvider

            member this.Length =
                SymbolicExpression.length Singletons.engine.Value this.sexp


    module Factor =

        type RFactor =
            internal { Sexp: SymbolicExpression }
            member this.AsRExpr = this.Sexp |> RExprWrapper.toRProvider
            member this.Levels = lazy (Factor.trylevels Singletons.engine.Value this.Sexp)

            member this.Indices = lazy (Extract.extractIntArray Singletons.engine.Value this.Sexp)

            member this.AsStringVector =
                lazy
                    (this.Levels.Value
                    |> Option.defaultWith (fun _ -> failwith "Could not get levels for factor")
                    |> fun levels ->
                        this.Indices.Value |> Array.map (fun i -> levels.[i - 1]))

        let tryFromExpression expr : RFactor option =
            match expr with
            | ActivePatterns.Factor Singletons.engine.Value e -> Some { Sexp = e }
            | _ -> None


    /// Type wrapper for R data.frame instances.
    module DataFrame =

        module Column =

            type Column =
                | NumericColumn of Real.Vector.RRealVector<1>
                | IntegerColumn of RProvider.Abstractions.RExpr
                | LogicalColumn of RProvider.Abstractions.RExpr
                | FactorColumn of Factor.RFactor
                | CharacterColumn of RProvider.Abstractions.RExpr
                | ListColumn of HeterogeneousList.HList
                | MatrixColumn of RProvider.Abstractions.RExpr

            let tryFromExpression sexp =
                match classify Singletons.engine.Value sexp with
                | VectorType ->
                    match sexp with
                    | ActivePatterns.RealVector Singletons.engine.Value v -> Real.Vector.tryFromExpression v |> Option.map NumericColumn
                    | ActivePatterns.IntegerVector Singletons.engine.Value v -> IntegerColumn (RExprWrapper.toRProvider sexp) |> Some 
                    | ActivePatterns.LogicalVector Singletons.engine.Value v -> LogicalColumn (RExprWrapper.toRProvider sexp) |> Some 
                    | ActivePatterns.ComplexVector Singletons.engine.Value v -> None
                    | ActivePatterns.CharacterVector Singletons.engine.Value v -> CharacterColumn (RExprWrapper.toRProvider sexp) |> Some 
                    | ActivePatterns.RawVector Singletons.engine.Value v -> None
                    | _ -> None
                | FactorType -> Factor.tryFromExpression sexp |> Option.map FactorColumn
                | MatrixType -> MatrixColumn (RExprWrapper.toRProvider sexp) |> Some
                | _ -> None
            
            let getSexp = function
                | NumericColumn c -> c.Inner.Sexp
                | MatrixColumn c
                | IntegerColumn c
                | LogicalColumn c
                | CharacterColumn c -> c |> RExprWrapper.toRBridge
                | ListColumn c -> c.Sexp
                | FactorColumn c -> c.AsRExpr |> RExprWrapper.toRBridge

        type RFrame = internal { Sexp: SymbolicExpression }

        let tryCreate (cols: (string * SymbolicExpression) array) =
            failwith "Creation of semantic types currently not implemented"

        /// Get row names of an R data frame. R stores row names in three
        /// ways: as sequential numbers, as character strings, and as
        /// a 'compact' encoding. All cases are extracted to a string array
        /// by this function.
        let rowNames df =
            match SymbolicExpression.tryGetAttribute df.Sexp "row.names" Singletons.engine.Value with
            | Some rn ->
                match rn with
                | ActivePatterns.CharacterVector Singletons.engine.Value _ ->
                    Extract.extractStringArray Singletons.engine.Value rn
                | ActivePatterns.IntegerVector Singletons.engine.Value _ ->
                    let ints = Extract.extractIntArray Singletons.engine.Value rn
                    if ints.Length = 2
                    then 
                        if ints.[0] = -2147483648 && ints.[1] < 0
                        then
                            let n = -ints.[1]
                            [| 1 .. n |]
                        else ints
                    else ints
                    |> Array.map(sprintf "%i")
                | _ -> Array.empty
            | None -> Array.empty

        let rowCount df =
            rowNames df |> Seq.length

        let tryFromExpression (sexp: SymbolicExpression) =
            match SymbolicExpression.getClasses Singletons.engine.Value sexp with
            | ls when ls |> List.contains "data.frame" -> Some { Sexp = sexp }
            | _ -> None

        let getColumnNames frame =
            SymbolicExpression.tryGetAttribute frame.Sexp "names" Singletons.engine.Value
            |> Option.map (RBridge.Extensions.Promise.force Singletons.engine.Value)
            |> Option.map (Extract.extractStringArray Singletons.engine.Value)
            |> Option.defaultValue [||]

        let getColumn name frame =
            let names = getColumnNames frame
            let colIndex =
                names
                |> Array.tryFindIndex ((=) name)
                |> Option.defaultWith (fun () ->
                    failwithf "Column '%s' not found in R data.frame" name)
            let colSexp = SymbolicExpression.getVectorElement Singletons.engine.Value frame.Sexp colIndex
            colSexp
            |> Column.tryFromExpression
            |> Option.defaultWith(fun _ -> failwithf "Column %s could not be coerced into a vector" name)


        type RFrame with
            static member GetColumnNames df = getColumnNames df
            static member GetColumn(df, name) = getColumn name df
            static member RowCount df = rowCount df

            member this.AsRExpr = this.Sexp |> RExprWrapper.toRProvider
            member this.Column name = RFrame.GetColumn(this, name)
            member this.Names = RFrame.GetColumnNames this
            member this.RowNames = rowNames this


    type RScalar<[<Measure>] 'u> =
        | NumericS of Real.Scalar.RRealScalar<'u>
        | IntegerS of RProvider.Abstractions.RExpr
        | LogicalS of RProvider.Abstractions.RExpr
        | CharacterS of RProvider.Abstractions.RExpr
        | ComplexS of RProvider.Abstractions.RExpr
        | RawS of RProvider.Abstractions.RExpr

    with
        member this.AsReal() =
            match this with
            | NumericS s -> s
            | _ -> failwith "Expression was not a real number"

        member internal this.Sexp =
            match this with
            | NumericS s -> s.Sexp
            | IntegerS s
            | LogicalS s
            | CharacterS s
            | ComplexS s
            | RawS s -> s |> RExprWrapper.toRBridge


    module GenericScalar =

        let tryFromExpression sexp =
            match sexp with
            | ActivePatterns.RealVector Singletons.engine.Value v ->
                Real.Scalar.tryFromExpression v |> Option.map NumericS
            | _ -> failwith "not implemented (Scalar)"


    /// A wrapped representation of an R value, which
    /// remains in R. It is not within .NET memory space.
    type RSemantic<[<Measure>] 'u> =
        | ScalarInR of RScalar<'u>
        | VectorInR of RVector<'u>
        | DataFrameInR of DataFrame.RFrame
        | FactorInR of Factor.RFactor
        | ListInR of HeterogeneousList.HList

    with
        member internal this.Sexp =
            match this with
            | ScalarInR s -> s.Sexp
            | VectorInR v -> v.Sexp
            | DataFrameInR df -> df.Sexp
            | FactorInR f -> f.Sexp
            | ListInR l -> l.Sexp