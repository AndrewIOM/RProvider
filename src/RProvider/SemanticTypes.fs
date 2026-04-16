namespace RProvider.Runtime

open RBridge
open RBridge.Extensions

/// Types representing common R types, and
/// functions for working with them.
/// Includes FsSci shapes for interop with other libraries.
module RTypes =

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

    /// A base 
    module VectorBase =

        type RVectorBase<'T> = { Sexp : SymbolicExpression }
        with

            /// If a vector is a named vector, returns a list of the names
            /// associated with the vector. Otherwise, returns a blank array.
            member this.Names =
                Vector.tryNames Singletons.engine.Value this.Sexp
                |> Option.defaultValue [||]

            /// Get a scalar item by R index (1..n based, not zero based).
            member this.Item(i:int, mk) : 'T =
                let idxSexp = Create.intVector Singletons.engine.Value [| i + 1 |]
                R.baseOp2 "[[" this.Sexp idxSexp mk

            /// If a vector is named, get the named item from the vector.
            member this.Item(name : string, mk) : 'T =
                let idx = this.Names |> Array.findIndex ((=) name)
                this.[idx, mk]

    /// Types for expressing floating point numbers that are within
    /// an R environment. Operations are zero-copy within R.
    module Real =

        module Scalar =

            /// A scalar value currently residing in R's memory space.
            type RRealScalar<[<Measure>] 'u> = private { Sexp : SymbolicExpression }

            let tryFromExpression (sexp: SymbolicExpression) =
                match SymbolicExpression.getType Singletons.engine.Value sexp with
                | SymbolicExpression.RealVector ->
                    if SymbolicExpression.length Singletons.engine.Value sexp = 1 then
                        Some { Sexp = sexp }
                    else None
                | _ -> None

            /// Enforces that a scalar is a vector of a single element,
            /// to be used before any operation.
            let internal enforceShape (num:RRealScalar<'u>) =
                if SymbolicExpression.length Singletons.engine.Value num.Sexp = 1
                then num
                else failwith "A scalar R value was mutated and is no longer scalar."

            let extractScalarFloat (scalar: RRealScalar<'u>) =
                enforceShape scalar
                |> fun s -> s.Sexp
                |> Extract.extractFloatArray Singletons.engine.Value
                |> Array.head
                |> (*) (LanguagePrimitives.FloatWithMeasure<'u> 1.)

            let fromFloat (value: float<'u>) : RRealScalar<'u> option =
                Create.realVector Singletons.engine.Value [| value |]
                |> tryFromExpression

            type RRealScalar<'u> with
                static member Add (a: RRealScalar<'u>) (b: RRealScalar<'u>) : RRealScalar<'u> = R.baseOp2 "+" a.Sexp b.Sexp tryFromExpression
                static member Sub (a: RRealScalar<'u>) (b: RRealScalar<'u>) : RRealScalar<'u> = R.baseOp2 "-" a.Sexp b.Sexp tryFromExpression
                static member Mul (a: RRealScalar<'u>) (b: RRealScalar<'v>) : RRealScalar<'u 'v> = R.baseOp2 "*" a.Sexp b.Sexp tryFromExpression
                static member Div (a: RRealScalar<'u>) (b: RRealScalar<'v>) : RRealScalar<'u/'v> = R.baseOp2 "/" a.Sexp b.Sexp tryFromExpression
                static member Log a = R.baseOp "log" a.Sexp tryFromExpression
                static member Exp a = R.baseOp "exp" a.Sexp tryFromExpression
                static member Scale (a: RRealScalar<'u>) (s: RRealScalar<1>) : RRealScalar<'u> = R.baseOp2 "*" a.Sexp s.Sexp tryFromExpression
                static member ToFloat a = extractScalarFloat a
                static member FromFloat f = fromFloat f
                member this.RExpr = this.Sexp

                static member (+) (a,b) = RRealScalar.Add a b
                static member (-) (a,b) = RRealScalar.Sub a b
                static member (*) (a,b) = RRealScalar.Mul a b
                static member (/) (a,b) = RRealScalar.Div a b


        module Vector =

            type RRealVector<[<Measure>] 'u> = { Inner: VectorBase.RVectorBase<Scalar.RRealScalar<'u>> }

            let tryCreate sexp =
                { Inner = { Sexp = sexp } } |> Some
            
            type RRealVector<'u> with
                static member Add (a: RRealVector<'u>) (b: RRealVector<'u>) : RRealVector<'u> = R.baseOp2 "+" a.Inner.Sexp b.Inner.Sexp tryCreate
                static member Mean(a: RRealVector<'u>) = R.baseOp "mean" a.Inner.Sexp Scalar.tryFromExpression
                static member (+) (a,b) = RRealVector.Add a b
                member this.Item(i: int) = this.Inner.[i, Scalar.tryFromExpression]
                member this.Item(name: string) = this.Inner.[name, Scalar.tryFromExpression]


    /// Type wrapper for R data.frame instances.
    module DataFrame =

        type RFrame = private { Sexp : SymbolicExpression }
        with
            member this.RExp = this.Sexp


        let createFrame (cols: (string * SymbolicExpression) array) =
            failwith "TODO"
            // let sexps = cols |> Array.map snd
            // let names = cols |> Array.map fst
            // let vec = Create.list engine sexps
            // Constructors.setAttribute engine vec "names" (Constructors.createStringVector engine names)
            // Constructors.setAttribute engine vec "class" (Constructors.createStringVector engine [| "data.frame" |])
            // { Sexp = vec }
            // // let names = Extract.getNames eng sexp
            // // let cols =
            // //     names
            // //     |> Array.map (fun n -> n, Extract.getElement eng sexp n)
            // //     |> Map.ofArray
            // // { Engine = eng; Sexp = sexp; ColumnNames = names; Columns = cols }
            // // |> unbox<'T>
            // // data frame = list + class="data.frame" + row.names + names
            // // let cols =
            // //     df.ColumnNames
            // //     |> Array.map (fun name ->
            // //         let colSexp = toR eng (df.Columns.[name] :> obj)
            // //         name, colSexp)
            // // let list = Create.createList eng cols
            // // Attributes.setClass eng list [| "data.frame" |]
            // // Attributes.setNames eng list df.ColumnNames
            // // list

        let tryAsFrame (sexp: SymbolicExpression) =
            match SymbolicExpression.getClasses Singletons.engine.Value sexp with
            | ls when ls |> List.contains "data.frame" -> Some { Sexp = sexp }
            | _ -> None

        let getColumnNames engine frame =
            SymbolicExpression.tryGetAttribute frame.Sexp "names" engine
            |> Option.map (Extract.extractStringArray engine)
            |> Option.defaultValue [||]

        let getColumn name engine frame =
            failwith "not implemented"

    module Factor =

        type RFactor = { Sexp : SymbolicExpression }
        with
            member this.Levels =
                lazy (Factor.trylevels Singletons.engine.Value this.Sexp)

            member this.Indices =
                lazy (Extract.extractIntArray Singletons.engine.Value this.Sexp)

            member this.AsStrings =
                lazy (
                    let levels = this.Levels.Value
                    this.Indices.Value |> Array.map (fun i -> levels.[i - 1])
                )

        let tryFromExpr expr : RFactor option =
            match expr with
            | ActivePatterns.Factor Singletons.engine.Value e -> Some { Sexp = e }
            | _ -> None

    type RVector<[<Measure>] 'u> =
        | NumericV of Real.Vector.RRealVector<'u>
        | IntegerV of Real.Vector.RRealVector<'u> // <int>
        | LogicalV of Real.Vector.RRealVector<'u> // <bool>
        | CharacterV of Real.Vector.RRealVector<'u> // <string>
        | ComplexV of Real.Vector.RRealVector<'u> // <Extensions.RComplex>
        | RawV of Real.Vector.RRealVector<'u> // <byte>

    module GenericVector =

        let tryCreate sexp =
            match sexp with
            | ActivePatterns.RealVector Singletons.engine.Value v ->
                Real.Vector.tryCreate v
                |> Option.map NumericV
            | _ -> failwith "not implemented"


    type RScalar<[<Measure>] 'u> =
        | NumericS of Real.Scalar.RRealScalar<'u>
        // | IntegerV of VectorBase.RVectorBase<int>
        // | LogicalV of VectorBase.RVectorBase<bool>
        // | CharacterV of VectorBase.RVectorBase<string>
        // | ComplexV of VectorBase.RVectorBase<Extensions.RComplex>
        // | RawV of VectorBase.RVectorBase<byte>

    module GenericScalar =

        let tryCreate sexp =
            match sexp with
            | ActivePatterns.RealVector Singletons.engine.Value v ->
                Real.Scalar.tryFromExpression v
                |> Option.map NumericS
            | _ -> failwith "not implemented"


    /// A wrapped representation of an R value, which
    /// remains in R. It is not within .NET memory space.
    type RSemantic<[<Measure>] 'u> =
        | ScalarInR of RScalar<'u>
        | VectorInR of RVector<'u>
        | DataFrameInR of DataFrame.RFrame
        | FactorInR of Factor.RFactor

    module Semantic =

        let getExpr semantic =
            match semantic with
            | ScalarInR s ->
                match s with
                | NumericS s -> s.RExpr
