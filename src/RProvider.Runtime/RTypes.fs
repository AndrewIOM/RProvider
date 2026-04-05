namespace RProvider.Runtime

open RBridge
open RBridge.Extensions
open RProvider.Internal.RInit

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
            tryMake sexp |> Option.get

        let baseOp2 (fn: string) (a: SymbolicExpression) (b: SymbolicExpression) tryMake =
            let rEnv = REnvironment.globalEnv Singletons.engine.Value
            let sexp = Call.callFuncByName passThrough rEnv "base" fn Seq.empty [| a; b |]
            tryMake sexp |> Option.get


    /// A zero-copy representation of a scalar value in R.
    module Scalar =

        /// A scalar value currently residing in R's memory space.
        type RNumericScalar = private { Sexp : SymbolicExpression }

        let tryNumeric (sexp: SymbolicExpression) =
            match SymbolicExpression.getType Singletons.engine.Value sexp with
            | SymbolicExpression.RealVector
            | SymbolicExpression.IntegerVector ->
                if SymbolicExpression.length Singletons.engine.Value sexp = 1 then
                    Some { Sexp = sexp }
                else None
            | _ -> None

        /// Enforces that a scalar is a vector of a single element,
        /// to be used before any operation.
        let internal enforceShape (num:RNumericScalar) =
            if SymbolicExpression.length Singletons.engine.Value num.Sexp = 1
            then num
            else failwith "A scalar R value was mutated and is no longer scalar."

        let extractScalarFloat (scalar: RNumericScalar) =
            enforceShape scalar
            |> fun s -> s.Sexp
            |> Extract.extractFloatArray Singletons.engine.Value
            |> Array.head

        let fromFloat (value: float) =
            Create.realVector Singletons.engine.Value [| value |]
            |> tryNumeric

        type RNumericScalar with
            static member Add a b = R.baseOp2 "+" a.Sexp b.Sexp
            static member Sub a b = R.baseOp2 "-" a.Sexp b.Sexp
            static member Mul a b = R.baseOp2 "*" a.Sexp b.Sexp
            static member Div a b = R.baseOp2 "/" a.Sexp b.Sexp
            static member Log a = R.baseOp "log" a.Sexp
            static member Exp a = R.baseOp "exp" a.Sexp
            static member Scale a s = R.baseOp2 "*" a.Sexp s.Sexp
            static member ToFloat a = extractScalarFloat a
            static member FromFloat f = fromFloat f
            member this.RExpr = this.Sexp

            static member (+) (a,b) = RNumericScalar.Add a b
            static member (-) (a,b) = RNumericScalar.Sub a b
            static member (*) (a,b) = RNumericScalar.Mul a b
            static member (/) (a,b) = RNumericScalar.Div a b


    module Vector =

        type RVectorInner<'T> = { Sexp : SymbolicExpression }

        type RVector =
            | NumericV of RVectorInner<float>
            | IntegerV of RVectorInner<int>
            | LogicalV of RVectorInner<bool>
            | CharacterV of RVectorInner<string>
            | ComplexV of RVectorInner<Extensions.RComplex>
            | RawV of RVectorInner<byte>
        
        with
            member internal this.Sexp =
                match this with
                | NumericV inn -> inn.Sexp
                | IntegerV inn -> inn.Sexp
                | LogicalV inn -> inn.Sexp
                | CharacterV inn -> inn.Sexp
                | ComplexV inn -> inn.Sexp
                | RawV inn -> inn.Sexp

            /// If a vector is a named vector, returns a list of the names
            /// associated with the vector. Otherwise, returns a blank array.
            member this.Names =
                Vector.tryNames Singletons.engine.Value this.Sexp
                |> Option.defaultValue [||]

            /// Get a scalar item by R index (1..n based, not zero based).
            member this.Item(i:int) : 'T =
                let idxSexp = Create.intVector Singletons.engine.Value [| i + 1 |]
                let elem mk = R.baseOp2 "[[" this.Sexp idxSexp mk
                match this with
                | NumericV _ -> elem Scalar.tryNumeric
                | CharacterV _ -> failwith "not finished"

            /// If a vector is named, get the named item from the vector.
            member this.Item(name : string) : 'T =
                let idx = this.Names |> Array.findIndex ((=) name)
                this.[idx]

        let tryCreate sexp =
            match sexp with
            | ActivePatterns.RealVector Singletons.engine.Value s -> NumericV { Sexp = s } |> Ok
            | ActivePatterns.ComplexVector Singletons.engine.Value s -> ComplexV { Sexp = s } |> Ok
            | ActivePatterns.IntegerVector Singletons.engine.Value s -> IntegerV { Sexp = s } |> Ok
            | ActivePatterns.LogicalVector Singletons.engine.Value s -> LogicalV { Sexp = s } |> Ok
            | ActivePatterns.CharacterVector Singletons.engine.Value s -> CharacterV { Sexp = s } |> Ok
            | ActivePatterns.RawVector Singletons.engine.Value s -> RawV { Sexp = s } |> Ok
            | _ -> Error "Could not make vector. R expression was not a vector."


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

    /// A wrapped representation of an R value, which
    /// remains in R. It is not within .NET memory space.
    type RSemantic =
        | ScalarInR of Scalar.RNumericScalar
        | VectorInR of Vector.RVector
        | DataFrameInR of DataFrame.RFrame
        | FactorInR of Factor.RFactor