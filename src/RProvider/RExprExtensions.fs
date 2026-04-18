namespace RProvider

open RProvider.Abstractions
open RProvider.Runtime

/// Functions for working with R expressions.
[<RequireQualifiedAccess>]
module RExpr =

    open RBridge

    /// <summary> For an S4 object, get a dictionary containing first the
    /// slot name and second the slot's R type. If the expression
    /// is not an S4 object, returns `None`.</summary>
    /// <param name="expr">An R symbolic expression</param>
    /// <returns>A diictionary with key = slot name, and value = R type</returns>
    let trySlots: RExpr -> Map<string,string> option = RExprWrapper.toRBridge >> SymbolicExpression.trySlots

    /// <summary> For an S4 object, get a dictionary containing first the
    /// slot name and second the slot's R type.</summary>
    /// <param name="expr">An R symbolic expression</param>
    /// <returns>A diictionary with key = slot name, and value = R type</returns>
    let slots: RExpr -> Map<string,string> = RExprWrapper.toRBridge >> SymbolicExpression.slots

    /// <summary>Gets the value of a slot as a SymbolicExpression</summary>
    /// <param name="name">Slot name to retrieve</param>
    /// <param name="expr">An R symbolic expression</param>
    /// <returns>Some symbolic expression if the expression was an S4
    /// object and had the slot, or None otherwise.</returns>
    let trySlot name expr = expr |> RExprWrapper.toRBridge |> SymbolicExpression.trySlot name

    /// <summary>Gets the value of a slot as a SymbolicExpression</summary>
    /// <param name="name">Slot name to retrieve</param>
    /// <param name="expr">An R symbolic expression</param>
    /// <returns>A symbolic expression containing the slot value</returns>
    let slot name expr = expr |> RExprWrapper.toRBridge |> SymbolicExpression.slot name

    /// <summary>Get the data from a column in an R dataframe
    /// by its name.</summary>
    /// <param name="name">The column name</param>
    /// <param name="expr">An R symbolic expression</param>
    /// <returns>A vector containing the data</returns>
    let column (name:string) expr =
        expr |> RExprWrapper.toRBridge |> SymbolicExpression.column name |> RExprWrapper.toRProvider

    /// Get the R classes associated with an R expression.
    let classes = RExprWrapper.toRBridge >> SymbolicExpression.rClass

    /// Pass a value from R memory space into .NET, represented
    /// as a .NET primitive or the closest approximation of the
    /// relevant R primitive.
    let tryGetValue<'a> = RExprWrapper.toRBridge >> SymbolicExpression.tryGetValue<'a>

    let getValue<'a> = RExprWrapper.toRBridge >> SymbolicExpression.getValue<'a>
    let tryGetTyped: RExpr -> Runtime.RTypes.RSemantic<1> option = RExprWrapper.toRBridge >> SymbolicExpression.tryGetTyped

    let getTyped: RExpr -> Runtime.RTypes.RSemantic<1> = RExprWrapper.toRBridge >> SymbolicExpression.getTyped

    let listItem name expr =
        expr |> RExprWrapper.toRBridge |> SymbolicExpression.listItem name |> RExprWrapper.toRProvider

    let getMember name expr =
        expr |> RExprWrapper.toRBridge |> SymbolicExpression.getMember name |> RExprWrapper.toRProvider

    let typedVectorByName (name:string) expr =
        expr |> RExprWrapper.toRBridge |> SymbolicExpression.typedVectorByName name

    let typedVectorByIndex (index:int) expr =
        expr |> RExprWrapper.toRBridge |> SymbolicExpression.typedVectorByIndex index

    let head: RExpr -> Runtime.RTypes.RScalar<1> = RExprWrapper.toRBridge >> SymbolicExpression.head
    let tryHead: RExpr -> Runtime.RTypes.RScalar<1> option = RExprWrapper.toRBridge >> SymbolicExpression.tryHead

    let printToString = RExprWrapper.toRBridge >> Runtime.Printing.printUsingTempFile


/// [omit]
/// Public API for accessing RExpr, including converting to
/// R semantic types, .NET types, and extracting key metadata.
[<AutoOpen>]
module RExprExtensions =
    
    type RExpr with
        
        member this.Class: string [] = SymbolicExpression.rClass (RExprWrapper.toRBridge this)
        
        member this.TryFromR<'a> () = SymbolicExpression.tryGetValue<'a> (RExprWrapper.toRBridge this)

        /// Extract the value from R memory space into .NET, with
        /// type 'a.
        member this.FromR<'a> () = SymbolicExpression.getValue<'a> (RExprWrapper.toRBridge this)

        /// Get the member symbolic expression of given name.
        member this.Member(name: string) = SymbolicExpression.getMember name (RExprWrapper.toRBridge this) |> RExprWrapper.toRProvider

        /// Get the value from the typed vector by name.
        member this.ValueOf (name: string) : Runtime.RTypes.RScalar<'u> = SymbolicExpression.typedVectorByName name (RExprWrapper.toRBridge this)

        /// Represents the R value in an appropriate semantic
        /// R type for further data exploration and analysis, without
        /// extraction from R memory.
        member this.TryAsRTyped = SymbolicExpression.tryGetTyped (RExprWrapper.toRBridge this)
        member this.AsTyped = SymbolicExpression.getTyped (RExprWrapper.toRBridge this)
        member this.AsDataFrame = Runtime.RTypes.DataFrame.tryAsFrame (RExprWrapper.toRBridge this)
        member this.AsVector = Runtime.RTypes.GenericVector.tryCreate (RExprWrapper.toRBridge this)
        member this.AsScalar = Runtime.RTypes.GenericScalar.tryCreate (RExprWrapper.toRBridge this)
        member this.AsFactor = Runtime.RTypes.Factor.tryFromExpr (RExprWrapper.toRBridge this)

        /// Get the value from an indexed vector by index.
        member this.ValueAt(index: int) : Runtime.RTypes.RScalar<'u> = SymbolicExpression.typedVectorByIndex index (RExprWrapper.toRBridge this)

        /// Get the first value of a vector.
        member this.First<'a>() = (RExprWrapper.toRBridge this).ValueAt<'a>(0)

        /// Try and get the first value of a vector, returning
        /// `None` if the `RExpr` is not a vector
        /// or an empty vector.
        member this.TryHead<'a>() = (RExprWrapper.toRBridge this).TryHead ()

        member this.Print () = this |> RExpr.printToString