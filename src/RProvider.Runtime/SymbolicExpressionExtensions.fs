namespace RProvider

open RBridge
open RProvider.Internal.RInit
open RBridge.Extensions
open RBridge.Extensions.ActivePatterns

/// Contains functions to make working with SymbolicExpression
/// more idiomatic.
[<RequireQualifiedAccess>]
module SymbolicExpression =

    /// <summary> For an S4 object, get a dictionary containing first the
    /// slot name and second the slot's R type. If the expression
    /// is not an S4 object, returns `None`.</summary>
    /// <param name="expr">An R symbolic expression</param>
    /// <returns>A diictionary with key = slot name, and value = R type</returns>
    let trySlots (expr:SymbolicExpression) =
        match expr with
        | S4Object Singletons.engine.Value s4 ->
            S4.tryGetSlotTypes Singletons.engine.Value s4
        | _ -> None

    /// <summary> For an S4 object, get a dictionary containing first the
    /// slot name and second the slot's R type.</summary>
    /// <param name="expr">An R symbolic expression</param>
    /// <returns>A diictionary with key = slot name, and value = R type</returns>
    let slots (expr:SymbolicExpression) =
        match trySlots expr with
        | Some slots -> slots
        | None -> invalidOp "Can only get slots for an S4 object (R type)"

    /// <summary>Gets the value of a slot as a SymbolicExpression</summary>
    /// <param name="name">Slot name to retrieve</param>
    /// <param name="expr">An R symbolic expression</param>
    /// <returns>Some symbolic expression if the expression was an S4
    /// object and had the slot, or None otherwise.</returns>
    let trySlot name (expr:SymbolicExpression) = 
        match expr with
        | S4Object Singletons.engine.Value s4 ->
            S4.tryGetSlot Singletons.engine.Value s4 name
        | _ -> None

    /// <summary>Gets the value of a slot as a SymbolicExpression</summary>
    /// <param name="name">Slot name to retrieve</param>
    /// <param name="expr">An R symbolic expression</param>
    /// <returns>A symbolic expression containing the slot value</returns>
    let slot name (expr:SymbolicExpression) = 
        match trySlot name expr with
        | Some slot -> slot
        | None -> invalidOp "Can only get slot for an S4 object (R type)"

    /// <summary>Get the data from a column in an R dataframe
    /// by its name.</summary>
    /// <param name="name">The column name</param>
    /// <param name="expr">An R symbolic expression</param>
    /// <returns>A vector containing the data</returns>
    let column (name:string) (expr:SymbolicExpression) : SymbolicExpression =
        match expr with
        | DataFrame Singletons.engine.Value df ->
            Runtime.RTypes.DataFrame.tryAsFrame df Singletons.engine.Value
            |> Option.defaultWith (fun _ -> failwith "The expression was not a valid R data frame")
            |> Runtime.RTypes.DataFrame.getColumn name Singletons.engine.Value
        | _ -> invalidOp "The expression is not an R data frame."
