module Solution

open System


let private doUnitsReact unitA unitB =
    unitA <> unitB && Char.ToUpperInvariant(unitA) = Char.ToUpperInvariant(unitB)

let private foldPolymer unitStack unit =
    match unitStack with
    | head :: tail ->
        if doUnitsReact unit head then
            tail
        else
            unit :: head :: tail
    | [] -> [ unit ]

let private removeUnitType unitType polymer =
    let remainingUnits =
        polymer
        |> Seq.filter (fun unit -> Char.ToUpperInvariant(unit) <> unitType)
        |> Seq.toArray

    new String(remainingUnits)

let private reducePolymer polymer =
    let reducedUnits =
        polymer
        |> Seq.fold foldPolymer []
        |> Seq.rev
        |> Seq.toArray

    new String(reducedUnits)

let findBestAugmentedPolymerReduction polymer =
    let unitTypes =
        polymer
        |> Seq.map (fun unit -> Char.ToUpperInvariant(unit))
        |> Set.ofSeq

    unitTypes
    |> Seq.map (fun unitType -> removeUnitType unitType polymer)
    |> Seq.map reducePolymer
    |> Seq.minBy (fun polymer -> polymer.Length)
