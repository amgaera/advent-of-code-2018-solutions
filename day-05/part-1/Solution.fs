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

let reducePolymer polymer =
    let reducedUnits =
        polymer
        |> Seq.fold foldPolymer []
        |> Seq.rev
        |> Seq.toArray

    new String(reducedUnits)
