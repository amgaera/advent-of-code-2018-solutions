module Solution

open System


let private getBoxIdDifference firstBoxId secondBoxId =
    Seq.zip firstBoxId secondBoxId
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.length

let private getBoxIdsCommonLetters firstBoxId secondBoxId =
    let commonLetters =
        Seq.zip firstBoxId secondBoxId
        |> Seq.filter (fun (a, b) -> a = b)
        |> Seq.map fst
        |> Seq.toArray
    new String (commonLetters)

let getCorrectBoxIdsCommonLetters boxIds =
    let cachedBoxIds = Seq.cache boxIds
    Seq.allPairs cachedBoxIds cachedBoxIds
    |> Seq.filter (fun (firstBoxId, secondBoxId) -> getBoxIdDifference firstBoxId secondBoxId = 1)
    |> Seq.head
    ||> getBoxIdsCommonLetters