module Solution

open System


let private getBoxIdChecksumComponents boxId =
    let letterCounts =
        boxId
        |> Seq.countBy id
        |> Seq.map snd
        |> Set.ofSeq
    let twoCountComponent = if Set.contains 2 letterCounts then 1 else 0
    let threeCountComponent = if Set.contains 3 letterCounts then 1 else 0

    twoCountComponent, threeCountComponent


let getBoxIdListChecksum boxIds =
    let twoCountScore, threeCountScore =
        boxIds
        |> Seq.map getBoxIdChecksumComponents
        |> Seq.reduce (fun (a, b) (c, d) -> a + c, b + d)

    twoCountScore * threeCountScore