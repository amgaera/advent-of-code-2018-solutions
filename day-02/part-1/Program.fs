open System


[<EntryPoint>]
let main argv =
    let input = seq {
        while Console.In.Peek() >= 0 do
            yield Console.In.ReadLine()
    }
    let boxIds =
        input |> Seq.filter (fun line -> String.IsNullOrEmpty (line) |> not)

    let solution = Solution.getBoxIdListChecksum boxIds
    printfn "The checksum for the list of box IDs is '%d'" solution

    0 // return an integer exit code
