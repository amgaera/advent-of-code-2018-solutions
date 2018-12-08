open System


[<EntryPoint>]
let main argv =
    let input = seq {
        while Console.In.Peek() >= 0 do
            yield Console.In.ReadLine()
    }
    let boxIds =
        input |> Seq.filter (fun line -> String.IsNullOrEmpty (line) |> not)

    let solution = Solution.getCorrectBoxIdsCommonLetters boxIds
    printfn "The letters common between the two correct box IDs are '%s'" solution

    0 // return an integer exit code
