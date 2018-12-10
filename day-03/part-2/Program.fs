open System


[<EntryPoint>]
let main argv =
    let input = seq {
        while Console.In.Peek() >= 0 do
            yield Console.In.ReadLine()
    }
    let claims =
        input |> Seq.filter (fun line -> String.IsNullOrEmpty (line) |> not)

    let solution = Solution.getNonOverlappingClaimId claims
    printfn "The ID of the only claim that doesn't overlap is '%s'" solution

    0 // return an integer exit code
