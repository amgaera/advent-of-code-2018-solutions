open System


[<EntryPoint>]
let main argv =
    let input = seq {
        while Console.In.Peek() >= 0 do
            yield Console.In.ReadLine()
    }
    let claims =
        input |> Seq.filter (fun line -> String.IsNullOrEmpty (line) |> not)

    let solution = Solution.getOverlappingClaimAreaSize claims
    printfn "'%d' square inches of fabric are within two or more claims" solution

    0 // return an integer exit code
