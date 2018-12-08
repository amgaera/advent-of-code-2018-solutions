open System


[<EntryPoint>]
let main argv =
    let input = Console.In.ReadToEnd ()
    let frequencyChanges =
        input.Split (Environment.NewLine)
        |> Seq.filter (fun line -> String.IsNullOrEmpty (line) |> not)

    let solution = Solution.getResultingFrequency frequencyChanges
    printfn "The resulting frequency is '%d'" solution

    0 // return an integer exit code
