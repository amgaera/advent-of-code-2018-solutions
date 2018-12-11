open System


[<EntryPoint>]
let main argv =
    let input = seq {
        while Console.In.Peek() >= 0 do
            yield Console.In.ReadLine()
    }
    let coordinates =
        input |> Seq.filter (fun line -> String.IsNullOrEmpty (line) |> not)

    let solution = Solution.getLargestFiniteAreaSize coordinates
    printfn "The size of the largest finite area is '%d'" solution

    0 // return an integer exit code
