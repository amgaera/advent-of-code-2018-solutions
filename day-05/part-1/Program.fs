open System


[<EntryPoint>]
let main argv =
    let input = seq {
        while Console.In.Peek() >= 0 do
            yield Console.In.ReadLine()
    }
    let polymer =
        input |> Seq.filter (fun line -> String.IsNullOrEmpty (line) |> not) |> Seq.exactlyOne

    let solution = Solution.reducePolymer polymer
    printfn "Polymer '%s' reduces to '%d' units" polymer solution.Length

    0 // return an integer exit code
