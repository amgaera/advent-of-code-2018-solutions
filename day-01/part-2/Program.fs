open System
open System.IO


[<EntryPoint>]
let main argv =
    let input = seq {
        while Console.In.Peek() >= 0 do
            yield Console.In.ReadLine()
    }
    let frequencyChanges =
        input |> Seq.filter (fun line -> String.IsNullOrEmpty (line) |> not)

    let solution = Solution.getFirstRepeatedFrequency frequencyChanges
    printfn "The first repeated frequency is '%d'" solution

    0 // return an integer exit code
