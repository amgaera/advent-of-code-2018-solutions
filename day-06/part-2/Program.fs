open System


[<EntryPoint>]
let main argv =
    let input = seq {
        while Console.In.Peek() >= 0 do
            yield Console.In.ReadLine()
    }
    let coordinates =
        input |> Seq.filter (fun line -> String.IsNullOrEmpty (line) |> not)

    let maxTotalCoordDistance = 10000
    let solution = Solution.getSizeOfCenteredArea maxTotalCoordDistance coordinates

    printf "The size of the region containing all locations which have a total distance "
    printfn "to all given coordinates of less than '%d' is '%d'" maxTotalCoordDistance solution

    0 // return an integer exit code
