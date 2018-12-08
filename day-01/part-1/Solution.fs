module Solution

open System


let private parseFrequencyChange frequencyChange =
    Int32.Parse (frequencyChange)

let getResultingFrequency frequencyChanges =
    frequencyChanges
    |> Seq.map parseFrequencyChange
    |> Seq.sum
