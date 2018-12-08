module Solution

open System


type SearchState =
    | New
    | InProgress of currentFrequency : int * frequenciesSeen : int Set
    | Done of firstRepeatedFrequency : int

let private searchForFirstRepeatedFrequency state frequencyChange =
    match state with
    | InProgress (currentFrequency, frequenciesSeen) ->
        let newFrequency = currentFrequency + frequencyChange
        if Set.contains newFrequency frequenciesSeen then
            Done(newFrequency)
        else
            InProgress(newFrequency, Set.add newFrequency frequenciesSeen)
    | New -> InProgress(frequencyChange, Set.ofArray [| 0; frequencyChange |])
    | other -> other

let private parseFrequencyChange frequencyChange =
    Int32.Parse (frequencyChange)

let getFirstRepeatedFrequency frequencyChanges =
    let parsedFrequencyChanges =
        frequencyChanges
        |> Seq.map parseFrequencyChange
        |> Seq.cache

    let finalSearchState =
        Seq.initInfinite (fun _ -> parsedFrequencyChanges)
        |> Seq.concat
        |> Seq.scan searchForFirstRepeatedFrequency SearchState.New
        |> Seq.skipWhile
            (fun state ->
                match state with
                | Done _ -> false
                | _ -> true)
        |> Seq.head

    match finalSearchState with
    | Done firstRepeatedFrequency -> firstRepeatedFrequency
    | _ -> failwith "Search for the first repeated frequency failed"