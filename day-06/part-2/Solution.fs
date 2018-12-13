module Solution

open System


let private getAdjacentCoordinates (x, y) =
    seq {
        for x in x - 1 .. x + 1 do
            yield x, y - 1
            yield x, y + 1

        yield x - 1, y
        yield x + 1, y
    }

let private getManhattanDistance (a, b) (x, y) =
    [ x - a; y - b ]
    |> Seq.map abs
    |> Seq.sum

let private getMinTotalDistanceCoord (coords : (int * int) list) =
    let xSum = coords |> Seq.sumBy fst
    let ySum = coords |> Seq.sumBy snd

    xSum / coords.Length, ySum / coords.Length

let private getCoordTotalDistance coord (coords : (int * int) list) =
    coords
    |> Seq.sumBy (getManhattanDistance coord)

type private GetCenteredAreaRecState = {
    MaxTotalCoordDistance : int
    StartCoord : int * int
    CandidateCoords : (int * int) list
    AreaCoords : (int * int) list
    CheckedCoords : Set<int * int>
    CoordQueue : (int * int) list
}

let rec private getCenteredAreaCore state =
    match state.CoordQueue with
    | [] -> state.AreaCoords
    | coord :: coordQueue ->
        let isCoordInArea =
            getCoordTotalDistance coord state.CandidateCoords < state.MaxTotalCoordDistance

        let areaCoords =
            if isCoordInArea then coord :: state.AreaCoords
            else state.AreaCoords

        let newCoords =
            if isCoordInArea then
                getAdjacentCoordinates coord
                |> Seq.filter (fun adjacentCoord -> Set.contains adjacentCoord state.CheckedCoords |> not)
                |> Seq.toList
            else []

        let updatedState =
            { state with AreaCoords = areaCoords
                         CheckedCoords = Set.union (Set.ofList newCoords) state.CheckedCoords
                         CoordQueue = List.append newCoords coordQueue }
        getCenteredAreaCore updatedState

let private getCenteredArea maxTotalCoordDistance coords =
    let startCoord = getMinTotalDistanceCoord coords
    let initialState = {
        MaxTotalCoordDistance = maxTotalCoordDistance
        StartCoord = startCoord
        CandidateCoords = coords
        AreaCoords = List.empty
        CheckedCoords = Set.ofList [ startCoord ]
        CoordQueue = [ startCoord ]
    }

    getCenteredAreaCore initialState

let private parseCoordinate (coordinateText : string) =
    let parsedCoords =
        coordinateText.Split (',')
        |> Array.map Int32.Parse

    match parsedCoords with
    | [| x; y |] -> x, y
    | _ -> failwithf "Failed to parse coordinates from string '%s'" coordinateText

let getSizeOfCenteredArea maxTotalCoordDistance coordinates =
    let parsedCoords =
        coordinates
        |> Seq.map parseCoordinate
        |> Seq.toList

    getCenteredArea maxTotalCoordDistance parsedCoords
    |> List.length
