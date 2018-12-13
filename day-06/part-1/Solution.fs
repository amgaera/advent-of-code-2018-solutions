module Solution

open System


type private Rectangle = {
    X : int
    Y : int
    Width : int
    Height : int
}

let private getMinBoundingRect coordinates =
    let minX, minY, maxX, maxY =
        coordinates
        |> Seq.fold
            (fun (minX, minY, maxX, maxY) (x, y) ->
                min minX x, min minY y, max maxX x, max maxY y)
            (Int32.MaxValue, Int32.MaxValue, Int32.MinValue, Int32.MinValue)
    
    { X = minX; Y = minY; Width = maxX- minX; Height = maxY - minY }

let private getPerimeterCoordinates rect =
    seq {
        for x in rect.X .. rect.X + rect.Width do
            yield x, rect.Y
            yield x, rect.Y + rect.Height
        
        for y in rect.Y + 1 .. rect.Y + rect.Height - 1 do
            yield rect.X, y
            yield rect.X + rect.Width, y
    }

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

let private getClosestCoord coords startCoord =
    let sortedCoords =
        coords
        |> Seq.map (fun coord -> coord, getManhattanDistance startCoord coord)
        |> Seq.sortBy snd
        |> Seq.take 2
        |> Seq.toArray

    match sortedCoords with
    | [| (p1, d1); (_p2, d2) |] ->
        if d1 = d2 then None
        else Some p1
    | [| (p1, _d1) |] -> Some p1
    | _ -> failwithf "Unexpected number of elements in sorted coordinates: '%d'" sortedCoords.Length

let private getCoordsWithFiniteAreas minBoundingRect coords =
    getPerimeterCoordinates minBoundingRect
    |> Seq.choose (getClosestCoord coords)
    |> Set.ofSeq
    |> Set.difference (Set.ofSeq coords)

type private GetCoordinateAreaRecState = {
    StartCoord : int * int
    CandidateCoords : (int * int) list
    AreaCoords : (int * int) list
    CheckedCoords : Set<int * int>
    CoordQueue : (int * int) list
}

let rec private getCoordinateAreaCore state =
    match state.CoordQueue with
    | [] -> state.AreaCoords
    | coord :: coordQueue ->
        let isCoordInArea =
            getClosestCoord state.CandidateCoords coord
            |> Option.map
                (fun closestCoord ->
                    if closestCoord = state.StartCoord then true
                    else false)
            |> Option.defaultValue false

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
        getCoordinateAreaCore updatedState

let private getCoordinateArea coords coord =
    let initialState = {
        StartCoord = coord
        CandidateCoords = coords
        AreaCoords = List.empty
        CheckedCoords = Set.ofList [ coord ]
        CoordQueue = [ coord ]
    }

    getCoordinateAreaCore initialState

let private parseCoordinate (coordinateText : string) =
    let parsedCoords =
        coordinateText.Split (',')
        |> Array.map Int32.Parse

    match parsedCoords with
    | [| x; y |] -> x, y
    | _ -> failwithf "Failed to parse coordinates from string '%s'" coordinateText

let getLargestFiniteAreaSize coordinates =
    let parsedCoords =
        coordinates
        |> Seq.map parseCoordinate
        |> Seq.toList
    let minBoundingRect =
        parsedCoords
        |> getMinBoundingRect
    
    getCoordsWithFiniteAreas minBoundingRect parsedCoords
    |> Seq.map (getCoordinateArea parsedCoords)
    |> Seq.map List.length
    |> Seq.max
