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

let private getManhattanDistance (a, b) (x, y) =
    [ x - a; y - b ]
    |> Seq.map abs
    |> Seq.sum

let private getClosestCoord coords startCoord =
    coords
    |> Seq.minBy (getManhattanDistance startCoord)

let private getCoordsWithFiniteAreas minBoundingRect coords =
    getPerimeterCoordinates minBoundingRect
    |> Seq.map (getClosestCoord coords)
    |> Set.ofSeq
    |> Set.difference (Set.ofSeq coords)

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
        |> Seq.cache
    let minBoundingRect =
        parsedCoords
        |> getMinBoundingRect
    
    let coordsWithFiniteAreas = getCoordsWithFiniteAreas minBoundingRect parsedCoords
    
    printfn "%A" coordsWithFiniteAreas
    0
