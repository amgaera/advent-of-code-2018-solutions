module Solution

open System
open System.Text.RegularExpressions


type private Rectangle = {
    X : int
    Y : int
    Width : int
    Height : int
}

type private Claim = {
    Id : string
    Area : Rectangle
}

type private RegionTree =
    | InnerNode of minBoundingRect : Rectangle * children : RegionTree list
    | LeafNode of minBoundingRect : Rectangle * claims : Claim list

let private getMinBoundingRect rectangles =
    let minX, minY, maxX, maxY =
        rectangles
        |> Seq.fold
            (fun (minX, minY, maxX, maxY) rect ->
                let newMinX = min minX rect.X
                let newMinY = min minY rect.Y
                let newMaxX = max maxX (rect.X + rect.Width)
                let newMaxY = max maxY (rect.Y + rect.Height)
                newMinX, newMinY, newMaxX, newMaxY)
            (Int32.MaxValue, Int32.MaxValue, Int32.MinValue, Int32.MinValue)
    {
        X = minX
        Y = minY
        Width = maxX - minX
        Height = maxY - minY
    }

let private getRegionTreeMinBoundingRect regionTree =
    match regionTree with
    | LeafNode(minBoundingRect, _claims) ->
        minBoundingRect
    | InnerNode(minBoundingRect, _children) ->
        minBoundingRect

let private createLeafNode claims =
    let minBoundingRect =
        claims
        |> Seq.map (fun claim -> claim.Area)
        |> getMinBoundingRect

    LeafNode(minBoundingRect, claims)

let private createInnerNode children =
    let minBoundingRect =
        children
        |> Seq.map getRegionTreeMinBoundingRect
        |> getMinBoundingRect

    InnerNode(minBoundingRect, children)

let private claimRegex =
    new Regex("#(?<ClaimId>\d+) @ (?<X>\d+),(?<Y>\d+): (?<Width>\d+)x(?<Height>\d+)", RegexOptions.Compiled)

let private parseClaim claimText =
    let matchResult = claimRegex.Match (claimText)
    if not matchResult.Success then
        failwithf "Failed to parse claim from text '%s'" claimText
    
    let claimId = matchResult.Groups.["ClaimId"].Value
    let x = matchResult.Groups.["X"].Value |> Int32.Parse
    let y = matchResult.Groups.["Y"].Value |> Int32.Parse
    let width = matchResult.Groups.["Width"].Value |> Int32.Parse
    let height = matchResult.Groups.["Height"].Value |> Int32.Parse

    let area = { X = x; Y = y; Width = width; Height = height }
    { Id = claimId; Area = area }

let private getClaimAreaPoints claim =
    let area = claim.Area
    seq {
        for x in area.X .. area.X + area.Width - 1 do
            for y in area.Y .. area.Y + area.Height - 1 do
                yield x, y
    }

let private calculatePerimeter rect =
    2 * (rect.Width + rect.Height)

let private splitRectangles rectangles =
    let sortFuncs = [|
        (fun rect -> rect.X)
        (fun rect -> rect.X + rect.Width)
        (fun rect -> rect.Y)
        (fun rect -> rect.Y + rect.Height)
    |]

    sortFuncs
    |> Seq.map (fun sortFunc -> List.sortBy (fst >> sortFunc) rectangles)
    |> Seq.collect
        (fun sortedRectangles ->
            [|
                List.splitAt 2 sortedRectangles
                List.splitAt 3 sortedRectangles
            |])
    |> Seq.minBy
        (fun (rectanglesA, rectanglesB) ->
            [| rectanglesA; rectanglesB |]
            |> Array.map (fun rectanglesWithData -> rectanglesWithData |> Seq.map fst)
            |> Array.sumBy (getMinBoundingRect >> calculatePerimeter))

let private splitClaims claims =
    let firstRectSet, secondRectSet =
        claims
        |> List.map (fun claim -> claim.Area, claim)
        |> splitRectangles

    let firstLeafNode =
        firstRectSet |> List.map snd |> createLeafNode
    let secondLeafNode =
        secondRectSet |> List.map snd |> createLeafNode

    firstLeafNode, secondLeafNode

let private pickSubtreeForInsertion claim subtrees =
    subtrees
    |> Seq.minBy
        (fun subtree ->
            let currentBoundingRect = getRegionTreeMinBoundingRect subtree
            let updatedBoundingRect = getMinBoundingRect [| currentBoundingRect; claim.Area |]

            (calculatePerimeter updatedBoundingRect) - (calculatePerimeter currentBoundingRect))

type private InsertionResult =
    | NoSplit of node : RegionTree
    | Split of firstNode : RegionTree * secondNode : RegionTree

let rec private insertIntoClaimRegionTree claim regionTree =
    match regionTree with
    | LeafNode(_minBoundingRect, claims) ->
        let updatedClaims = List.Cons (claim, claims)
        if updatedClaims.Length > 4 then
            Split(splitClaims updatedClaims)
        else
            NoSplit(createLeafNode updatedClaims)
    | InnerNode(_minBoundingRect, children) ->
        let subtreeForInsertion = pickSubtreeForInsertion claim children

        match insertIntoClaimRegionTree claim subtreeForInsertion with
        | NoSplit updatedSubtree ->
            let updatedSubtrees =
                children
                |> List.map
                    (fun subtree ->
                        if subtree = subtreeForInsertion then
                            updatedSubtree
                        else subtree)
            NoSplit(createInnerNode updatedSubtrees)
        | Split (firstNode, secondNode) ->
            let otherSubtrees =
                children |> List.filter (fun subtree -> subtree <> subtreeForInsertion)
            let updatedSubtrees =
                List.append [firstNode; secondNode] otherSubtrees

            if updatedSubtrees.Length > 4 then
                let firstRectSet, secondRectSet =
                    updatedSubtrees
                    |> List.map (fun subtree -> getRegionTreeMinBoundingRect subtree, subtree)
                    |> splitRectangles

                Split(
                    firstRectSet |> List.map snd |> createInnerNode,
                    secondRectSet |> List.map snd |> createInnerNode)
            else
                NoSplit(createInnerNode updatedSubtrees)

let private createClaimRegionTree regionTree claim =
    match insertIntoClaimRegionTree claim regionTree with
    | NoSplit updatedTree ->
        updatedTree
    | Split (firstNode, secondNode) ->
        createInnerNode [firstNode; secondNode]

let private rectanglesOverlap rectA rectB =
    rectA.X < rectB.X + rectB.Width && rectA.X + rectA.Width > rectB.X &&
        rectA.Y + rectA.Height > rectB.Y && rectA.Y < rectB.Y + rectB.Height

let rec private findOverlappingRegions rect regionTree =
    match regionTree with
    | LeafNode(_minBoundingRect, claims) ->
        claims
        |> Seq.map (fun claim -> claim.Area)
        |> Seq.filter (rectanglesOverlap rect)
    | InnerNode(_minBoundingRect, children) ->
        children
        |> Seq.filter (getRegionTreeMinBoundingRect >> (rectanglesOverlap rect))
        |> Seq.collect (findOverlappingRegions rect)

let getNonOverlappingClaimId claims =
    let parsedClaims  =
        claims
        |> Seq.map parseClaim
        |> Seq.cache

    let regionTree =
        parsedClaims
        |> Seq.fold createClaimRegionTree (LeafNode({ X = 0; Y = 0; Width = 0; Height = 0 }, List.empty))

    let nonOverlappingClaim =
        parsedClaims
        |> Seq.filter
            (fun claim ->
                let overlappingRegionCount =
                    findOverlappingRegions claim.Area regionTree
                    |> Seq.length
                overlappingRegionCount = 1)
        |> Seq.exactlyOne

    nonOverlappingClaim.Id