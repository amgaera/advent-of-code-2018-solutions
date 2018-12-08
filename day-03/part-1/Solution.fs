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

let claimRegex = new Regex("#(?<ClaimId>\d+) @ (?<X>\d+),(?<Y>\d+): (?<Width>\d+)x(?<Height>\d+)", RegexOptions.Compiled)

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

let getOverlappingClaimAreaSize claims =
    claims
    |> Seq.map parseClaim
    |> Seq.collect getClaimAreaPoints
    |> Seq.countBy id
    |> Seq.map snd
    |> Seq.filter (fun overlapCount -> overlapCount > 1)
    |> Seq.length