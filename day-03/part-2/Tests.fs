module Tests

open Xunit


[<Fact>]
let ``4 square inches of fabric are within two or more claims in the first example`` () =
    let claims = [|
        "#1 @ 1,3: 4x4"
        "#2 @ 3,1: 4x4"
        "#3 @ 5,5: 2x2"
    |]

    Assert.Equal ("3", Solution.getNonOverlappingClaimId claims)
