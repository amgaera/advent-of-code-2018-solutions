module Tests

open Xunit


[<Fact>]
let ``The letters common between the two correct box IDs in the first example are fgij`` () =
    let boxIds = [|
        "abcde"; "fghij"; "klmno"; "pqrst"; "fguij"; "axcye"; "wvxyz"
    |]

    Assert.Equal ("fgij", Solution.getCorrectBoxIdsCommonLetters boxIds)
