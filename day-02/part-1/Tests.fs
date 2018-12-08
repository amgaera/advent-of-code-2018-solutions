module Tests

open Xunit


[<Fact>]
let ``The resulting frequency in the first example is 3`` () =
    let boxIds = [|
        "abcdef"; "bababc"; "abbcde"; "abcccd"; "aabcdd"; "abcdee"; "ababab"
    |]

    Assert.Equal (12, Solution.getBoxIdListChecksum boxIds)
