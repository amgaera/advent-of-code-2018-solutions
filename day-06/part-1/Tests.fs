module Tests

open Xunit


[<Fact>]
let ``The size of the largest finite area in the first example is 17`` () =
    let coordinates = [|
        "1, 1"
        "1, 6"
        "8, 3"
        "3, 4"
        "5, 5"
        "8, 9"
    |]

    Assert.Equal (17, Solution.getLargestFiniteAreaSize coordinates)
