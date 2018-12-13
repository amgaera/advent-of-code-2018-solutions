module Tests

open Xunit


[<Fact>]
let ``The size of the region containing all locations which have a total distance to all given coordinates of less than 32 is 16`` () =
    let coordinates = [|
        "1, 1"
        "1, 6"
        "8, 3"
        "3, 4"
        "5, 5"
        "8, 9"
    |]

    Assert.Equal (16, Solution.getSizeOfCenteredArea 32 coordinates)
