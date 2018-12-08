module Tests

open Xunit


[<Fact>]
let ``The resulting frequency in the first example is 3`` () =
    let frequencyChanges = [|
        "+1"; "-2"; "+3"; "+1"
    |]

    Assert.Equal (3, Solution.getResultingFrequency frequencyChanges)

[<Fact>]
let ``The resulting frequency in the second example is 3`` () =
    let frequencyChanges = [|
        "+1"; "-2"; "+3"; "+1"
    |]

    Assert.Equal (3, Solution.getResultingFrequency frequencyChanges)

[<Fact>]
let ``The resulting frequency in the third example is 0`` () =
    let frequencyChanges = [|
        "+1"; "+1"; "-2"
    |]

    Assert.Equal (0, Solution.getResultingFrequency frequencyChanges)

[<Fact>]
let ``The resulting frequency in the fourth example is -6`` () =
    let frequencyChanges = [|
        "-1"; "-2"; "-3"
    |]

    Assert.Equal (-6, Solution.getResultingFrequency frequencyChanges)
