module Tests

open Xunit


[<Fact>]
let ``The first repeated frequency in the first example is 2`` () =
    let frequencyChanges = [|
        "+1"; "-2"; "+3"; "+1"
    |]

    Assert.Equal (2, Solution.getFirstRepeatedFrequency frequencyChanges)

[<Fact>]
let ``The first repeated frequency in the second example is 0`` () =
    let frequencyChanges = [|
        "+1"; "-1"
    |]

    Assert.Equal (0, Solution.getFirstRepeatedFrequency frequencyChanges)

[<Fact>]
let ``The first repeated frequency in the third example is 10`` () =
    let frequencyChanges = "+3, +3, +4, -2, -4".Split (',')

    Assert.Equal (10, Solution.getFirstRepeatedFrequency frequencyChanges)

[<Fact>]
let ``The first repeated frequency in the fourth example is 5`` () =
    let frequencyChanges = "-6, +3, +8, +5, -6".Split (',')

    Assert.Equal (5, Solution.getFirstRepeatedFrequency frequencyChanges)

[<Fact>]
let ``The first repeated frequency in the fifth example is 14`` () =
    let frequencyChanges = "+7, +7, -2, -7, -4".Split (',')

    Assert.Equal (14, Solution.getFirstRepeatedFrequency frequencyChanges)
