module Tests

open System
open Xunit


[<Fact>]
let ``Polymer from the first example reduces to an empty string`` () =
    let polymer = "aA"

    Assert.Equal (String.Empty, Solution.reducePolymer polymer)

[<Fact>]
let ``Polymer from the second example reduces to an empty string`` () =
    let polymer = "abBA"

    Assert.Equal (String.Empty, Solution.reducePolymer polymer)

[<Fact>]
let ``Polymer from the third example doesn't reduce`` () =
    let polymer = "abAB"

    Assert.Equal (polymer, Solution.reducePolymer polymer)

[<Fact>]
let ``Polymer from the fourth example doesn't reduce`` () =
    let polymer = "aabAAB"

    Assert.Equal (polymer, Solution.reducePolymer polymer)

[<Fact>]
let ``Polymer from the fifth example reduces to dabCBAcaDA`` () =
    let polymer = "dabAcCaCBAcCcaDA"

    Assert.Equal ("dabCBAcaDA", Solution.reducePolymer polymer)