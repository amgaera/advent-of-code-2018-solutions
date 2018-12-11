module Tests

open System
open Xunit


[<Fact>]
let ``The best reduction for the polymer from the first example is daDA`` () =
    let polymer = "dabAcCaCBAcCcaDA"

    Assert.Equal ("daDA", Solution.findBestAugmentedPolymerReduction polymer)