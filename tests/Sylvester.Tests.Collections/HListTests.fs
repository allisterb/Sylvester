namespace Sylvester.Tests.Collections

module HListTests =

    open Xunit
    open Sylvester
    open N10
    open Sylvester.Collections

    type ZeroMap = ZeroMap with
        static member inline ($) (ZeroMap, s: string) = ""
        static member inline ($) (ZeroMap, i: int) = 0

    // Example of converting to strings and folding them up
    type StrFolder = StrFolder with
        static member inline ($) (StrFolder, (s1: string, s2: string)) = s1 + s2 
        static member inline ($) (StrFolder, (s1: string, i2: int)) = s1 + (string i2)
    
    let hl1 = 1 ^+^ '1' ^+^ HNil
    let hl2 =  "1" ^+^ true ^+^ HNil
    let hl3 = "one" ^+^ 123 ^+^ HNil
    
    (*
    [<Fact>]
    let ``Can create HList``() =
        let len1 = !+ hl1
        let a  = HMapper(ZeroMap) $ hl3 
        let b = HFolder(StrFolder, "") $ hl3
        let x = hl1 |@| one
        Assert.Equal('1', x)
        ()

        *)