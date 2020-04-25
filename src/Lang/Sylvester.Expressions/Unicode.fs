namespace Sylvester

open System

[<AutoOpen>]
module Unicode =
    [<AttributeUsage(AttributeTargets.Field ||| AttributeTargets.Property ||| AttributeTargets.Method)>]
    type UnicodeAttribute(symbol:string) =
        inherit Attribute()
        member val Symbol = symbol
    

