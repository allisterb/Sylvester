namespace Sylvester

open System

[<AttributeUsage(AttributeTargets.Field ||| AttributeTargets.Property ||| AttributeTargets.Method)>]
type UnicodeAttribute(symbol:string) =
    inherit Attribute()
    member val Symbol = symbol

[<AutoOpen>]
module Display =
    type DisplayType =
    | Text
    | Latex
    | Jupyter
    | Web

    type IDisplay =
        abstract Output:'t->string
        abstract Transform:string->string
        
    let mutable defaultDisplay = Text