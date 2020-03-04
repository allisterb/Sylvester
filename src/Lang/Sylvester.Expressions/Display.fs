namespace Sylvester

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

    let replaceCommonLogicalSymbols (s:string) = 
        s.Replace("|||", "\u2228")
         .Replace("|&|", "\u2227")
         .Replace("==", "\u2261")
         .Replace("|-", "\u22A2")
         .Replace(" not ", " \u00AC ")
         .Replace("not ", "\u00AC ")
         .Replace(" * ", " \u22C5 ")
         .Replace("* ", "\u22C5 ")
         .Replace("!!", " \u00AC ")