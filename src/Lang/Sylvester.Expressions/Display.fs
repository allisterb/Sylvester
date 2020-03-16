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
