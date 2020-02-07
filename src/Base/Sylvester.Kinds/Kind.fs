namespace Sylvester

open Sylvester.Arithmetic

type TagId = Number 

[<StructuredFormatDisplay("{Display}")>]
type Kind<'o, 'tag when 'tag :> TagId>(o:'o, tag: 'tag, display:string) = 
    member x.Obj = o
    member x.Tag = tag
    member val Display = display
  