namespace Sylvester

open Sylvester.Arithmetic

type TagId = Number 

type Tagged<'o, 't when 't :> TagId>(o:'o) = 
    member x.Obj = o
    member x.Tag = number<'t>
  