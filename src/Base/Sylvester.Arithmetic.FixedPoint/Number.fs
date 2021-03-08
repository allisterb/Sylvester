namespace Sylvester.Arithmetic

open System

type Number = 
  abstract Val:int64
  abstract IntVal:int
  abstract UIntVal:uint64

[<AutoOpen>]
module Number = 
    let number<'n when 'n :> Number> = Activator.CreateInstance<'n>()
    