namespace Sylvester.Collections
 
open System
open System.Collections.Generic

open Sylvester.Arithmetic

/// An array of n objects.
[<AbstractClass>]
type Array<'n when 'n :> Number>() = 
    
    member x.Length = number<'n>
    
    member x.IntLength = x.Length.IntVal

    static member inline (!+) (v:Array<'n>) = v.Length

/// An array of n objects of type 't.
[<StructuredFormatDisplay("{Display}")>]
type Array<'n, 't when 'n :> Number>(items:'t[]) = 
    inherit Array<'n>()
    
    member x._Array = if items.Length = x.IntLength then items else raise (ArgumentOutOfRangeException("items", sprintf "The initializing array length %i does not match the type length %i." items.Length x.IntLength))
         
    member inline x.SetVal(i:'i, item:'t) =
        checkidx(i, x.Length)
        x._Array.[i |> int] <- item

    member inline x.For(start:'start, finish:'finish, f: int -> 't -> unit) =
        checkidx(start, x.Length)
        checkidx(finish, x.Length)
        checklt(start, finish)
        for i in ((int) start)..((int)finish) do f i x._Array.[i]
      
    member inline x.ForAll(f: int -> 't -> unit) =
        for i in 0..x._Array.Length - 1 do f i x._Array.[i]

    member inline x.SetVals(items: IEnumerable<'t> ) = 
        do if Seq.length items <> x.IntLength then raise(ArgumentOutOfRangeException("The length of the sequence argument does not match the type length."))
        x.ForAll(fun i a -> x._Array.SetValue(a, i))

    member inline x.Item(i:'i) : 't = 
        checkidx(i, x.Length)
        x._Array.[i |> int]
           
    member inline x.GetSlice(start: 'a option, finish : 'b option) = 
        let inline create(c:'c, items: 't[] when 'c :> Number) = 
            Array<'n, 't>(items)

        checkidx(start.Value, x.Length)
        checkidx(finish.Value, x.Length)
        checklt(start.Value, finish.Value)
        let _start, _finish = start.Value, finish.Value            
        let intstart, intfinish = _start |> int, _finish |> int
        let length = (_finish - _start)   

        create(length, x._Array.[intstart..intfinish])

    member inline x.Map(m:'t->'u) = Array<'n, 'u>(Array.map m x._Array)
    
    member x.Display = sprintf "Array<%i, %s>: %A" x.IntLength typeof<'t>.Name x._Array

    new(x:'t) = 
        Array<'n, 't>(Array.create (number<'n>.IntVal) x)

    new() = 
        Array<'n, 't>(Array.create number<'n>.IntVal Unchecked.defaultof<'t>)

 type Array<'n when 'n :> Number> with
    /// Create an array of n objects of type 't.
    static member create(arr: 't[]) = new Array<'n, 't>(arr)
    
    /// Create an array of n objects of type 't of a single value.
    static member create(v: 't) = new Array<'n, 't>(Array.create (number<'n>.IntVal) (v))

[<AutoOpen>]
module VArray =
    
    open Sylvester.Arithmetic.N10

    let inline vainit (items:IEnumerable<'t>) (vl:Array<'n, 't>) = 
        do vl.SetVals items
        vl
    
    let inline varray (n: 'n when 'n :> Number) (arr:'t[]) = Array<'n, 't>(arr)
    
    let inline vanew<'n, 't when 'n :> Number>  =  Array<'n, 't>()

    let inline vacopy (n: 'n) (items:IEnumerable<'t>) =  vanew<'n, 't>  |> vainit items

    let arrayOf1<'t> (x1:'t) = Array<one, 't>(x1)

    let arrayOf2<'t> (x1:'t) (x2:'t) = Array<two, 't>([|x1; x2|])

    let arrayOf3<'t> (x1:'t) (x2:'t) (x3:'t) = Array<three, 't>([|x1; x2; x3|])

    let arrayOf4<'t> (x1:'t) (x2:'t) (x3:'t) (x4:'t) = Array<four, 't>([|x1; x2; x3; x4|])

    let arrayOf5<'t> (x1:'t) (x2:'t) (x3:'t) (x4:'t) (x5:'t) = Array<five, 't>([|x1; x2; x3; x4; x5|])

    let arrayOf6<'t> (x1:'t) (x2:'t) (x3:'t) (x4:'t) (x5:'t) (x6:'t) = Array<six, 't>([|x1; x2; x3; x4; x5; x6|])

    let arrayOf7<'t> (x1:'t) (x2:'t) (x3:'t) (x4:'t) (x5:'t) (x6:'t) (x7:'t) = Array<seven, 't>([|x1; x2; x3; x4; x5; x6; x7|])

    let arrayOf8<'t> (x1:'t) (x2:'t) (x3:'t) (x4:'t) (x5:'t) (x6:'t) (x7:'t) (x8:'t) = Array<eight, 't>([|x1; x2; x3; x4; x5; x6; x7; x8|])

    let arrayOf9<'t> (x1:'t) (x2:'t) (x3:'t) (x4:'t) (x5:'t) (x6:'t) (x7:'t) (x8:'t) (x9:'t) = Array<nine, 't>([|x1; x2; x3; x4; x5; x6; x7; x8; x9|])

    let arrayOf10<'t> (x1:'t) (x2:'t) (x3:'t) (x4:'t) (x5:'t) (x6:'t) (x7:'t) (x8:'t) (x9:'t) (x10:'t) = Array<ten, 't>([|x1; x2; x3; x4; x5; x6; x7; x8; x9; x10|])
    