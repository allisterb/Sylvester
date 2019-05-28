// Contains code from https://github.com/Ming-Tang/TypeLevel/blob/master/TypeLevel/Functional.fs by Ming-Tang: http://stackoverflow.com/users/303939/Ming-Tang
namespace Sylvester
#nowarn "3186"

open System

[<AutoOpen>]
module Func =

    
    /// Function composition
    type Comp<'a, 'b> = Comp of 'a * 'b with
        static member inline (<|-) (Comp(f, g), x) = f <|- (g <|- x)
    
    let inline (-|>) x f = f <|- x
    let inline (<-<) f g = Comp(f, g)
    let inline (>->) g f = Comp(f, g)

    /// Functions
    type Id = Id with 
        static member inline (<|-) (Id, x) = x

    type Const<'k> = Const of 'k with
        static member inline (<|-) (Const(k), x) = k

    type Not = Not with
        static member inline (<|-) (Not, _:True) = _false
        static member inline (<|-) (Not, _:False) = _true

    type Or = Or with
        static member inline (<|-) (Or, (_:True, _:True)) = _true
        static member inline (<|-) (Or, (_:True, _:False)) = _true
        static member inline (<|-) (Or, (_:False, _:True)) = _true
        static member inline (<|-) (Or, (_:False, _:False)) = _false

    type And = And with
        static member inline (<|-) (And, (_:True, _:True)) = _true
        static member inline (<|-) (And, (_:True, _:False)) = _false
        static member inline (<|-) (And, (_:False, _:True)) = _false
        static member inline (<|-) (And, (_:False, _:False)) = _false

    type Add<'n> = Add of 'n  with
        static member inline (<|-) (Add(n), x) = x + n

    type Sub<'n> = Sub of 'n  with
        static member inline (<|-) (Sub(n), x) = x - n
  
    type Mul<'n> = Mul of 'n  with
        static member inline (<|-) (Mul(n), x) = x * n

    type LessThan<'n> = LessThan of 'n  with
        static member inline (<|-) (LessThan(n), x) = x +< n

    type GreaterThan<'n> = GreaterThan of 'n  with
        static member inline (<|-) (GreaterThan(n), x) = x +> n

    type If<'c, 'p, 'q> = If of 'c * 'p * 'q with
        static member inline (<|-) (If(c, p, q), v) = ((c <|- v) <?> (p, q)) <|- v

    (*
    type While<'p, 'f> = While of 'p * 'f with
        static member inline (<|-) (While(p, f), v) = ((p <|- v) <?> (While(p, f) <-< f, Id)) <|- v
    *)
    type For<'s, 'e, 'f> = For of 's * 'e * 'f with
        static member inline (<|-) (For(s, e, f), v) = (s +< e <?> (For(s + one, e, f) <-< f, Id)) <|- v

  


    


  

