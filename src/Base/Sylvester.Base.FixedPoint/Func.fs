// Contains code from https://github.com/Ming-Tang/TypeLevel/blob/master/TypeLevel/Functional.fs by Ming-Tang: http://stackoverflow.com/users/303939/Ming-Tang
namespace Sylvester
#nowarn "3186"

open System

[<AutoOpen>]
module Func =

    type Func = interface end

    /// Function composition
    type Comp<'a, 'b> = Comp of 'a * 'b with
        interface Func
        static member inline (<|-) (Comp(f, g), x) = f <|- (g <|- x)
    
    let inline (-|>) x f = f <|- x
    let inline (<-<) f g = Comp(f, g)
    let inline (>->) g f = Comp(f, g)

    /// Functions
    type Id = Id with 
        interface Func
        static member inline (<|-) (Id, x) = x

    type Const<'k> = Const of 'k with
        interface Func
        static member inline (<|-) (Const(k), x) = k

    type Not = Not with
        interface Func
        static member inline (<|-) (Not, _:True) = _false
        static member inline (<|-) (Not, _:False) = _true

    type Or = Or with
        interface Func
        static member inline (<|-) (Or, (_:True, _:True)) = _true
        static member inline (<|-) (Or, (_:True, _:False)) = _true
        static member inline (<|-) (Or, (_:False, _:True)) = _true
        static member inline (<|-) (Or, (_:False, _:False)) = _false

    type And = And with
        interface Func
        static member inline (<|-) (And, (_:True, _:True)) = _true
        static member inline (<|-) (And, (_:True, _:False)) = _false
        static member inline (<|-) (And, (_:False, _:True)) = _false
        static member inline (<|-) (And, (_:False, _:False)) = _false

    type Add<'n> = Add of 'n  with
        interface Func
        static member inline (<|-) (Add(n), x) = x + n

    type Sub<'n> = Sub of 'n  with
        interface Func
        static member inline (<|-) (Sub(n), x) = x - n


  
    type Mul<'n> = Mul of 'n  with
        interface Func
        static member inline (<|-) (Mul(n), x) = x * n

    type Eq<'n> = Eq of 'n  with
        interface Func
        static member inline (<|-) (Eq(n), x) = x +== n

    type LessThan<'n> = LessThan of 'n  with
        interface Func
        static member inline (<|-) (LessThan(n), x) = x +< n

    type GreaterThan<'n> = GreaterThan of 'n  with
        interface Func
        static member inline (<|-) (GreaterThan(n), x) = x +> n

 

    //let inline addneg (a9, a8, a7, a6, a5, a4, a3, a2, a1)
    type IsZero = IsZero with
        interface Func
        static member inline (<|-) (IsZero, n:N9<_,_,_,_,_,_,_,_,_>) = N9<_,_,_,_,_,_,_,_,_>.IsZero(n) 
    
    type IsNotZero = IsNotZero with
        interface Func
        static member inline (<|-) (IsNotZero, n:N9<_,_,_,_,_,_,_,_,_>) = !!N9<_,_,_,_,_,_,_,_,_>.IsZero(n) 

     
    type Neg = Neg with
        interface Func
        static member inline (<|-) (Neg, n:N9<_,_,_,_,_,_,_,_,_>) = N9<_,_,_,_,_,_,_,_,_>.Neg

    type If<'c, 'p, 'q> = If of 'c * 'p * 'q with
        interface Func
        static member inline (<|-) (If(c, p, q), v) = ((c <|- v) <?> (p, q)) <|- v

    let inline w (f, n) =
        //let inline id x = x
        let inline g a = ((IsZero <|- a) <?> (Id, f))
        let r0 = g n
        let r1 = g (n - one) >-> r0
        let r2 = g (n - two) >-> r1
        let r3 = g (n - three) >-> r2
       
        r3 <|- n

    //let i p = p + one  
    let j = w (Add(one), four)

   
   (*
    type While<'p, 'f> = While of 'p * 'f with
        interface Func
        static member inline (<|-) (While(p, f), v) = (p <|- v) <?> (While(p, f) <-< f, Id) <|- v
        *)

 (*
 type While2<'e, 'f> = While2 of 'f with
        interface Func
        //static member (<|-) (While2(_:N0, _), v) = v
        static member (<|-) (While2(_:_:N9<_,_,_,_,_,_,_,_,_1>, f), v) = While2(zero , f) <|- v
        static member inline (<|-) (While2(n, v)) = While2(n - one , f) <|- v
        //static member (<|-) (While2(_:_:N9<_0,_0,_0,_0,_0,_0,_0,_0,_2>, f), v) = While2(one, f << f) <|- v
        //static member (<|-) (While2(_:_:N9<_0,_0,_0,_0,_0,_0,_0,_0,_3>, f), v) = While2(two, f << f << f) <|- v
   *)

    (*
    type WhileNotZero<'e, 'f> = WhileNotZero of 'e * 'f with
        interface Func
        static member inline (<|-) (WhileNotZero(e, f), v) = (IsNotZero <|- e) <?> (WhileNotZero(e - one, f) <-< f, Id) <|- v
    
    
    type While2<'e, 'f> = While2 of 'f with
        interface Func
        //static member inline (<|-) (While2(_:N0, _), v) = v
        static member inline (<|-) (While2(e, f), v) = ((IsZero <|- e) <?> While2(e - one, f <-< f) , Id) <|- v 

    
    type For<'e, 'f> = For of 'e * 'f with
        static member inline (<|-) (For(_:N0, f), v) = v
        static member inline (<|-) (For(_:N9<_0,_0,_0,_0,_0,_0,_0,_0,_1>, f), v) = For(zero, f) <|= f v
        static member inline (<|-) (For(_:N9<_0,_0,_0,_0,_0,_0,_0,_0,_2>, f), v) = For(one, f) <|- (f << f) v
        static member inline (<|-) (For(_:N9<_0,_0,_0,_0,_0,_0,_0,_0,_3>, f), v) = For(two, f) <|- (f << f << f) v
        //static member inline (<|-) (For(n, f), v) = For(n - one, f) <|- (f << f << f) v
        //static member inline (<|-) (For(_:_3, f), v) = For(d2, f) <|- (f << f << f) v

        
        //static member inline (<|-) (For(three, f), v) = For(two, f >> f) <|- v
        //static member inline (<|-) (For(three, f), v) = For(two, f >> f >> f) <|- v
        //static member inline (<|-) (For(four, f), v) = For(three, f >> f >> f >> f) <|- v
        //static member inline (<|-) (While3(p, f), v) = (p(v)) <?> (While3(p, f) <|- f(v), v)

    
        *)
    (*
    type While3<'p, 'f> = While3 of 'p * 'f with
        
        //static member inline x p f v = (While3(p, f), v) = (p(v)) <?> (While3(p, f) <|- f(v), v)
        static member inline (<|-) (_:_0, v) = v 
        static member inline (<|-) (While3(p, f), v) = (p(v)) <?> (While3(p, f) <|- f(v), v)
            //et inline sitch (a:Bool, b, c) = a <?> (b,c)
      *)      
        
      (*
    type While0<'p, 'f> = While0 of 'p * 'f with
        interface Func
        static member inline (<|-) (While0(p, f), v) = (IsZero <|- p) <?> (While(Sub(one) <|- p, f) <-< f, Id) <|- v
    
      *)
      (*
    type While2<'n, 'p> = While2 of 'n * 'p with
        interface Func
        static member inline (<|-) (While2(p, f), v) = If(p, While2(p, f) <-< f, Id) <|- v
        *)
        //If(//(if(p) <?> ((While(p, f) <|- f(v)), v))
        
    
        
        
    (*
    type For<'e, 'f> = For of 'e * 'f with
        static member inline (<|-) (For(e, f), v) = ((IsZero <|- e) <?> (Id, For(e - one, f) <-< f)) <|- v //(For(s, e, f), v) = (For(s, e - one, f) <-< f) <|- v // //(For(s, e - one, f) <-< f) <|- v//
      *)  

  

