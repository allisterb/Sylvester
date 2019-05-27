namespace Sylvester

[<AutoOpen>]
module Func =

    type Func = interface end

    /// Function composition
    type Comp<'a, 'b> = Comp of 'a * 'b with
        static member inline (<|-) (Comp(f, g), x) = f <|- (g <|- x)

    let inline (-|>) x f = f <|- x
    let inline (<-<) f g = Comp(f, g)
    let inline (>->) g f = Comp(f, g)

    type Id = Id with 
        interface Func
        static member inline (<|-) (Id, x) = x

    type Fst = Fst with
        interface Func
        static member inline (<|-) (Fst, (x, y)) = x

    type Snd = Snd with
        interface Func
        static member inline (<|-) (Snd, (x, y)) = y

    type Const<'k> = Const of 'k with
        interface Func
        static member inline (<|-) (Const(k), x) = k

    type If<'c, 'p, 'q> = If of 'c * 'p * 'q with
        interface Func
        static member inline (<|-) (If(c, p, q), v) = ((c <|- v) <?> (p, q)) <|- v

    type Not = Not with
        interface Func
        static member inline (<|-) (Not, _:True) = _false
        static member inline (<|-) (Not, _:False) = _true

    type While<'p, 'f> = While of 'p * 'f with
        interface Func
        static member inline (<|-) (While(p, f), v) = ((p <|- v) <?> (While(p, f) <-< f, Id)) <|- v
    
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

    type IsZero = IsZero with
        interface Func
        static member inline (<|-) (IsZero, n:N9<_,_,_,_,_,_,_,_,_>) = isZero n.Digits
    
    type Neg = Neg with
        interface Func
        static member inline (<|-) (Neg, n:N9<_,_,_,_,_,_,_,_,_>) = neg n.Digits


    type Succ = Succ with
        interface Func
        static member inline (<|-) (Succ, n) = n + one

    type Pred = Pred with
        interface Func
        
        static member inline (<|-) (Pred, n) =  n + (neg one.Digits)





  

