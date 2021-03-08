
## Sylvester.Arithmetic

This library implements lightweight dependently typed natural number arithmetic and constraints which enable arithmetic operations like bounds checking to be performed at compile-time by the F# type checker as long as the values for the operations are known at compile-time. 


```fsharp
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10

///Create typed representations of some natural numbers
let a,b,c = new Nat<400>(), new Nat<231111>(), new Nat<6577700>()

a + b + c
```




    Nat<6809211UL>



These values have types derived from *Sylvester.Arithmetic.N10*. The type of ``a`` is *N10&lt;0,0,0,0,0,0,0,4,0,0&gt;* and the type of ``c`` is *N10&lt;0,0,0,6,5,7,7,7,0,0&gt;*.


```fsharp
c.GetType()
```




    Sylvester.Arithmetic.N10+N10`10[Sylvester.Arithmetic.Base10+0,Sylvester.Arithmetic.Base10+0,Sylvester.Arithmetic.Base10+0,Sylvester.Arithmetic.Base10+6,Sylvester.Arithmetic.Base10+5,Sylvester.Arithmetic.Base10+7,Sylvester.Arithmetic.Base10+7,Sylvester.Arithmetic.Base10+7,Sylvester.Arithmetic.Base10+0,Sylvester.Arithmetic.Base10+0]



The types of the results of arithmetic operations depend on the values of the operands.


```fsharp
let d = (a + b + c) * four

d
```




    N<27236844UL>




```fsharp
d.GetType()
```




    Sylvester.Arithmetic.N10+N10`10[Sylvester.Arithmetic.Base10+0,Sylvester.Arithmetic.Base10+0,Sylvester.Arithmetic.Base10+2,Sylvester.Arithmetic.Base10+7,Sylvester.Arithmetic.Base10+2,Sylvester.Arithmetic.Base10+3,Sylvester.Arithmetic.Base10+6,Sylvester.Arithmetic.Base10+8,Sylvester.Arithmetic.Base10+4,Sylvester.Arithmetic.Base10+4]



This enables type-level constraints to be written which run at compile-time


```fsharp
check(d +< ten) /// Type error
```

    Type mismatch. Expecting a
        'Constraint<Success>'    
    but given a
        'Constraint<Failure>'    
    The type 'Success' does not match the type 'Failure'


```fsharp
a - (two * a) /// A negative number results in a type representing an underflow at compile-time 
```




    N10Underflow




```fsharp
let myop a b c d =
    check(a +> b)
    check (b +== zero)
    check (c +== (a + one))
    a + b + c + d
    
myop four five four ten  ///Type error. Program will not compile
```

    Type mismatch. Expecting a
        'Constraint<Success>'    
    but given a
        'Constraint<Failure>'    
    The type 'Success' does not match the type 'Failure'
    Type mismatch. Expecting a
        'Constraint<Success>'    
    but given a
        'Constraint<Failure>'    
    The type 'Success' does not match the type 'Failure'


```fsharp
let myop a b c d =
    check(a +> b)
    check (b +== zero)
    check (c +== (a + one))
    a + b + c + d
    
myop seven zero eight ten  ///Program compiles once the parameters to myop satisfy the arithmetic constraints
```




    Nat<25UL>


