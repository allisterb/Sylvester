namespace Sylvester

[<AutoOpen>]
module Constraint =
    open System.Diagnostics

    type Success = Success
    type Failure = Failure
    type Constraint<'c> = Constraint of 'c
   

    let inline check q = 
        let _check _q = _q <?> (Constraint(Success), Constraint(Failure))
        let r:Constraint<Success> = _check q
        ()

    let inline checkres (result:'r, condition) = 
        let res:'r = condition <?> (result, Constraint(Failure))
        res
    
   



