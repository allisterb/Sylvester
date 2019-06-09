namespace Sylvester.Arithmetic

[<AutoOpen>]
module Constraint =
    type Success = Success
    type Failure = Failure
    type Constraint<'c> = Constraint of 'c
   

    let inline check q = 
        let _:Constraint<Success> = q <?> (Constraint(Success), Constraint(Failure))
        ()

    let inline checkres (result:'r, condition) = 
        let res:'r = condition <?> (result, Constraint(Failure))
        res





                








    



 


  
  

    


   



