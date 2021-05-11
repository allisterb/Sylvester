namespace Sylvester

[<AutoOpen>]
module Constraints =
    
    type Success = Success
    
    type Failure = Failure
    
    type Constraint<'n> = Constraint of 'n

    type IndexInRange<'n> = IndexInRange of 'n

    type IndexOutOfRange<'n> = IndexOutOfRange of 'n

    type GreaterThanOrEqual<'n> = GreaterThanOrEqual of 'n

    type LessThan<'n> = LessThan of 'n

    type GreaterThan<'n> = GreaterThan of 'n

    type LessThanOrEqual<'n> = LessThanOrEqual of 'n

    type Equal<'n> = Equal of 'n

    type NotEqual<'n> = NotEqual of 'n
   
    let inline check q = 
        let _:Constraint<Success> = q <?> (Constraint(Success), Constraint(Failure))
        ()

    let inline checkres (result:'r, condition) = 
        let res:'r = condition <?> (result, Constraint(Failure))
        res        

    let inline checkidx(a: 'a, l:'n  when 'a: (static member (+@<<): 'a ->'n -> IndexInRange<'n>)) = ()

    let inline checklt(l: 'l, r:'r  when 'l: (static member (+@<): 'l ->'r -> LessThan<'r>)) = ()

    let inline checklte(l: 'l, r:'r  when 'l: (static member (+@<=): 'l ->'r -> LessThanOrEqual<'r>)) = ()

    let inline checkgt(l: 'l, r:'r  when 'l: (static member (+@>): 'l ->'r -> GreaterThan<'r>)) = ()

    let inline checkgte(l: 'l, r:'r  when 'l: (static member (+@>=): 'l ->'r -> GreaterThanOrEqual<'r>)) = ()

    let inline checkeq(l: 'l, r:'r  when 'l: (static member (+@==): 'l ->'r -> Equal<'r>)) = ()

    let inline checknoteq(l: 'l, r:'r  when 'l: (static member (+@!=): 'l ->'r -> NotEqual<'r>)) = ()
                








    



 


  
  

    


   



