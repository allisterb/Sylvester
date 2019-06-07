namespace Sylvester

[<AutoOpen>]
module Bool = 

    type Bool = interface end 
       
    type True() = interface Bool
    type False() = interface Bool

    let _true = True()
    let _false = False()
    
    type True with 
        static member inline (!!) (_:True) = _false 
        static member (*) (_:True, _:True) = _true
        static member (*) (_:True, _:False) = _false
        static member (+) (_:True, _:True) = _true
        static member (+) (_:True, _:False) = _true

        static member inline (<?>) (_:True, (x, _)) = x
      

    type False with 
        static member inline (!!) (_:False) = _true
        static member (*) (_:False, _:True) = _false
        static member (*) (_:False, _:False) = _false
        static member (+) (_:False, _:True) = _true
        static member (+) (_:False, _:False) = _false
        
        static member inline (<?>) (_:False, (_, y)) = y

    
        

    



    
 

    

    

   


