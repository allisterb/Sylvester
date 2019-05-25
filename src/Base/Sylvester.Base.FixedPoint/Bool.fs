namespace Sylvester

[<AutoOpen>]
module Bool = 

    type IBool = interface end 
       
    type True() = interface IBool
    type False() = interface IBool

    let _true = True()
    let _false = False()
    
    type True with 
        static member inline (!!) (_:True) = _false 
        static member (*) (_:True, _:True) = _true
        static member (*) (_:True, _:False) = _false
        static member (+) (_:True, _:True) = _true
        static member (+) (_:True, _:False) = _true

    type False with
        static member inline (!!) (_:False) = _true
        static member (*) (_:False, _:True) = _false
        static member (*) (_:False, _:False) = _false
        static member (+) (_:False, _:True) = _true
        static member (+) (_:False, _:False) = _false


    

   


