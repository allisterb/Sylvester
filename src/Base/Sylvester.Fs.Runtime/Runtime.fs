namespace Sylvester

open System

open ExpectNet

[<AutoOpen>]
module FsRuntime =
     
    /// Result of a runtime operation.
    type Result<'TSuccess,'TFailure> = 
        | Success of 'TSuccess
        | Failure of 'TFailure
        with member x.Res with get() = match x with | Success s -> s | Failure f -> failwith "This operation failed."

    let info mt args = Runtime.Info(mt, List.toArray args)

    let debug mt args = Runtime.Debug(mt, List.toArray args)

    let err mt args = Runtime.Error(mt, List.toArray args)

    let errex mt args = Runtime.Error(mt, List.toArray args)

    let (|Default|) defaultValue input =    
        defaultArg input defaultValue
    
    let tryCatch f x =
        try
            f x |> Success
        with
        | ex -> 
            err "The runtime operation failed." [ex]
            Failure ex

    let tryCatch' f x =
        try
            f x 
        with
        | ex -> 
            err "The runtime operation failed." [ex]
            Failure ex

    let tryCatchAsync' f x = tryCatch' Async.RunSynchronously << f <| x
    
    let bind f = 
        function
        | Success value -> tryCatch f value
        | Failure failure -> Failure failure

    let bind' f = 
        function
        | Success value -> tryCatch' f value
        | Failure failure -> Failure failure
    
    let bind'' f = 
        function
        | Success value -> f value |> Success
        | Failure failure -> Failure failure

    let test = 
        function
        | Success _ -> true
        | Failure _ -> false

    let succ = 
        function
        | Success value -> value
        | Failure failure -> failwith "This operation failed."

    let init (r: 'T when 'T :> Runtime) = if r.Initialized then r else failwith "This runtime object is not initialized."

    let init' (r: 'T when 'T :> Runtime) = if r.Initialized then Success r else Failure (exn "This runtime object is not initialized.")

    let switch' res f = 
        match res with
        | Success _ -> tryCatch' f
        | Failure failure -> fun _ -> Failure failure
    
    let switchAsync' res f =
        match res with
        | Success _ -> tryCatchAsync' f
        | Failure failure -> fun _ -> Failure failure

    let try' f x = tryCatch' f x

    let try'' f x = tryCatchAsync' f x

    let (!>) f = tryCatch f

    let (!>>) f = tryCatch' f   
    
    let (!>>>) f = tryCatchAsync' f
    
    let (|>>) res f = switch' res f

    let (|>>>) res f = switchAsync' res f

    let (>>|) r f = r |> init' |> switch' <| f
    
    let (>>>|) r f = r |> init' |> switchAsync' <| f

    let (>>=) f1 f2  = bind f2 f1 

    let (>>>=) f1 f2  = bind' f2 f1
    
    let (>>@=) f1 f2  = bind'' f2 f1

    let (<<@=) f1 f2  = bind'' f1 f2

    let (>=>) f1 f2 = tryCatch' f1 >> (bind' f2)

    type NullCoalesce =  
        static member Coalesce(a: 'a option, b: 'a Lazy) = match a with Some a -> a | _ -> b.Value
        static member Coalesce(a: 'a Nullable, b: 'a Lazy) = if a.HasValue then a.Value else b.Value
        static member Coalesce(a: 'a when 'a:null, b: 'a Lazy) = match a with null -> b.Value | _ -> a
    
    let inline nullCoalesceHelper< ^t, ^a, ^b, ^c when (^t or ^a) : (static member Coalesce : ^a * ^b -> ^c)> a b = 
        ((^t or ^a) : (static member Coalesce : ^a * ^b -> ^c) (a, b))
   
    let inline (|??) a b = nullCoalesceHelper<NullCoalesce, _, _, _> a b

    // Strings
    let empty (s:string) = String.IsNullOrEmpty s