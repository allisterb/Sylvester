namespace Sylvester

open System
open ExpectNet

module Expect =

    let send_line (s:Session) line = s.Send.Line line
    
    let private default_expect_params timeout_ms retries= 
           let t = defaultArg timeout_ms (Nullable<int>())
           let r = defaultArg retries (Nullable<int>())
           t,r

    let starts_with (s:Session) (q:string) (timeout_ms:Nullable<int> option) (retries: Nullable<int> option) = 
        let p = default_expect_params timeout_ms retries
        s.Expect.StartsWith(q, fst p, snd p)

    let contains (s:Session) (q:string) (timeout_ms:Nullable<int> option) (retries: Nullable<int> option) = 
        let p = default_expect_params timeout_ms retries
        s.Expect.Contains(q, fst p, snd p)

    let is_match (m:IResult)  = m.IsMatch
