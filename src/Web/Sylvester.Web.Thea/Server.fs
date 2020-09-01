namespace Sylvester.Web.Thea

open FSharp.Quotations

open WebSharper
open Sylvester
open Sylvester.NLU.Wit

module Server =

    let private witai = new WitClient()

    [<Rpc>]
    let GetUser(user:string) : Async<User option> = async { return None } 

    [<Rpc>]
    let AddUser (user:string) : Async<unit Option> = async {return None }

    [<Rpc>]
    let UpdateUserLastLogin (user:string) : Async<unit> = async {return () }

    [<Rpc>]
    let GetMeaning input = 
        async {
            match! witai.GetMeaning input |> Async.AwaitTask |> Async.Catch with
            | Choice1Of2 o when not(isNull(o)) -> 
                let intents = o.Intents |> Seq.map (fun i -> Text.Intent'(i.Name, i.Confidence)) |> List.ofSeq
                let entities = 
                    o.Entities 
                    |> Seq.map (fun en -> en.Value) 
                    |> Seq.concat 
                    |> Seq.map (fun e -> Text.Entity'(e.Name, e.Confidence, e.Role, e.Value))
                    |> List.ofSeq
                return Text.Meaning'(intents, entities) |> Some
            | Choice2Of2 exn -> err "Could not get Wit.ai meaning for input '{0}'. Exception: {1}" [input; exn.Message]; return None
            | _ -> err "Could not get Wit.ai meaning for input '{0}'. Exception: {1}" [input]; return None
        }

    [<Rpc>]
    let Parse (input:string) =
        let P s = Parsers.parse<bool> input
        async {
            return P input
        }
