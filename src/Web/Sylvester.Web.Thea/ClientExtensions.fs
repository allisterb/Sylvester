// Contains code from FsHtml: https://github.com/ptrelford/FsHtml by Phillip Trelford
// FsHtml is licensed under the Apache License 2.0: https://raw.githubusercontent.com/ptrelford/FsHtml/master/LICENSE 

namespace Sylvester.Web.Thea

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.JQuery

open SMApp.JQueryTerminal
open SMApp.WebSpeech

[<AutoOpen; JavaScript>]
module ClientExtensions =
    let rawOpt = EchoOptions(Raw=true)
    type Terminal with
        member x.Echo' (text:string) = x.Pause(); x.Echo text; x.Resume()
        member x.EchoHtml' (text:string) = x.Pause(); x.Echo(text, rawOpt) ; x.Resume()
      
    let toArray (a : ArrayLike<'t>) =
        JQuery.MakeArray a |> Array.map (fun a -> a :?> 't)

    let jserror = JQuery.JQuery.Error 

    let info = Console.Info
    
    let error = Console.Error

    let debug (loc:string) t = info <| sprintf "DEBUG: %s: %A" (loc.ToUpper()) t
    
    let toLower (s:string) = s.ToLower()

    let replace_tok (token:string) (value:string) (s:string) = s.Replace(token, value)

[<JavaScript>]
type _Html =
   | Elem of string * _Html list
   | Attr of string * string
   | Text of string
   with
   static member toString elem =
      let rec toString indent elem =
         let spaces = String.replicate indent " "
         match elem with
         | Attr(name,value) -> name+"=\""+value+"\""
         | Elem(tag, [Text s]) ->
            spaces+"<"+tag+">"+s+"</"+tag+">\r\n"
         | Elem(tag, content) ->
            let isAttr = function Attr _ -> true | _ -> false
            let attrs, elems = content |> List.partition isAttr
            let attrs =         
               if attrs = [] then ""
               else " " + String.concat " " [for attr in attrs -> toString 0 attr]
            match elems with
            | [] -> spaces+"<"+tag+attrs+"/>\r\n"
            | _ ->
               spaces+"<"+tag+attrs+">\r\n"+
                  String.concat "" [for e in elems -> toString (indent+1) e] +
                     spaces+"</"+tag+">\r\n"
         | Text(text) ->            
            spaces + text + "\r\n"
      toString 0 elem
   override this.ToString() = _Html.toString this

type htm = _Html

[<RequireQualifiedAccess; JavaScript>]
module htm = 
    let elem tag content = Elem(tag,content)
    let html = elem "html"
    let head = elem "head"
    let title = elem "title"
    let style = elem "style"
    let body = elem "body"
    let div = elem "div"
    let br = elem "br"
    let section = elem "section"
    let span = elem "span"
    let table = elem "table"
    let thead = elem "thead"
    let tbody = elem "tbody"
    let tfoot = elem "tfoot"
    let img = elem "img"
    let map = elem "map"
    let area = elem "area"
    let p = elem "p"
    let a = elem "a"
    let tr = elem "tr"
    let td = elem "td"
    let th = elem "th"
    let ul = elem "ul"
    let li = elem "li"
    let h1 = elem "h1"
    let h2 = elem "h1"
    let h3 = elem "h1"
    let h4 = elem "h1"
    let strong = elem "strong"
    let (~%) s = [Text(s.ToString())]
    let (%=) name value = Attr(name,value)
    let str h = _Html.toString h
    let cls c = Attr("class", c)
    let src c = Attr("src", c)
    let alt c = Attr("alt", c)