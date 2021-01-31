namespace Sylvester

open System
open System.IO
open System.Text
open FSharp.Quotations
open Antlr4.Runtime

open TPTPParser

module TPTP =
    type Parser(text:string) = 
        inherit tptp_v7_0_0_0Parser(new CommonTokenStream(new tptp_v7_0_0_0Lexer(new AntlrInputStream(text), new StringWriter(new StringBuilder()), new StringWriter(new StringBuilder()))))
        member x.GetOutput() = let sw = (x.Output :?> StringWriter) in sw.GetStringBuilder().ToString()
        member x.GetErrorOutput() = let sw = (x.ErrorOutput :?> StringWriter) in sw.GetStringBuilder().ToString()
        member x.FileContext = x.tptp_file()
    
    type Visitor() = 
        inherit TPTPVisitor<Expr>()
        let mutable expr = null

    
    let parse text = 
        let p = new Parser(text)    
        let v = new Visitor()
        p.tptp_input().Accept(v)

    let parse_file path = 
        let text = File.ReadAllText(path) in parse text
        
      
        //t
    

