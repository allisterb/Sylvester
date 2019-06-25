namespace Sylvester.Tensors.IO

open System
open System.Collections.Generic
open System.IO
open CsvHelper
open CsvHelper.Configuration

module FileData =

    let read config path mapCsv =
            seq {
                use reader = new StreamReader(path = path)
                use csv = new CsvReader(reader, configuration = config)         

                do if csv.Configuration.HasHeaderRecord then
                    csv.Read() |> ignore
                    csv.ReadHeader() |> ignore

                while csv.Read() do
                    yield mapCsv csv          
            }

    let readCsvSingleCol<'t> (path:string) skipHeader (col:int)  =
        let config = Configuration()
        config.HasHeaderRecord <- skipHeader
        read config path (fun (csv:CsvReader) -> csv.GetField<'t>(col))

    let readCsvCols<'t> (path:string) skipHeader (cols:seq<int>)  =
        let config = Configuration()
        config.HasHeaderRecord <- skipHeader
        read config path (fun (csv:CsvReader) ->  (Seq.map (fun (c:int) -> csv.GetField<'t>(c)) cols) |> Seq.toArray) 

    let readAllCsvCols<'t> (path:string) skipHeader = 
        
        let readRow (csv:CsvReader) = 
            let l = List<'t>()
            let mutable x = Unchecked.defaultof<'t>
            let mutable c = 0
            while csv.TryGetField<'t>(c, &x) do
                l.Add(x)
                c <- c + 1
            l.ToArray()

        let config = Configuration()
        config.HasHeaderRecord <- skipHeader
        read config path readRow