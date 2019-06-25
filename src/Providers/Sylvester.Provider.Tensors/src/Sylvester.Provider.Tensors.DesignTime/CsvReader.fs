namespace Sylvester.TEnsors

open System
open System.IO
open CsvHelper

module CsvReader =

    let read config path mapCsv =
            seq {
                use reader = new StreamReader(path = path)
                use csv = new CsvReader(reader, configuration = config)         

                csv.Read() |> ignore
                csv.ReadHeader() |> ignore

                while csv.Read() do
                    yield mapCsv csv          
            }
