using System;
using System.Collections.Generic;
using System.IO;
using CsvHelper;

namespace Sylvester.Data
{
    public class CsvFile
    {
        public CsvFile(string path, string delimiter = ",", bool inferFieldNames = true, bool skipHeader = false)
        {
            Path = path ?? throw new ArgumentNullException("path");
            if (inferFieldNames && skipHeader) throw new ArgumentException("Cannot skip heeader and infer field names simultaneously.");
            Delimiter = delimiter;
            InferFieldNames = inferFieldNames;
            SkipHeader = skipHeader;
            
            if (InferFieldNames)
            {
                string[] header = null;
                using (var stream = File.OpenRead(Path))
                using (var reader = new StreamReader(stream))
                using (var parser = new CsvParser(reader))
                {
                 
                    header = parser.Read();
                }
                if (header == null || header.Length == 0)
                {
                    throw new ArgumentException($"The file {Path} does not contain a valid CSV header.");
                }
               
                for (int i = 0; i < header.Length; i++)
                {
                    Fields.Add(new CsvField(i, typeof(string), header[i] != string.Empty ? header[i] : "Field" + i.ToString()));
                }
            }
        }

        public string Path { get; }

        public string Delimiter { get; }

        public bool InferFieldNames { get; }

        public bool SkipHeader { get; }

        public List<CsvField> Fields { get; } = new List<CsvField>();

        public byte[] ReadEntireFile() => File.ReadAllBytes(Path); 
        

        public List<string[]> Parse(byte[] fileData)
        {
            using(var stream = new MemoryStream(fileData))
            using (var reader = new StreamReader(stream))
            using (var parser = new CsvParser(reader))
            {
                List<string[]> rows = new List<string[]>(100000);
                string[] row;
                if (InferFieldNames || SkipHeader)
                {
                    parser.Read();
                }
                while ((row = parser.Read()) != null)
                {
                    rows.Add(row);
                }
                return rows;
            }
        }
    }

    

   
}
