using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Threading.Tasks;

using CsvHelper;

namespace Sylvester.Data
{
    public class CsvFile : IEnumerable<CsvField>
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
                using (var stream = Path.StartsWith("http") ? HttpClient.GetStreamAsync(Path).Result :
                    File.OpenRead(Path))
                using (var reader = new StreamReader(stream))
                using (var parser = new CsvParser(reader))
                {
                    parser.Configuration.Delimiter = Delimiter;
                    header = parser.Read();
                }
                if (header == null || header.Length == 0)
                {
                    throw new ArgumentException($"The file {Path} does not contain valid CSV data.");
                }
               
                for (int i = 0; i < header.Length; i++)
                {
                    Fields.Add(new CsvField(i, typeof(string), header[i] != string.Empty ? header[i] : "Field" + i.ToString()));
                }
            }
        }

        static HttpClient HttpClient = new HttpClient();

        public string Path { get; }

        public string Delimiter { get; }

        public bool InferFieldNames { get; }

        public bool SkipHeader { get; }

        public List<CsvField> Fields { get; } = new List<CsvField>();

        public CsvField this[int i] => Fields[i];

        public CsvField this[string i] => Fields.Where(f => f.Label == i).First();

        public IEnumerator<CsvField> GetEnumerator()
        {
            for (int i = 0; i < Fields.Count; i++)
            {
                yield return Fields[i];
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            for (int i = 0; i < Fields.Count; i++)
            {
                yield return Fields[i];
            }
        }

        public byte[] ReadEntireFile() => File.ReadAllBytes(Path); 


        public CsvFile Add<T>(int index, string label, T defaultVal = default, T constant = default, string boolFalse = "", string boolTrue = "") where T : struct
        {
            if (Fields.Any(f => f.Index == index))
            {
                throw new CsvFieldAlreadyExistsException(index, Fields[index].Label);
            }
            if (!typeof(T).IsValueType && !(typeof(T).Name == "String"))
            {
                throw new CsvFieldIsNotValueTypeException(typeof(T));
            }
            Fields.Add(new CsvField(index, typeof(T), label, defaultVal, constant, boolFalse, boolTrue));
            return this;
        }
            
        public void Parse(int threads = 0)
        {
            byte[] fileData = Path.StartsWith("http") ? HttpClient.GetByteArrayAsync(Path).Result :
                File.ReadAllBytes(Path);
            List<string[]> rows = new List<string[]>(100000);
            string[] row = null;
            using (var stream = new MemoryStream(fileData))
            using (var reader = new StreamReader(stream))
            using (var parser = new CsvParser(reader))
            {
                parser.Configuration.Delimiter = Delimiter;
                if (InferFieldNames || SkipHeader)
                {
                    parser.Read();
                }
                while ((row = parser.Read()) != null)
                {
                    rows.Add(row);
                }
            }
            InitParser(rows.Count);
            ParseActions(rows);
        }

        public void Parse(int batchSize, int threads = 0)
        {
            foreach (var f in Fields)
            {
                f.BatchData = new List<Array>();
            }
            List<string[]> rows = new List<string[]>(batchSize);
            string[] row = null;
            using (Stream stream = Path.StartsWith("http") ? HttpClient.GetStreamAsync(Path).Result : File.OpenRead(Path))
            using (var reader = new StreamReader(stream))
            using (var parser = new CsvParser(reader))
            {
                parser.Configuration.Delimiter = Delimiter;
                if (InferFieldNames || SkipHeader)
                {
                    parser.Read();
                }
                int c = 0;
                int b = 0;
                while ((row = parser.Read()) != null)
                {
                    rows.Add(row);
                    c++;
                    b++;
                    if (b == batchSize)
                    {
                        InitBatchParser(b);
                        ParseActions(rows, threads);
                        rows.Clear();
                        b = 0;
                    }
                }
                if (b > 0)
                {
                    InitBatchParser(b);
                    ParseActions(rows, threads);
                    rows.Clear();
                    b = 0;
                }
            }

        }

        protected void InitParser(int length)
        {
            for (int i = 0; i < Fields.Count; i++)
            {
                if (Fields[i].Ignore)
                {
                    continue;
                }
                int i0 = i;
                switch (Fields[i].Type.Name)
                {
                    case "String":
                        var sdata = new string[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            sdata[index] = s == "" ? Fields[i0].DefaultVal : s;
                        };
                        Fields[i].Data = sdata; 
                        break;
                    case "DateTime":
                        var dtdata = new DateTime[length];
                        Fields[i].DefaultVal = default(DateTime);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            dtdata[index] = DateTime.TryParse(s, out DateTime dtv) ? dtv : Fields[i0].DefaultVal;
                        };
                        Fields[i].Data = dtdata;
                        break;
                    case "Byte":
                        var bdata = new Byte[length];
                        Fields[i].DefaultVal = default(byte);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            bdata[index] = Byte.TryParse(s, out byte bv) ? bv : Fields[i0].DefaultVal;
                        };
                        Fields[i].Data = bdata;
                        break;
                    case "SByte":
                        var sbdata = new SByte[length];
                        Fields[i].DefaultVal = default(sbyte);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            sbdata[index] = SByte.TryParse(s, out sbyte sbv) ? sbv : Fields[i0].DefaultVal;
                        };
                        Fields[i].Data = sbdata;
                        break;
                    case "UInt16":
                        var usdata = new ushort[length];
                        Fields[i].DefaultVal = default(UInt16);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            usdata[index] = ushort.TryParse(s, out ushort usv) ? usv : Fields[i0].DefaultVal;
                        };
                        Fields[i].Data = usdata;
                        break;
                    case "Int16":
                        var shdata = new short[length];
                        Fields[i].DefaultVal = default(Int16);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            shdata[index] = short.TryParse(s, out short shv) ? shv : Fields[i0].DefaultVal;
                        };
                        Fields[i].Data = shdata;
                        break;
                    case "UInt32":
                        var undata = new uint[length];
                        Fields[i].DefaultVal = default(UInt32);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            undata[index] = uint.TryParse(s, out uint unv) ? unv : Fields[i0].DefaultVal;
                        };
                        Fields[i].Data = undata;
                        break;
                    case "Int32":
                        var ndata = new int[length];
                        Fields[i].DefaultVal = default(Int32);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            ndata[index] = int.TryParse(s, out int nv) ? nv : Fields[i0].DefaultVal;
                        };
                        Fields[i].Data = ndata;
                        break;
                    case "UInt64":
                        var uldata = new ulong[length];
                        Fields[i].DefaultVal = default(UInt64);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            uldata[index] = ulong.TryParse(s, out ulong ulv) ? ulv : Fields[i0].DefaultVal;
                        };
                        Fields[i].Data = uldata;
                        break;
                    case "Int64":
                        var ldata = new long[length];
                        Fields[i].DefaultVal = default(Int64);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            ldata[index] = long.TryParse(s, out long lv) ? lv : Fields[i0].DefaultVal;
                        };
                        Fields[i].Data = ldata;
                        break;
                    case "Single":
                        var fdata = new float[length];
                        Fields[i].DefaultVal = default(float);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            fdata[index] = float.TryParse(s, out float fv) ? fv : Fields[i0].DefaultVal;
                        };
                        Fields[i].Data = fdata;
                        break;
                    case "Double":
                        var ddata = new double[length];
                        Fields[i].DefaultVal = default(double);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            ddata[index] = double.TryParse(s, out double ddv) ? ddv : Fields[i0].DefaultVal;

                        };
                        Fields[i].Data = ddata;
                        break;
                    case "Decimal":
                        var dedata = new decimal[length];
                        Fields[i].DefaultVal = default(decimal);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            dedata[index] = decimal.TryParse(s, out decimal dev) ? dev : Fields[i0].DefaultVal;
                        };
                        Fields[i].Data = dedata;
                        break;
                    case "Boolean":
                        var bodata = new bool[length];
                        Fields[i].DefaultVal = default(bool);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            bodata[index] = bool.TryParse(s, out bool bov) ? bov : Fields[i0].DefaultVal;
                        };
                        Fields[i].Data = bodata;
                        break;
                    default:
                        throw new NotImplementedException("Field type must be numeric, string, datetime, or boolean.");
                }
            }
        }

        protected void InitBatchParser(int length)
        {
            for (int i = 0; i < Fields.Count; i++)
            {
                if (Fields[i].Ignore)
                {
                    continue;
                }
                int i0 = i;
                switch (Fields[i].Type.Name)
                {
                    case "String":
                        var sdata = new string[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            sdata[index] = s == "" ? Fields[i0].DefaultVal : s;
                        };
                        Fields[i].BatchData.Add(sdata);
                        break;
                    case "DateTime":
                        var dtdata = new DateTime[length];
                        Fields[i].DefaultVal = default(DateTime);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            dtdata[index] = DateTime.TryParse(s, out DateTime dtv) ? dtv : Fields[i0].DefaultVal;
                        };
                        Fields[i].BatchData.Add(dtdata);
                        break;
                    case "Byte":
                        var bdata = new Byte[length];
                        Fields[i].DefaultVal = default(byte);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            bdata[index] = Byte.TryParse(s, out byte bv) ? bv : Fields[i0].DefaultVal;
                        };
                        Fields[i].BatchData.Add(bdata);
                        break;
                    case "SByte":
                        var sbdata = new SByte[length];
                        Fields[i].DefaultVal = default(sbyte);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            sbdata[index] = SByte.TryParse(s, out sbyte sbv) ? sbv : Fields[i0].DefaultVal;
                        };
                        Fields[i].BatchData.Add(sbdata);
                        break;
                    case "UInt16":
                        var usdata = new ushort[length];
                        Fields[i].DefaultVal = default(UInt16);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            usdata[index] = ushort.TryParse(s, out ushort usv) ? usv : Fields[i0].DefaultVal;
                        };
                        Fields[i].BatchData.Add(usdata);
                        break;
                    case "Int16":
                        var shdata = new short[length];
                        Fields[i].DefaultVal = default(Int16);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            shdata[index] = short.TryParse(s, out short shv) ? shv : Fields[i0].DefaultVal;
                        };
                        Fields[i].BatchData.Add(shdata);
                        break;
                    case "UInt32":
                        var undata = new uint[length];
                        Fields[i].DefaultVal = default(UInt32);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            undata[index] = uint.TryParse(s, out uint unv) ? unv : Fields[i0].DefaultVal;
                        };
                        Fields[i].BatchData.Add(undata);
                        break;
                    case "Int32":
                        var ndata = new int[length];
                        Fields[i].DefaultVal = default(Int32);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            ndata[index] = int.TryParse(s, out int nv) ? nv : Fields[i0].DefaultVal;
                        };
                        Fields[i].BatchData.Add(ndata);
                        break;
                    case "UInt64":
                        var uldata = new ulong[length];
                        Fields[i].DefaultVal = default(UInt64);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            uldata[index] = ulong.TryParse(s, out ulong ulv) ? ulv : Fields[i0].DefaultVal;
                        };
                        Fields[i].BatchData.Add(uldata);
                        break;
                    case "Int64":
                        var ldata = new long[length];
                        Fields[i].DefaultVal = default(Int64);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            ldata[index] = long.TryParse(s, out long lv) ? lv : Fields[i0].DefaultVal;
                        };
                        Fields[i].BatchData.Add(ldata);
                        break;
                    case "Single":
                        var fdata = new float[length];
                        Fields[i].DefaultVal = default(float);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            fdata[index] = float.TryParse(s, out float fv) ? fv : Fields[i0].DefaultVal;
                        };
                        Fields[i].BatchData.Add(fdata);
                        break;
                    case "Double":
                        var ddata = new double[length];
                        Fields[i].DefaultVal = default(double);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            ddata[index] = double.TryParse(s, out double ddv) ? ddv : Fields[i0].DefaultVal;

                        };
                        Fields[i].BatchData.Add(ddata);
                        break;
                    case "Decimal":
                        var dedata = new decimal[length];
                        Fields[i].DefaultVal = default(decimal);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            dedata[index] = decimal.TryParse(s, out decimal dev) ? dev : Fields[i0].DefaultVal;
                        };
                        Fields[i].BatchData.Add(dedata);
                        break;
                    case "Boolean":
                        var bodata = new bool[length];
                        Fields[i].DefaultVal = default(bool);
                        Fields[i].ParseAction = (index, s) =>
                        {
                            bodata[index] = bool.TryParse(s, out bool bov) ? bov : Fields[i0].DefaultVal;
                        };
                        Fields[i].BatchData.Add(bodata);
                        break;
                    default:
                        throw new NotImplementedException("Field type must be numeric, string, datetime, or boolean.");
                }
            }
        }

        protected void ParseActions(List<string[]> currentRows, int threads = 0)
        {
            Action<int, string>[] parseActions = new Action<int, string>[currentRows[0].Length];
            for (int i = 0; i < parseActions.Length; i++)
            {
                if (Fields.Any(f => f.Index == i))
                {
                    parseActions[i] = Fields[i].ParseAction;
                }

            }
            if (threads == 0)
            {
                for (int i = 0; i < currentRows.Count; i++)
                {
                    for (int j = 0; j < currentRows[i].Length; j++)
                    {
                        parseActions[j]?.Invoke(i, currentRows[i][j]);
                    }
                }
            }
            else
            {
                Parallel.For(0, currentRows.Count, new ParallelOptions() { MaxDegreeOfParallelism = threads }, (i, loopState) =>
                {
                    for (int j = 0; j < currentRows[i].Length; j++)
                    {
                        parseActions[j]?.Invoke(i, currentRows[i][j]);
                    }
                });
            }
        }
    }
}
