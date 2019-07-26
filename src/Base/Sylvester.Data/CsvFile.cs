using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
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

        public HashSet<int> FieldIndices => new HashSet<int>(Fields.Select(f => f.Index));

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
            byte[] fileData = File.ReadAllBytes(Path);
            List<string[]> rows = new List<string[]>(100000);
            string[] row = null;
            using (var stream = new MemoryStream(fileData))
            using (var reader = new StreamReader(stream))
            using (var parser = new CsvParser(reader))
            {
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
            Action<int, string>[] parseActions = new Action<int, string>[rows[0].Length];
            for (int i = 0; i < parseActions.Length; i++)
            {
                if (Fields.Any(f => f.Index == i))
                {
                    parseActions[i] = Fields[i].ParseAction;
                }
                
            }
            if (threads == 0)
            {
                for (int i = 0; i < rows.Count; i++)
                {
                    for (int j = 0; j < rows[i].Length; j++)
                    {
                        if (parseActions[j] != null)
                        {
                            parseActions[j](i, rows[i][j]);
                        }
                    }
                }
            }
        }

        protected void InitParser(int length)
        {
            for (int i = 0; i < Fields.Count; i++)
            {
                switch(Fields[i].Type.Name)
                {
                    case "String":
                        var sdata = new string[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            sdata[index] = s == "" ? Fields[i].DefaultVal : s;
                        };
                        Fields[i].Data = sdata; 
                        break;
                    case "DateTime":
                        var dtdata = new DateTime[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            dtdata[index] = DateTime.TryParse(s, out DateTime dtv) ? dtv : Fields[i].DefaultVal;
                        };
                        Fields[i].Data = dtdata;
                        break;
                    case "Byte":
                        var bdata = new Byte[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            bdata[index] = Byte.TryParse(s, out byte bv) ? bv : Fields[i].DefaultVal;
                        };
                        Fields[i].Data = bdata;
                        break;
                    case "SByte":
                        var sbdata = new SByte[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            sbdata[index] = SByte.TryParse(s, out sbyte sbv) ? sbv : Fields[i].DefaultVal;
                        };
                        Fields[i].Data = sbdata;
                        break;
                    case "UInt16":
                        var usdata = new ushort[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            usdata[index] = ushort.TryParse(s, out ushort usv) ? usv : Fields[i].DefaultVal;
                        };
                        Fields[i].Data = usdata;
                        break;
                    case "Int16":
                        var shdata = new short[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            shdata[index] = short.TryParse(s, out short shv) ? shv : Fields[i].DefaultVal;
                        };
                        Fields[i].Data = shdata;
                        break;
                    case "UInt32":
                        var undata = new uint[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            undata[index] = uint.TryParse(s, out uint unv) ? unv : Fields[i].DefaultVal;
                        };
                        Fields[i].Data = undata;
                        break;
                    case "Int32":
                        var ndata = new int[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            ndata[index] = int.TryParse(s, out int nv) ? nv : Fields[i].DefaultVal;
                        };
                        Fields[i].Data = ndata;
                        break;
                    case "UInt64":
                        var uldata = new ulong[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            uldata[index] = ulong.TryParse(s, out ulong ulv) ? ulv : Fields[i].DefaultVal;
                        };
                        Fields[i].Data = uldata;
                        break;
                    case "Int64":
                        var ldata = new long[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            ldata[index] = long.TryParse(s, out long lv) ? lv : Fields[i].DefaultVal;
                        };
                        Fields[i].Data = ldata;
                        break;
                    case "Single":
                        var fdata = new float[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            fdata[index] = float.TryParse(s, out float fv) ? fv : Fields[i].DefaultVal;
                        };
                        Fields[i].Data = fdata;
                        break;
                    case "Double":
                        var ddata = new double[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            ddata[index] = double.TryParse(s, out double ddv) ? ddv : Fields[i].DefaultVal;

                        };
                        Fields[i].Data = ddata;
                        break;
                    case "Decimal":
                        var dedata = new decimal[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            dedata[index] = decimal.TryParse(s, out decimal dev) ? dev : Fields[i].DefaultVal;
                        };
                        Fields[i].Data = dedata;
                        break;
                    case "Boolean":
                        var bodata = new bool[length];
                        Fields[i].ParseAction = (index, s) =>
                        {
                            bodata[index] = bool.TryParse(s, out bool bov) ? bov : Fields[i].DefaultVal;
                        };
                        Fields[i].Data = bodata;
                        break;
                    default:
                        throw new NotImplementedException("Field type must be numeric, string, datetime, or boolean.");
                }
            }
        }
    }   
}
