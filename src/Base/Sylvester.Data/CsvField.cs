using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Data
{
    public class CsvField
    {
        public CsvField(int index, Type type, string label, dynamic defaultVal = default, dynamic constant = default, string boolFalse = "", string boolTrue = "")
        {
            if (!type.IsValueType && type.Name != "String")
            {
                throw new CsvFieldIsNotValueTypeException(type);
            }
            Index = index;
            Type = type;
            Label = label;
            DefaultVal = defaultVal;
            Constant = constant;
            BoolFalse = boolFalse;
            BoolTrue = boolTrue;
        }

        public Array Data { get; internal set; }

        public List<Array> BatchData { get; internal set; }

        public Action<int, string> ParseAction { get; internal set; }

        public int Index { get; set; }

        public Type Type
        {
            get => _type;
            set
            {
                _type = value;
                DefaultVal = GetDefaultValue(_type);
            }
        }

        public string Label { get; set; }

        public dynamic DefaultVal { get; set; }

        public dynamic Constant { get; set; }

        public string BoolFalse { get; set; }

        public string BoolTrue { get; set; }

        protected Type _type;

        protected dynamic GetDefaultValue(Type type)
        {
            switch (type.Name)
            {
                case "String":
                    return default(string);
                case "DateTime":
                    return default(DateTime);
                case "Byte":
                    return default(byte);
                case "SByte":
                    return default(sbyte);
                case "UInt16":
                    return default(ushort);
                case "Int16":
                    return default(short);
                case "UInt32":
                    return default(uint);
                case "Int32":
                    return default(int);
                case "UInt64":
                    return default(ulong);
                case "Int64":
                    return default(long);
                case "Single":
                    return default(float);
                case "Double":
                    return default(double);
                case "Decimal":
                    return default(decimal);
                case "Boolean":
                    return default(bool);
                default:
                    return null;
            }
        }
    }
}
