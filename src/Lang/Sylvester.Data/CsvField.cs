using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Data
{
    public class CsvField
    {
        public CsvField(int index, Type type, string label, dynamic defaultVal = default, dynamic constant = default, string boolFalse = "", string boolTrue = "")
        {
            Index = index;
            Type = type;
            Label = label;
            DefaultVal = defaultVal;
            Constant = constant;
            BoolFalse = boolFalse;
            BoolTrue = boolTrue;
        }

        public int Index { get; }

        public Type Type { get; }
        public string Label { get; set; }

        public dynamic DefaultVal { get; set; }

        public dynamic Constant { get; set; }

        public string BoolFalse { get; set; }

        public string BoolTrue { get; set; }
    }
}
