using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Data.Csv
{
    public class Field<T> : CsvField where T : struct
    {
        public Field(int index, string label, T defaultVal = default, T constant = default, string boolFalse = "", string boolTrue = "") :
            base(index, typeof(T), label, defaultVal, constant, boolFalse, boolTrue)
        {
            

        }
    }
}
