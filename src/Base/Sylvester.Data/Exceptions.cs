using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Data
{
    public class CsvFieldAlreadyExistsException : Exception
    {
        public CsvFieldAlreadyExistsException(int index, string label) : 
            base($"A Csv field at index {index} already exists: {label}.") {}
    }

    public class CsvFieldIsNotValueTypeException : Exception
    {
        public CsvFieldIsNotValueTypeException(Type type) : base($"The type {type.Name} is not a value type.")
        { }
    }
}
