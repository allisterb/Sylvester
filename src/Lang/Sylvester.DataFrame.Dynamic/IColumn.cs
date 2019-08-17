using System;
using System.Collections;
using System.Collections.Generic;
using System.Dynamic;

namespace Sylvester.Data
{
    public interface IColumn : IEnumerable, IDynamicMetaObjectProvider
    {
        Type DataType { get; }

        string Label { get; }

        int Length { get;  }

        dynamic DefaultVal { get; }

        dynamic GetVal(int index);
 
        bool SetVal(int index, dynamic value);

        Dictionary<string, object> Attrs { get; }

        IBackend Backend { get; set; }

        IColumn Clone(string label);

        IColumn Append(params dynamic[] values);
    }
}
