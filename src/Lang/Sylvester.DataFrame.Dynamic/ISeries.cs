using System;
using System.Collections;
using System.Dynamic;

namespace Sylvester
{
    public interface ISeries : IEnumerable, IDynamicMetaObjectProvider
    {
        Type DataType { get; }

        string Label { get; }

        int Length { get;  }

        dynamic DefaultVal { get; }
        dynamic GetVal(int index);
 
        bool SetVal(int index, dynamic value);

        IBackend Backend { get; set; }

        ISeries Clone(string label);

        ISeries Append(params dynamic[] values);
    }
}
