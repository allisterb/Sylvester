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

        dynamic GetVal(int index);

        bool SetVal(int index, dynamic value);

        ISeries Append(params dynamic[] values);
    }
}
