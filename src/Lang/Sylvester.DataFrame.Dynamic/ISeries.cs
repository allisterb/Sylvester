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
    }
}
