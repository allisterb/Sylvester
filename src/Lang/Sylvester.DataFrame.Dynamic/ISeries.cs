using System;
using System.Dynamic;

namespace Sylvester
{
    public interface ISeries : IDynamicMetaObjectProvider
    {
        Type DataType { get; }

        string Label { get; }

        int Length { get;  }
    }
}
