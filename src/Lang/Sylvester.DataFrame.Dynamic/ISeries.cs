using System;
using System.Dynamic;

namespace Sylvester
{
    public interface ISeries : IDynamicMetaObjectProvider
    {
        Type DataType { get; }

        Array _Data { get; }

        string Label { get; }

        int Length { get;  }
    }
}
