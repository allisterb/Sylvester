using System;

namespace Sylvester
{
    public interface ISeries
    {
        Type DataType { get; }

        Array _Data { get; }

        string Label { get; }

        int Length { get;  }
    }
}
