using System;

namespace Sylvester.Compiler
{
    public interface INDArray
    {
        string Name { get; }

        int NDim { get; }

        int[] DeviceBufferShape { get; }

        int ElementCount { get; }

        Type DType { get; }

        int ItemSize { get; }

        INDArray Zeros();

        INDArray Ones();

        INDArray Random();
    }
}