using System.Collections.Generic;

namespace Sylvester.Compiler
{
    public interface ITensorContext
    {
        List<INDArray> Tensors { get; }
    }
}