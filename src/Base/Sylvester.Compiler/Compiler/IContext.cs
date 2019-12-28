using System.Collections.Generic;

namespace Sylvester.Compiler
{
    public interface IContext
    {
        List<INDArray> Tensors { get; }
    }
}