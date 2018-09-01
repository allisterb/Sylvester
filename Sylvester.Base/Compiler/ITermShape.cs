using System.Collections.Generic;

namespace Sylvester.Compiler
{
    public interface ITermShape : IShape, ITerm, IEnumerable<int>
    {
        ITermShape CloneShape(string namme);
    }
}