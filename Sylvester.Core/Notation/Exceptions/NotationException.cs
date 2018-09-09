using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Notation
{
    public class IndexNotationException : Exception
    {
        public Index Index { get; protected set; }

        public IndexNotationException(Index index, string message) : base(message)
        {
            Index = index;
        }
    }
}
