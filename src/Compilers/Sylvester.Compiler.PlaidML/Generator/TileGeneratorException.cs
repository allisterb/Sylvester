using System;
using System.Collections.Generic;
using System.Text;
using Sylvester.Trees;
using Sylvester.Generator;

namespace Sylvester.Compiler.PlaidML.Generator
{
    public class TileGeneratorException : LanguageGeneratorException<TensorOp, TileWriter>
    {
        public TileGeneratorException(TileGenerator gen, string message) : base(gen, message) {}
    }
}
