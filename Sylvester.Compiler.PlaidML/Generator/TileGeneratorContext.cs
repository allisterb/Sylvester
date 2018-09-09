using System;
using System.Collections.Generic;
using System.Text;

using Sylvester.Generator;
using Sylvester.Trees;

namespace Sylvester.Compiler.PlaidML.Generator
{
    public class TileGeneratorContext : LanguageGeneratorContext<TensorOp, TileWriter>
    {
        public TileGeneratorContext(IExpressionTree tree) : base(tree)
        {
         
        }
    }
}
