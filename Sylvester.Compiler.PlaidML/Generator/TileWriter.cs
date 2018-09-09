using System;
using System.Collections.Generic;
using System.Text;

using N = Sylvester.Notation;
using Sylvester.Generator;
using Sylvester.Trees;

namespace Sylvester.Compiler.PlaidML.Generator
{
    public class TileWriter : LanguageWriter<TensorOp> 
    {
        protected override Dictionary<TensorOp, string> OperatorTemplate { get; } = new Dictionary<TensorOp, string>
        {
            { TensorOp.NoOp, string.Empty },
            { TensorOp.Assign, "{0} = {1};" },
            { TensorOp.ElementWiseAssign, "{0} = {1};" },
            { TensorOp.IndexedAssign, "{0} = {1};" },
            { TensorOp.Index, "{0}[{1}]" },

            { TensorOp.Add, "{0} + {1}" },
            { TensorOp.Mul, "{0} * {1}" },
            { TensorOp.Sub, "{0} - {1}" },
            { TensorOp.Div, "{0} / {1}" },

            { TensorOp.Square, "({0} * {0})" },
            { TensorOp.Sqrt, "sqrt({0})"},

            { TensorOp.Sum, "+({0})" },
            { TensorOp.Product, "*({0})" },
            { TensorOp.Max, ">({0})" },
            { TensorOp.Min, "<({0})" },

            
        };


        public override string WriteOperator(TensorOp op, string[] operands)
        {
            switch(op)
            {
                case TensorOp.Assign:
                case TensorOp.ElementWiseAssign:
                case TensorOp.IndexedAssign:
                    StringBuilder sb = new StringBuilder();
                    while (VariableDefinitions.Count > 0)
                    {
                        sb.Append(VariableDefinitions.Dequeue());
                    }
                    sb.Append(base.WriteOperator(op, operands));
                    return sb.ToString();

                default: return base.WriteOperator(op, operands);
            }
        }

        public override string WriteValueText(ITreeValueNode vn)
        {
            switch (vn.NodeType)
            {
                case ValueNodeType.TENSOR: return base.WriteValueText(vn).ToUpper();
                case ValueNodeType.INDEXSET: return base.WriteValueText(vn).ToLower();
                default: return base.WriteValueText(vn);
            }

        }

    }
}
