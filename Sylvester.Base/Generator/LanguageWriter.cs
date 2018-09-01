using System;
using System.Collections.Generic;
using System.Linq;
using Sylvester.Trees;

namespace Sylvester.Generator
{
    public abstract class LanguageWriter<TOp>
    {
        public Queue<string> VariableDefinitions { get; } = new Queue<string>();

        protected Dictionary<string, object> Options { get; }

        protected abstract Dictionary<TOp, string> OperatorTemplate { get; }


        protected LanguageWriter(Dictionary<string, object> options = null)
        {
            if (options != null)
            {
                Options = options;
            }
        }

        public virtual string WriteValueText(ITreeValueNode vn)
        {
            switch (vn.NodeType)
            {
                case ValueNodeType.TENSOR:
                    return vn.Label;
                case ValueNodeType.VARIABLE:
                    return string.Empty;
                case ValueNodeType.INDEXSET:
                    IEnumerable<ITerm> indices = vn.ValueAs<IEnumerable<ITerm>>();
                    return indices.Select(i => i.Label).Aggregate((a, b) => a + ", " + b);
                default: throw new Exception($"Unknown value type: {vn.NodeType.ToString()}.");
            }
        }

        public virtual string WriteOperator(TOp op, params string[] operands)
        {
            return string.Format(OperatorTemplate[op], operands);
        }

        public virtual string GetOperatorTemplate(ITreeOperatorNode<TOp> on) => OperatorTemplate[on.Op];

        public virtual string GetOperatorTemplate(TOp op) => OperatorTemplate[op];


        public string GetOperator(TOp op, params string[] operands)
        {
            return string.Format(OperatorTemplate[op], operands);
        }
    }
}