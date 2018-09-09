using System;
using Sylvester.Notation;
using Sawmill;

namespace Sylvester.Trees
{
    public class ValueNode : TreeNode, ITreeValueNode
    {
        public ValueNodeType NodeType { get; internal set; }

        public object Value { get; internal set; }

        public override string Label
        {
            get
            {
                switch (Value)
                {
                    case Term t: return t.Name;
                    case null: return "[]";
                    default: throw new NotSupportedException($"Unknown value: {Value}.");
                }
            }
        }

        internal ValueNode(int id, int? parentId, TreeNodePosition pos, object value) : base(id, parentId, pos)
        {
            switch (value)
            {
                case Tensor _:
                    NodeType = ValueNodeType.TENSOR;
                    break;

                case IndexSet _:
                    NodeType = ValueNodeType.INDEXSET;
                    break;
                case null:
                    NodeType = ValueNodeType.VARIABLE;
                    break;

                default:
                    throw new NotSupportedException($"Unknown value type: {value.GetType().Name}.");
            }

            Value = value;
        }


        public override Children<ITreeNode> GetChildren() => Children.None<ITreeNode>();

        public override ITreeNode SetChildren(Children<ITreeNode> newChildren) => this;

        public T ValueAs<T>() where T : class
        {
            return (T)Value;
        }
    }
}