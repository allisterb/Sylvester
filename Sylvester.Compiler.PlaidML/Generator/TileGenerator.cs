using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Sylvester.Generator;
using Sylvester.Trees;

namespace Sylvester.Compiler.PlaidML.Generator
{
    public class TileGenerator : LanguageGenerator<TensorOp, TileWriter>
    {
        public override List<TensorOp> AssignmentOperators { get; } = new List<TensorOp>()
        {
            TensorOp.ElementWiseAssign, TensorOp.IndexedAssign
        };

        public override List<TensorOp> NestedBinaryOperators { get; } = new List<TensorOp>()
        {
            TensorOp.Mul, TensorOp.Add, TensorOp.Sub, TensorOp.Div, TensorOp.Square
        };

        public override List<TensorOp> ContractionOperators { get; } = new List<TensorOp>()
        {
            TensorOp.Sum, TensorOp.Product, TensorOp.Max, TensorOp.Min
        };

        public List<ITermShape> InputShapes => Tree.InputVariableNodes.Select(t => t.ValueAs<ITermShape>()).ToList();

        public Dictionary<ITreeValueNode, string> TensorDimensionVariables { get; protected set; }

        public string FunctionText { get; protected set; }

        public TileGenerator(IExpressionTree tree) : base(tree)
        {
            Context = new TileGeneratorContext(tree);
            Writer = new TileWriter();
            this.VisitTree();
        }

        public override void AfterVisitTree()
        {
            base.AfterVisitTree();
            WriteFunctionPrologue();
        }

        public override void VisitInternal(ITreeOperatorNode<TensorOp> on)
        {
            /* When the operand of a contraction op is not a tensor variable, synthesize a scalar variable
             * to represent the expression
            */
            if (TreeNodeIsContractionOp(on) && !(TreeNodeIsTensor(on.Left) || TreeNodeIsAssignmentOp(on.Left)))
            {
                base.Visit(on.Left);
                string lhs = GetNewVariableName("SYN");
                string rhs = (string)Context.Pop();
                AddElementwiseVariableDefinition(lhs, rhs, Writer.GetOperator(TensorOp.IndexedAssign, lhs, rhs));
                Context.Push(Writer.WriteOperator(on.Op, lhs + "[]"));
                return;
            }

            switch (on.Op)
            {
                case TensorOp.ElementWiseAssign:
                    base.VisitInternal(on);
                    string s = (string)Context.Pop();
                    string lhs = s.Split('=').First().TrimEnd(), rhs = s.Split('=').Last().TrimEnd();
                    AddElementwiseVariableDefinition(lhs, rhs, s);
                    Context.Push(lhs);
                    return;

                case TensorOp.IndexedAssign:
                    if (TreeNodeIsTensorValue(on.Right) || TreeNodeIsContractionOp(on.Right))
                    {
                        base.VisitInternal(on);
                        if (TreeNodeIsRoot(on))
                        {
                            return;
                        }
                        else
                        {
                            s = (string)Context.Pop();
                            lhs = s.Split('=').First().TrimEnd(); rhs = s.Split('=').Last().TrimEnd();
                            AddIndexedVariableDefinition(lhs, rhs, s);
                            Context.Push(lhs);
                            return;
                        }
                    }
                    else
                    {
                        var rhsOp = (ITreeOperatorNode<TensorOp>)on.Right;
                        if (TreeNodeIsContractionOp(rhsOp.Left) &&
                           (TreeNodeIsElementwiseOp(rhsOp.Right) || TreeNodeIsTensorValue(rhsOp.Right)))
                        {
                            string leftIndexVarName = on.Left.Left.Label.ToUpper();
                            Visit(on.Left);
                            string leftIndexVar = (string)Context.Pop();
                            string newLeftIndexVarName = GetNewVariableName("SYN" + leftIndexVarName);
                            string newLeftIndexVar = leftIndexVar.Replace(leftIndexVarName, newLeftIndexVarName);

                            Visit(rhsOp.Left);
                            string contractionOp = (string)Context.Pop();
                            s = string.Format(Writer.GetOperatorTemplate(TensorOp.IndexedAssign), newLeftIndexVar, contractionOp);
                            AddIndexedVariableDefinition(newLeftIndexVar, contractionOp, s);

                            Visit(rhsOp.Right);
                            string rightOp = string.Format(Writer.GetOperatorTemplate(rhsOp.Op), newLeftIndexVarName,
                                (string)Context.Pop());
                            s = string.Format(Writer.GetOperatorTemplate(TensorOp.ElementWiseAssign), leftIndexVarName, rightOp);
                            AddElementwiseVariableDefinition(leftIndexVarName, rightOp, s);
                            Context.Push(Writer.WriteOperator(TensorOp.ElementWiseAssign, leftIndexVarName, rightOp));
                        }
                        return;

                    }

                default:
                    base.VisitInternal(on);
                    return;
            }
        }

        public override void VisitLeaf(ITreeValueNode node)
        {
            if (!TreeNodeIsDimensionVariable(node))
            {
                base.VisitLeaf(node);
                return;
            }
            else
            {
                (ITreeValueNode parent, int dim) = GetDimensionVariableParent(node);
                if (Tree.InputVariableNodes.Contains(parent))
                {
                    base.VisitLeaf(node);
                    return;
                }
                else
                {
                    ITermShape parentShape = parent.ValueAs<ITermShape>();
                    ITermShape substituteParentShape =
                        InputShapes.FirstOrDefault(s => s.Dimensions.SequenceEqual(parentShape.Dimensions));
                    if (substituteParentShape == null)
                    {
                        throw new TileGeneratorException(this, "Could not find input variable node to substitute for "
                            + $"{parentShape.Label}.");
                    }
                    else
                    {
                        Context.Push(substituteParentShape.Label.ToUpper() + "DIM" + dim.ToString());
                    }
                }
            }

        }

        protected bool TreeNodeIsTensorValue(ITreeNode node) =>
            node is ITreeValueNode vn && vn.NodeType == ValueNodeType.TENSOR;

        protected bool TreeNodeIsTensor(ITreeNode node) =>
           TreeNodeIsIndexOp(node) || node is ITreeValueNode vn && vn.NodeType == ValueNodeType.TENSOR;

        protected bool TreeNodeIsIndexOp(ITreeNode node) =>
         node is ITreeOperatorNode<TensorOp> on && on.Op == TensorOp.Index;

        protected bool TreeNodeIsContractionOp(ITreeNode node) =>
          node is ITreeOperatorNode<TensorOp> on && ContractionOperators.Contains(on.Op);

        protected bool TreeNodeIsAssignmentOp(ITreeNode node) =>
            node is ITreeOperatorNode<TensorOp> on && ContractionOperators.Contains(on.Op);

        protected bool TreeNodeIsElementwiseOp(ITreeNode node) =>
          node is ITreeOperatorNode<TensorOp> on && !ContractionOperators.Contains(on.Op);

        protected bool TreeNodeIsRoot(ITreeNode node) => node.Id == 0;

        protected bool TreeOperatorNodeLHSIsTensor(ITreeOperatorNode<TensorOp> on)
        {
            if (on.Left is ITreeValueNode vn)
            {
                return vn.NodeType == ValueNodeType.TENSOR;
            }
            else if ((on.Left is ITreeOperatorNode<TensorOp> op)
                 && (op.Op == TensorOp.ElementWiseAssign || op.Op == TensorOp.IndexedAssign))
            {
                return TreeOperatorNodeLHSIsTensor(op);
            }
            else
            {
                return false;
            }
        }

        protected bool TreeNodeIsDimensionVariable(ITreeNode node) => Tree.TreeNodeIsDimensionVariable(node);

        protected (ITreeValueNode parent, int dim) GetDimensionVariableParent(ITreeValueNode node)
        {
            if (!TreeNodeIsDimensionVariable(node))
            {
                throw new ArgumentException($"The {node.Label} is not a dimension variable.");
            }
            ITermShape dim = node.ValueAs<ITermShape>();
            string[] p = dim.Label.Split(new[] { "DIM" }, StringSplitOptions.RemoveEmptyEntries);
            if (p.Length != 2 || !int.TryParse(p[1], out int idx))
            {
                throw new TileGeneratorException(this, $"Could not parse the dimension variable label {node.Label}");
            }
            ITreeValueNode parent =
                Tree.TensorNodes.First(shape => shape.Label == p[0]);
            return (parent, idx);
        }

        protected string WriteInputVariableDimensions(ITermShape input)
        {
            if (input.Rank == 0)
            {
                return "[]";
            }
            StringBuilder dimensions = new StringBuilder("[");
            dimensions.Append(Enumerable.Range(0, input.Rank)
                .Select(d => input.Label + "DIM" + d.ToString())
                .Aggregate((d1, d2) => d1 + "," + d2));
            dimensions.Append("]");
            return dimensions.ToString();
        }


        protected void WriteFunctionPrologue()
        {
            StringBuilder prologue = new StringBuilder("function(");
            foreach(ITermShape tensor in InputShapes)
            {
                prologue.AppendFormat("{0}{1}, ", tensor.Label.ToUpper(), WriteInputVariableDimensions(tensor).ToUpper());   
            }
            prologue.Remove(prologue.Length - 2, 2);
            prologue.Append(") -> ");
            if (!Tree.IndexSetNodes.Any(set => set.Parent.Label == Tree.OutputNode.Label))
            {
                prologue.AppendFormat("({0})", Tree.OutputNode.Label.ToUpper());
            }
            prologue.Append(" { ");
            FunctionText = prologue + this.Text + "}";
        }
    }
}

