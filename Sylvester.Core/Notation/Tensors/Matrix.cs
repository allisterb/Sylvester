using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;

namespace Sylvester.Notation
{
    public partial class Matrix : Tensor
    {
        public Dimension Rows => Shape[0];

        public Dimension Columns => Shape[1];

        internal override Name DefaultNameBase => "A";

        protected Matrix(string name) : base(name) {}

        public Matrix(string name, int rows, int columns) : base(name, rows, columns) {}

        public Matrix(int rows, int columns) : this(mn.A, rows, columns) {}

        public Matrix(string name, int rows, int columns, string indexNameBase, out Index i, out Index j) : 
            base(name, rows, columns)
        {
            i = new Index(null, 0, rows, indexNameBase);
            j = new Index(null, 1, columns, this.GenerateName(1, indexNameBase));
        }

        public Matrix(int rows, int columns, out Index i, out Index j) 
            : this(mn.A, rows, columns, "i", out i, out j) {}

        public Matrix(string name, int rows, int columns, out Index i, out Index j) 
            : this(name, rows, columns, "i", out i, out j) {}


        public static TensorExpression operator *(Matrix left, Matrix right) => left.Multiply(right);

        public static TensorExpression operator *(Matrix left, Vector right) => left.Multiply(right);

        public override TensorExpression Multiply(Tensor right)
        {
            if (right is Matrix r)
            {
                if (this.Columns.Length == r.Rows.Length)
                {
                    return new TensorExpression(this * right, new Shape(this, new Dimension(this.Rows.Length, 0),
                        new Dimension(r.Columns.Length, 1)));
                }
                else
                {
                    throw new TensorExpressionException(this, 
                        "The number of columns of the LHS matrix does not match the number of RHS matrix rows.");
                }
            }
            else if (right is Vector vr)
            {
                if (this.Columns.Length == vr.Length)
                {
                    return new TensorExpression(base.Multiply(right), new Shape(this, new Dimension(vr.Length, 1)));
                }
                else
                {
                    throw new TensorExpressionException(this,
                        "The number of columns of the LHS matrix does not match the length of the RHS vector.");
                }
            }
            else
            {
                throw new TensorExpressionException(this, "The RHS tensor is not a matrix or a vector.");
            }
        }

        public Matrix With(out Matrix with)
        {
            GeneratorContext = GeneratorContext ?? (this, 1);
            with = new Matrix(GenerateName(GeneratorContext.Value.index, Name), Dimensions[0], Dimensions[1]);
            GeneratorContext = (GeneratorContext.Value.tensor, GeneratorContext.Value.index + 1);
            return GeneratorContext.Value.tensor as Matrix;
        }

        public Matrix With(out Matrix with, int rows, int columns)
        {
            GeneratorContext = GeneratorContext ?? (this, 1);
            with = new Matrix(GenerateName(GeneratorContext.Value.index, Name), rows, columns);
            GeneratorContext = (GeneratorContext.Value.tensor, GeneratorContext.Value.index + 1);
            return GeneratorContext.Value.tensor as Matrix;
        }
    }
}
