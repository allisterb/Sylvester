using System.Linq.Expressions;

namespace Sylvester.Notation
{
    public partial class Scalar : Tensor
    {
        public object Value { get; protected set; }

        public bool IsDimensionVariable { get; protected set; }

        internal override Name DefaultNameBase => "a";


        public Scalar(string name, bool isDimensionVariable = false) : base(name, new int[0])
        {
            this.IsDimensionVariable = isDimensionVariable;
        }

        public Scalar() : this("a") {}

        public Scalar(string name, TensorIndexExpression expr) : this(name)
        {
            this.ContractionDefinition = (null, new TensorContraction(expr, this));
        }


        public Scalar With(out Scalar with)
        {
            int nameGeneratorStartIndex = 1;
            // TODO: [vermorel]  '1' magic number needs clarification.
            // REMARK: [allisterb] Use nameGeneratorStartIndex variable.
            GeneratorContext = GeneratorContext ?? (this, nameGeneratorStartIndex); 
            with = new Scalar(GenerateName(GeneratorContext.Value.index, Name));
            GeneratorContext = (GeneratorContext.Value.tensor, GeneratorContext.Value.index + 1);
            return GeneratorContext.Value.tensor as Scalar;
        }
    }
}