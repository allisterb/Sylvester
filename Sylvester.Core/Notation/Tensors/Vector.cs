namespace Sylvester.Notation
{
    public partial class Vector : Tensor
    {
        internal override Name DefaultNameBase => "V0";

        public int Length => Dimensions[0];

        protected Vector(string name) : base(name) { }

        public Vector(string name, int length) : base(name, length) {}

        public Vector(int length) : this(vn.V0, length)
        {
        }

        public Vector(string name, string indexName, out Index i, int length) : base(name, length)
        {
            i = new Index(null, 0, length, indexName);
        }

        public Vector(int length, out Index i) : this(vn.V0, "i", out i, length)
        {
        }
        
        public Vector(string name, out Index i, int length) : this(name, "i", out i, length)
        {
        }

        public Vector With(out Vector with)
        {
            GeneratorContext = GeneratorContext ?? (this, 1);
            with = new Vector(GenerateName(GeneratorContext.Value.index, Name), Dimensions[0]);
            GeneratorContext = (GeneratorContext.Value.tensor, GeneratorContext.Value.index + 1);
            return GeneratorContext.Value.tensor as Vector;
        }

        public Vector With(out Vector with, int length)
        {
            GeneratorContext = GeneratorContext ?? (this, 1);
            with = new Vector(GenerateName(GeneratorContext.Value.index, Name), length);
            GeneratorContext = (GeneratorContext.Value.tensor, GeneratorContext.Value.index + 1);
            return GeneratorContext.Value.tensor as Vector;
        }
    }
}