namespace Sylvester.Notation
{
    public static partial class Operators
    {
        public static ContractionOperator SUM { get; } = new ContractionOperator((expr) => Math.Sum(expr));

        public static ContractionOperator MEAN { get; } = new ContractionOperator((expr) => Math.Mean(expr));

        public static UnaryOperator<TensorExpression, TensorExpression> SQUARE { get; } = 
            new UnaryOperator<TensorExpression, TensorExpression>((expr) => Math.Square(expr));

        public static UnaryOperator<TensorExpression, TensorExpression> SQRT { get; } = 
            new UnaryOperator<TensorExpression, TensorExpression>((expr) => Math.Sqrt(expr));
    }
}