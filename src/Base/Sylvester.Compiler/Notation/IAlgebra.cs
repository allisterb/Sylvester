namespace Sylvester.Notation
{
    public interface IAlgebra<TTerm, TResult> where TTerm : ITerm where TResult : ITerm
    {
        TResult Negate();
        TResult Add(TTerm right);
        TResult Subtract(TTerm right);
        TResult Multiply(TTerm right);
        TResult Divide(TTerm right);
    }
}