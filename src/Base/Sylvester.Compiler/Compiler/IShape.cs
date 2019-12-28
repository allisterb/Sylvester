namespace Sylvester.Compiler
{
    public interface IShape
    {
        int[] Dimensions { get; }

        int[] Strides { get; }

        int Rank { get; }
    }
}