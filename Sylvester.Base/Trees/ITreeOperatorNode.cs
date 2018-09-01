namespace Sylvester.Trees
{
    public interface ITreeOperatorNode<TOp> : ITreeNode
    {
        TOp Op { get; }
    }
}