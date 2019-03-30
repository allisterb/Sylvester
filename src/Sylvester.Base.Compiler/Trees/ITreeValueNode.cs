namespace Sylvester.Trees
{
    public interface ITreeValueNode : ITreeNode
    {
        ValueNodeType NodeType { get; }
        object Value { get; }
        T ValueAs<T>() where T : class;
    }
}