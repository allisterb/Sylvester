using Sylvester.Trees;

namespace Sylvester.Generator
{
    public abstract class LanguageGeneratorContext<TOp, TWriter> : TreeVisitorContext<TOp, string, string>
    {
        public string CurrentText => Peek() as string;

        public LanguageGeneratorContext(IExpressionTree tree) : base(tree)
        {
        }
    }
}