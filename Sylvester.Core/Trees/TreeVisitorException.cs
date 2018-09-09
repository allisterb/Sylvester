using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Trees
{
    public class TreeVisitorException<TOp, TInternalContext, TLeafContext> : Exception
        where TInternalContext : class where TLeafContext : class
    {
        public TreeVisitor<TOp, TInternalContext, TLeafContext> TreeVisitor { get; protected set; }

        public TreeVisitorException(TreeVisitor<TOp, TInternalContext, TLeafContext> visitor, string message) : base(message)
        {
            TreeVisitor = visitor;
        }

    }
}
