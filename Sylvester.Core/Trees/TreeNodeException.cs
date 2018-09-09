using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Trees
{
    public class TreeNodeException : Exception
    {
        public ITreeNode TreeNode { get; protected set; }

        public TreeNodeException(string message) : base(message) {}

        public TreeNodeException(TreeNode node, string message) : this(message)
        {
            TreeNode = node;
        }
    }
}
