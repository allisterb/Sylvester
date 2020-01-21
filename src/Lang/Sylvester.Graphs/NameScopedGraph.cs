using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Satsuma;

namespace Sylvester
{
    public class NameScopedGraph : AbstractGraph
    {
        #region Constructors
        public NameScopedGraph() : base() {}
        public NameScopedGraph(IEnumerable<string> nodes) : base()
        {
            foreach(var n in nodes)
            {
                AddNode(n);
            }
        }
        #endregion

        #region Properties
        protected Dictionary<long, string> IdNameMap = new Dictionary<long, string>();
        protected Dictionary<string, long> NameIdMap = new Dictionary<string, long>();
        public IEnumerable<string> Names => NameIdMap.Keys;
        #endregion

        #region Methods
        public void AddNode(string name)
        {
            int id = name.GetHashCode();
            IdNameMap.Add(id, name);
            NameIdMap.Add(name, id);
            base.AddNode(id);
        }

        public NameScopedGraph SubGraph(string name) => 
            new NameScopedGraph(Names.Where(n => n == name));
        #endregion
    }
}
