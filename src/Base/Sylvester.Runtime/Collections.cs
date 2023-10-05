using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sylvester
{
    public static class Collections
    {
        public static Dictionary<string, object> AddAll(this Dictionary<string, object> d, Dictionary<string, object> a)
        {
            if (a != null)
            {
                foreach (var kv in a)
                {
                    d[kv.Key] = kv.Value;
                }
            }
            return d;
        }

        public static Dictionary<string, object> Replace(this Dictionary<string, object> d, Dictionary<string, object> a)
        {
            d.Clear();
            if (a != null)
            {
                foreach (var kv in a)
                {
                    d[kv.Key] = kv.Value;
                }
            }
            return d;
        }
    }
}
