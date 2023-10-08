using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sylvester
{
    public static class Strings
    {
        public static string Delete(this string s, string r) => s.Replace(r, string.Empty);
    }
}
