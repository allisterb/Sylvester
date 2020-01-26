using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester
{
    public class NotInitializedException : Exception
    {
        public NotInitializedException(Runtime api) : base($"The runtime {api.GetType().Name} object is not initialized.") {}
    }
}
