using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester
{
    public class ApiNotInitializedException : Exception
    {
        public ApiNotInitializedException(Api api) : base($"The {api.GetType().Name} Api is not initialized.") {}
    }
}
