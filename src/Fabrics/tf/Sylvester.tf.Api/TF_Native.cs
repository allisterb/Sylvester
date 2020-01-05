using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

using System.Linq;

namespace TensorFlow
{
    public abstract class TF_Native : IDisposable
    {
        public abstract void Dispose();
        public bool IsDeleted { get; protected set; }
    }
}
