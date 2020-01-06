using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

using System.Linq;

namespace TensorFlow
{
    public abstract class TF_Native 
    {
        public abstract IntPtr NativePtr { get; }

        public abstract bool OwnsNativeMemory { get; }

        public abstract void Delete();

        public bool IsDeleted => this.NativePtr == IntPtr.Zero;

    }
}
