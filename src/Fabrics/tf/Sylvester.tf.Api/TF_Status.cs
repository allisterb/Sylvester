using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

using System.Linq;

using Google.Protobuf;
using Tensorflow;

namespace TensorFlow
{
    public unsafe partial class TF_Status : TF_Native
    {
        #region Properties

        #endregion

        #region Overriden members
        public override IntPtr NativePtr => this.__Instance;
        public override bool OwnsNativeMemory => this.__ownsNativeInstance;
        public override void Delete()
        {
            if (!this.__ownsNativeInstance)
            {
                tf_status.TF_DeleteStatus(this);
                this.__Instance = IntPtr.Zero;
            }
        }
        #endregion

        #region Methods


        #endregion
    }
}
