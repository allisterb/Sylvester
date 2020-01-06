using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

using Google.Protobuf;
namespace TensorFlow
{
    public unsafe partial class TF_Buffer : TF_Native
    {
        #region Overriden members
        public override IntPtr NativePtr => this.__Instance;
        public override bool OwnsNativeMemory => this.__ownsNativeInstance;
        public override void Delete()
        {
            if (!this.__ownsNativeInstance)
            {
                c_api.TF_DeleteBuffer(this);
                this.__Instance = IntPtr.Zero;
            }
            else
            {
                this.Dispose();
            }
        }
        #endregion

        #region Operators
        public static explicit operator byte[](TF_Buffer b)
        {
            
            if (b.Length == 0UL)
            {
                return new byte[0];
            }
            else
            {
                byte[] arr = new byte[b.Length];
                
                Marshal.Copy(b.Data, arr, 0, checked(Convert.ToInt32(b.Length)));
                return arr;
            }
        }

        public static explicit operator UnmanagedMemoryStream(TF_Buffer b) => 
            new UnmanagedMemoryStream((byte *) b.Data.ToPointer(), Convert.ToInt64(b.Length));
        #endregion
    }
}
