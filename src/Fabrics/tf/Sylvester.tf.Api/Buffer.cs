using System;
using System.Collections.Generic;
using System.Buffers;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;

using TensorFlow.Delegates;

namespace TensorFlow
{
    public unsafe class Buffer
    {
        #region Constructors
        public Buffer(byte[] buffer)
        {
            Memory = new Memory<byte>(buffer);
            MemoryHandle = Memory.Pin();
            Ptr = new IntPtr(MemoryHandle.Pointer);
            Length = Convert.ToUInt64(buffer.LongLength);
            _Buffer = c_api.TF_NewBuffer() ?? throw new Exception("Could not create new TF_Buffer");
            _Buffer.Data = Ptr;
            _Buffer.Length = Length;
            _Buffer.DataDeallocator = DeallocateMethod;
            Initialized = true;
        }
        #endregion
        
        #region Properties
        public TF_Buffer _Buffer { get; }

        public Memory<byte> Memory { get; }

        public MemoryHandle MemoryHandle { get; }

        public IntPtr Ptr { get; }

        public ulong Length { get; }

        public bool Initialized { get; protected set; }
        #endregion

        #region Methods
        public void DeallocateMethod(IntPtr data, ulong len)
        {
            if (data != Ptr && len != Length)
            {
                throw new InvalidOperationException("The pointer for deallocation is not the pinned handle pointer.");
            }
            else
            {
                MemoryHandle.Dispose();
                this.Initialized = false;
            }
        }
        #endregion

        #region Operators
        public static implicit operator TF_Buffer(Buffer buffer)
        {
            if (!buffer.Initialized)
            {
                throw new InvalidOperationException("This buffer is not initialized.");
            }
            else
            {
                return buffer._Buffer;
            }
        }
        #endregion

    }
}
