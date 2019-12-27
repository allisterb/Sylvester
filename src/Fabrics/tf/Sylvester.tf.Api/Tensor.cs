using System;
using System.Collections.Generic;
using System.Buffers;
using System.Text;
using System.Numerics.Tensors;

using TensorFlow;
using TensorFlow.Delegates;

namespace Sylvester.tf
{
    public unsafe class Tensor<T> : Api where T : struct, IComparable, IConvertible, IFormattable 
    {
        #region Constructors

        public Tensor(long[] dims) : base()
        {
            __Tensor = new DenseTensor<T>(dims.ToInt32());
            MemoryHandle = __Tensor.Buffer.Pin();
            Ptr = new IntPtr(MemoryHandle.Pointer);
            Dims = dims;
            NumDims = dims.Length;
            Length = Convert.ToUInt64(__Tensor.Length);
            DataType = GetDataType();
            Deallocate = DeallocateMethod;
            _Tensor = tf_tensor.TF_NewTensor(DataType, ref dims[0], NumDims, Ptr, Length, Deallocate, IntPtr.Zero) ?? 
                throw new Exception($"Could not create new TF_Tensor with data type {DataType.ToString()} and {NumDims} dimensions.");
            Initialized = _Tensor != null;
        }
        #endregion

        #region Properties
        public TF_Tensor _Tensor { get; }
        
        public DenseTensor<T> __Tensor { get; protected set; }

        public MemoryHandle MemoryHandle { get; }

        public IntPtr Ptr { get; }

        public ulong Length { get; }
        
        public int NumDims { get; }

        public long[] Dims { get; }

        public TF_DataType DataType { get; }

        protected Action_IntPtr_ulong_IntPtr Deallocate;
        #endregion

        #region Methods
        public void DeallocateMethod (IntPtr data, ulong len, IntPtr arg)
        {
            if (data != Ptr && len != Convert.ToUInt64(__Tensor.Length))
            {
                throw new InvalidOperationException("The pointer for deallocation is not the pinned handle pointer.");
            }
            else
            {
                MemoryHandle.Dispose();
                __Tensor = null;
                Initialized = false;
            }
        }
        
        public static TF_DataType GetDataType()
        {
            switch (typeof(T).Name)
            {
                case "System.Boolean": return TF_DataType.TF_BOOL;
                case "System.SByte": return TF_DataType.TF_INT8;
                case "System.Byte": return TF_DataType.TF_UINT8;
                case "System.Int16": return TF_DataType.TF_INT16;
                case "System.UInt16": return TF_DataType.TF_UINT16;
                case "System.Int32": return TF_DataType.TF_INT32;
                case "System.UInt32": return TF_DataType.TF_UINT32;
                case "System.Int64": return TF_DataType.TF_INT64;
                case "System.UInt64": return TF_DataType.TF_UINT64;
                case "System.Single": return TF_DataType.TF_FLOAT;
                case "System.Double": return TF_DataType.TF_DOUBLE;
                case "System.Numerics.Complex": return TF_DataType.TF_COMPLEX128;
                default: throw new ArgumentException($"Type {typeof(T).Name} is not a tensor numeric type.");

            }
        }
        #endregion
    }
}
