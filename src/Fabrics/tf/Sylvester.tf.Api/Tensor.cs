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
            Length = Convert.ToUInt64(__Tensor.Length) * GetDataTypeByteSize();
            DataType = GetDataType();
            Deallocate = DeallocateMethod;
            _Tensor = tf_tensor.TF_NewTensor(DataType, ref dims[0], NumDims, Ptr, Length, Deallocate, IntPtr.Zero) ?? 
                throw new Exception($"Could not create new TF_Tensor with data type {DataType.ToString()} and {NumDims} dimensions.");
            Initialized = _Tensor != null;
        }

        public Tensor(int[] dims) : this(dims.ToInt64()) {}
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
        
        public static ulong GetDataTypeByteSize()
        {
            switch (typeof(T).Name)
            {
                case "Boolean": return 1L;
                case "SByte": return 1L;
                case "Byte": return 1L;
                case "Int16": return 2L;
                case "UInt16": return 2L;
                case "Int32": return 4L;
                case "UInt32": return 4L;
                case "Int64": return 8L;
                case "UInt64": return 8L;
                case "Single": return 4L;
                case "Double": return 8L;
                case "Complex": return 16L;
                default: throw new ArgumentException($"Type {typeof(T).Name} is not a tensor numeric type.");

            }
        }

        public static TF_DataType GetDataType()
        {
            switch (typeof(T).Name)
            {
                case "Boolean": return TF_DataType.TF_BOOL;
                case "SByte": return TF_DataType.TF_INT8;
                case "Byte": return TF_DataType.TF_UINT8;
                case "Int16": return TF_DataType.TF_INT16;
                case "UInt16": return TF_DataType.TF_UINT16;
                case "Int32": return TF_DataType.TF_INT32;
                case "UInt32": return TF_DataType.TF_UINT32;
                case "Int64": return TF_DataType.TF_INT64;
                case "UInt64": return TF_DataType.TF_UINT64;
                case "Single": return TF_DataType.TF_FLOAT;
                case "Double": return TF_DataType.TF_DOUBLE;
                case "Complex": return TF_DataType.TF_COMPLEX128;
                default: throw new ArgumentException($"Type {typeof(T).Name} is not a tensor numeric type.");

            }
        }
        #endregion

        #region Operators
        public static implicit operator TF_Tensor(Tensor<T> tensor)
        {
            if (!tensor.Initialized)
            {
                throw new InvalidOperationException("The tensor is not initialized.");
            }
            else
            {
                return tensor._Tensor;
            }
        }
        #endregion
    }
}
