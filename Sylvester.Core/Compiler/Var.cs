using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using Sylvester.Compiler;
using Sylvester.Math;
using Sylvester.Notation;

namespace Sylvester
{
    /// <summary>
    /// Bind a bind a tensor declaration to an array stored in memory. 
    /// </summary>
    /// <typeparam name="T">Numeric type of the tensor.</typeparam>
    public class Var<T> : IVariable<T>, IDisposable where T : unmanaged, IEquatable<T>, IComparable<T>, IConvertible
    {
        public Tensor Tensor { get; internal set; }

        public string Name => Tensor.Name;

        public int[] Dimensions => Tensor.Dimensions;

        public int[] Strides => Tensor.Strides;

        public int Rank => Tensor.Rank;

        public int ElementCount => Tensor.ElementCount;

        public bool Initialized { get; protected set; }

        public MemoryHandle MemoryHandle { get; protected set; }

        public int Pins { get; protected set; }

        public bool IsPinned => Pins > 0;

        public unsafe Span<T> Span
        {
            get
            {
                ThrowIfNotInitialized();
                ThrowIfHandleIsNull();
                return new Span<T>(MemoryHandle.Pointer, Tensor.ElementCount);
            }
        }

        internal Var(Tensor tensor)
        {
            Tensor = tensor;
        }

        internal Var(Tensor tensor, params T[] data) : this(tensor)
        {
            if (data.Length == 0)
            {
                throw new ArgumentException($"Zero data elements specified.");
            }
            else if (tensor != null && tensor.ElementCount != data.Length)
            {
                throw new ArgumentException($"The number of data elements specified ({data.Length}) "
                    + $"does not mach the number of elements in tensor {tensor.Label} : {tensor.ElementCount}.");
            }

            Tensor = tensor;
            MemoryHandle = new Memory<T>(data).Pin();
            Initialized = true;
        }

        /// <summary>
        /// Using Array allows to pass in multi-dimensional arrays like int[,,] in the constructor.
        /// All specialized .NET array types are convertible to Array. The type and dimensions
        /// of the array are checked to match the dimensions of the Tensor that the Var is bound to.
        /// </summary>
        internal unsafe Var(Tensor tensor, Array array) : this(tensor)
        {
            var zeroIndex = new int[array.Rank];
            var zeroElement = array.GetValue(zeroIndex);
            if (!(zeroElement is T))
            {
                throw new ArgumentException($"The array must have type {typeof(T).Name} to initialize this variable.");
            }
            if (array.Rank != tensor.Rank)
            {
                throw new ArgumentException($"The array rank is {array.Rank} but the tensor rank is {tensor.Rank}.");
            }
            for (int i = 0; i < array.Rank; i++)
            {
                int dim = array.GetLowerBound(i) == 0 ? array.GetUpperBound(i) + 1 : 
                    array.GetUpperBound(i) - array.GetLowerBound(i);
                if ( dim != tensor.Dimensions[i])
                {
                    throw new ArgumentException($"The array dimension {i} has size {dim} but tensor dimension {i} " + 
                        $"has size {tensor.Dimensions[i]}.");
                }
            }
          
            GCHandle h = GCHandle.Alloc(array, GCHandleType.Pinned);
            if (!h.IsAllocated)
            {
                // TODO: [vermorel] Do not throw naked 'Exception', use subtype.
                // REMARK: [allisterb] Throw OutOfMemoryException
                throw new OutOfMemoryException("Could not allocate GCHandle for array.");
            }
            MemoryHandle = new MemoryHandle(h.AddrOfPinnedObject().ToPointer(), h, this);
            Pins = 1;
            Initialized = true;
        }

        internal Var(Tensor tensor, Memory<T> memory) : this(tensor)
        {
            MemoryHandle = memory.Pin();
            Pins = 1;
            Initialized = true;
        }

        public ref T this[int index]
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get
            {
                ThrowIfNotInitialized();
                ThrowIfHandleIsNull();
                ThrowIfIndexOutOfRange(index);
                return ref Read(index);
            }
        }

        public static implicit operator Var<T>(T[] array) => new Var<T>(null, array);

        public static implicit operator Var<T>(Array array) => new Var<T>(null, array);

        public static implicit operator Var<T>(T c) => new Var<T>(null, c);

        public static implicit operator T (Var<T> v)
        {
            if (!v.Initialized)
            {
                throw new InvalidOperationException($"The variable {v.Name} is not initialized");
            }

            if (v.ElementCount != 1)
            {
                throw new InvalidCastException($"The variable {v.Name} has {v.ElementCount} elements.");
            }

            return v[0];
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public unsafe ref T Read(int index)
        {
            ThrowIfNotInitialized();
            ThrowIfHandleIsNull();
            ThrowIfIndexOutOfRange(index);
            return ref Read<T>(index);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public unsafe ref C Read<C>(int index) where C : unmanaged
        {
            ThrowIfNotInitialized();
            ThrowIfHandleIsNull();
            ThrowIfIndexOutOfRange(index);
            return ref Unsafe.AsRef<C>(PtrTo(index));
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public unsafe void Write(int index, T value)
        {
            ThrowIfNotInitialized();
            ThrowIfHandleIsNull();
            ThrowIfIndexOutOfRange(index);
            Unsafe.Write(PtrTo(index), value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public unsafe void Write<C>(int index, C value) where C : unmanaged
        {
            ThrowIfNotInitialized();
            ThrowIfHandleIsNull();
            ThrowIfIndexOutOfRange(index);
            Unsafe.Write(PtrTo(index), value);
        }


        public void CopyFrom(Span<T> data)
        {
            ThrowIfNotInitialized();
            ThrowIfHandleIsNull();
            ThrowIf1DArrayLengthIsNotTensorSize(data);
            bool r = data.TryCopyTo(Span);
            if (!r)
            {
                throw new Exception("Copy operation failed.");
            }
        }

        public void CopyFrom(params T[] data) => CopyFrom(new Span<T>(data));

        public void CopyTo(Span<T> destination)
        {
            ThrowIf1DArrayLengthIsNotTensorSize(destination);
            ThrowIfNotInitialized();
            ThrowIfHandleIsNull();
            bool r = Span.TryCopyTo(destination);
            if (!r)
            {
                // TODO: [vermorel] Do not throw naked 'Exception', use subtype instead.
                // REMARK: [allisterb] Throw OutofMemoryException.
                throw new OutOfMemoryException("Copy operation failed.");
            }
        }

        public void CopyTo(T[] destination) => CopyTo(new Span<T>(destination));

        public IVariable<T> Fill(T c)
        {
            Span.Fill(c);
            return this;
        }

        public MemoryHandle Pin(int elementIndex)
        {
            ThrowIfNotInitialized();
            Pins++;
            return MemoryHandle;
        }

        public void Unpin()
        {
            if (Pins == 0)
            {
                throw new InvalidOperationException("The memory for this variable is not pinned.");
            }
            Pins--;
        }

        // Begin IEnumerable<T> implementation
        public IEnumerator<T> GetEnumerator()
        {
            if (Initialized)
            {
                for (int i = 0; i < ElementCount; i++)
                {
                    yield return Read(i);
                }
            }
            else
            {
                yield return default;
            }
            
        }

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
        // End IEnumerable<T> implementation

        // Begin INDArray implementation
        public int NDim => Rank;

        public int[] DeviceBufferShape => Dimensions;

        public Type DType => typeof(T);

        public int ItemSize => Unsafe.SizeOf<T>();

        public INDArray Zeros() => Fill(GenericMath<T>.Const(0));

        public INDArray Ones() => Fill(GenericMath<T>.Const(1));

        public INDArray Random() => throw new NotImplementedException();

        // End NDArray implementation

        internal unsafe void* PtrTo(int index)
        {
            return Unsafe.Add<T>(MemoryHandle.Pointer, index);
        }

        internal void ThrowIfNotInitialized()
        {
            if (!Initialized)
            {
                throw new InvalidOperationException("This variable is not initialized.");

            }
        }

        internal unsafe void ThrowIfHandleIsNull()
        {
            if (MemoryHandle.Pointer == null)
            {
                throw new InvalidOperationException("This memory handle pointer is null.");

            }
        }
        internal void ThrowIf1DArrayLengthIsNotTensorSize(Array data)
        {
            if (Tensor.ElementCount != data.Length)
            {
                throw new ArgumentException($"The number of data elements ({data.Length}) does not match " +
                    $"the number of elements in tensor {Tensor.Label} : {Tensor.ElementCount}.");
            }
        }

        internal void ThrowIf1DArrayLengthIsNotTensorSize(params T[] data)
        {
            if (Tensor.ElementCount != data.Length)
            {
                throw new ArgumentException($"The number of data elements ({data.Length}) does not match " +
                    $"the number of elements in tensor {Tensor.Label} : {Tensor.ElementCount}.");
            }
        }

        internal void ThrowIf1DArrayLengthIsNotTensorSize(Span<T> data)
        {
            if (Tensor.ElementCount != data.Length)
            {
                throw new ArgumentException($"The number of data elements ({data.Length}) does not match " +
                    $"the number of elements in tensor {Tensor.Label} : {Tensor.ElementCount}.");
            }
        }

        internal void ThrowIfIndexOutOfRange(int index)
        {
            if (index >= Tensor.ElementCount)
            {
                throw new IndexOutOfRangeException($"Index {index} is greater than the maximum index of the memory " +
                    $"buffer {Tensor.ElementCount - 1}.");
            }
            else if (index < 0)
            {
                throw new IndexOutOfRangeException($"Index {index} is less than zero.");
            }
        }

        internal void ThrowIfPinned()
        {
            if (IsPinned)
            {
                throw new InvalidOperationException("The memory for this variable is still pinned and cannot be freed.");
            }
        }

        void IDisposable.Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        private void Dispose(bool disposing)
        {
            if (disposing)
            {
                ThrowIfPinned();
                MemoryHandle.Dispose();
            }
        }

        public static DataType GetDataType()
        {
            T t = default;
            switch (t)
            {
                case SByte _:
                    return DataType.INT8;

                case Byte _:
                    return DataType.UINT8;

                case Int16 _:
                    return DataType.INT16;

                case UInt16 _:
                    return DataType.UINT16;

                case Int32 _:
                    return DataType.INT32;

                case UInt32 _:
                    return DataType.UINT32;

                case Int64 _:
                    return DataType.INT64;

                case UInt64 _:
                    return DataType.UINT64;

                case Single _:
                    return DataType.FLOAT32;

                case Double _:
                    return DataType.FLOAT64;

                default:
                    throw new NotSupportedException($"Unsupported .NET type:{typeof(T).Name}");
            }

        }
    }
}