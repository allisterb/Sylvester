using System;
using System.Buffers;
using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class MemoryMapping : PlaidMLApi<MemoryMapping>, IPinnable
    {
        public DeviceBuffer Buffer { get; protected set; }

        public IntPtr BaseAddress
        {
            get
            {
                ThrowIfNotAllocated();
                ThrowIfNotValid();

                unsafe
                {
                    void* _r = plaidml.__Internal.PlaidmlGetMappingBase(_context, this);
                    return new IntPtr(_r);
                }
            }
        }

        public ulong SizeInBytes
        {
            get
            {
                ThrowIfNotAllocated();
                ThrowIfNotValid();
                return plaidml.__Internal.PlaidmlGetMappingSize(_context, this);
            }
        }

        public MemoryHandle MemoryHandle { get; protected set; }

        public bool IsValid { get; protected set; }

        /// <summary> Reference counting. </summary>
        public int Pins { get; protected set; }

        public bool IsPinned => Pins > 0;

        public MemoryMapType Type { get; protected set; }

        public MemoryMapping(DeviceBuffer buffer, bool discard = true) : base(buffer.Context)
        {
            if (discard)
            {
                _ptr = plaidml.__Internal.PlaidmlMapBufferDiscard(buffer.Context, buffer);
                Type = MemoryMapType.Discard;
            }
            else
            {
                _ptr = plaidml.__Internal.PlaidmlMapBufferCurrent(buffer, IntPtr.Zero, IntPtr.Zero);
                Type = MemoryMapType.Retain;
            }

            if (_ptr.IsZero())
            {
                ReportApiCallError(discard ? "plaidml_map_buffer_discard" : "plaidml_map_buffer_current");
                return;
            }

            Buffer = buffer;
            IsAllocated = true;
            IsValid = true;
            Pin(0);
        }


        public override void Free()
        {
            ThrowIfPinned();

            base.Free();
            plaidml.__Internal.PlaidmlFreeMapping(_ptr);
            _ptr = IntPtr.Zero;
        }

        public virtual bool Writeback()
        {
            ThrowIfNotAllocated();
            ThrowIfNotValid();
            Unpin();
            ThrowIfPinned();

            var r = plaidml.__Internal.PlaidmlWritebackMapping(_context, this);
            if (!r)
            {
                ReportApiCallError("plaidml_writeback_mapping");
            }
            else
            {
                Invalidate();
            }

            return r;
        }

        public unsafe Span<T> GetSpan<T>() where T : unmanaged, IEquatable<T>, IComparable<T>, IConvertible
        {
            ThrowIfNotAllocated();
            ThrowIfNotValid();

            return new Span<T>(BaseAddress.ToPointer(), (int) (SizeInBytes / (ulong) sizeof(T)));
        }

        public unsafe MemoryHandle Pin(int elementIndex)
        {
            ThrowIfNotAllocated();
            ThrowIfNotValid();

            Pins++;
            return MemoryHandle;
        }

        public void Unpin()
        {
            ThrowIfNotAllocated();
            ThrowIfNotValid();

            Pins--;
        }

        internal void ThrowIfNotValid()
        {
            if (!IsValid)
            {
                throw new InvalidOperationException("This mapping has been invalidated.");
            }
        }

        internal void ThrowIfPinned()
        {
            if (IsPinned)
            {
                throw new InvalidOperationException(
                    "The memory for this mapping is still pinned and cannot be invalidated or freed.");
            }
        }

        protected void Invalidate()
        {
            IsValid = false;
        }
    }
}