using System;
using System.Collections.Generic;
using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class Device : PlaidMLApi<Device>
    {
        public DeviceConfig DeviceConfig { get; protected set; }
        public bool IsOpen { get; protected set; }
        public bool IsClosed { get; protected set; }
        public List<DeviceBuffer> Buffers { get; protected set; }


        public Device(Context ctx, DeviceConfig devconf = null) : base(ctx)
        {
            if (devconf != null)
            {
                _ptr = plaidml.__Internal.PlaidmlOpenDevice(_context, devconf);
            }
            else
            {
                _ptr = plaidml.__Internal.PlaidmlOpenDevice(_context, IntPtr.Zero);
            }

            if (_ptr.IsZero())
            {
                ReportApiCallError("plaidml_open_device");
                throw new PlaidMLApiException<Device>(this, "Could not open device.");
            }

            DeviceConfig = devconf;
            Buffers = new List<DeviceBuffer>();
            IsAllocated = true;
            IsOpen = true;
        }

        public override void Free()
        {
            if (IsOpen)
            {
                Close();
            }

            base.Free();
        }

        public void Close()
        {
            ThrowIfNotAllocated();
            ThrowIfNotOpen();

            if (Buffers != null && Buffers.Count > 0)
            {
                Buffers.ForEach(buffer => buffer.Free());
            }

            plaidml.__Internal.PlaidmlCloseDevice(this);
            IsOpen = false;
            IsClosed = true;
        }

        public DeviceBuffer CreateBuffer(Shape shape)
        {
            return new DeviceBuffer(_context, this, shape);
        }

        internal void ThrowIfNotOpen()
        {
            if (!IsOpen)
            {
                throw new InvalidOperationException("This device is not open.");
            }
        }
    }
}