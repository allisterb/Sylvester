using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class DeviceEnumerator : PlaidMLApi<DeviceEnumerator>
    {
        public int Count
        {
            get
            {
                ThrowIfNotAllocated();
                return (int) plaidml.__Internal.PlaidmlGetDevconfCount(_context, this, true);
            }
        }

        public List<DeviceConfig> ValidDevices
        {
            get
            {
                var count = (int) plaidml.__Internal.PlaidmlGetDevconfCount(_context, this, true);
                if (count == 0)
                {
                    return null;
                }

                var vd = new List<DeviceConfig>(count);
                for (var i = 0; i < count; i++)
                {
                    vd.Add(new DeviceConfig(_context, this, (ulong) i));
                }

                return vd;
            }
        }

        public DeviceEnumerator(Context ctx) : base(ctx)
        {
            if (_context.Settings.IsManualConfig)
            {
                _ptr = plaidml.__Internal.PlaidmlAllocDeviceEnumeratorWithConfig(_context, _context.Settings.Config,
                    IntPtr.Zero, IntPtr.Zero);
            }
            else
            {
                _ptr = plaidml.__Internal.PlaidmlAllocDeviceEnumerator(_context, IntPtr.Zero, IntPtr.Zero);
            }

            if (_ptr.IsZero())
            {
                ReportApiCallError("plaidml_alloc_device_enumerator_with_config");
                throw new PlaidMLApiException<DeviceEnumerator>(this, "Could not allocate device enumerator.");
            }

            IsAllocated = true;
        }

        public override void Free()
        {
            base.Free();
            plaidml.__Internal.PlaidmlFreeDeviceEnumerator(_ptr);
            _ptr = IntPtr.Zero;
        }

        public string GetConfigSource()
        {
            IntPtr r = plaidml.__Internal.PlaidmlGetEnumeratorConfigSource(this);
            if (r.IsZero())
            {
                ReportApiCallError("plaidml_get_enumerator_config_source");
                return string.Empty;
            }

            return Marshal.PtrToStringAnsi(r);
        }
    }
}