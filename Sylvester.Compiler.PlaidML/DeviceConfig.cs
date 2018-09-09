using System;
using System.Runtime.InteropServices;
using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class DeviceConfig : PlaidMLApi<DeviceConfig>
    {
        protected string _id;
        protected string _config;
        protected string _description;
        protected string _details;


        public DeviceEnumerator Enumerator { get; protected set; }

        public ulong Index { get; protected set; }

        public string Id
        {
            get
            {
                if (_id.IsNullOrEmpty())
                {
                    _id = QueryStringProperty(PlaidmlDeviceProperty.PLAIDML_DEVICE_ID);
                }

                return _id;
            }
        }

        public string Config
        {
            get
            {
                if (_config.IsNullOrEmpty())
                {
                    _config = QueryStringProperty(PlaidmlDeviceProperty.PLAIDML_DEVICE_CONFIG);
                }

                return _config;
            }
        }

        public string Description
        {
            get
            {
                if (_description.IsNullOrEmpty())
                {
                    _description = QueryStringProperty(PlaidmlDeviceProperty.PLAIDML_DEVICE_DESCRIPTION);
                }

                return _description;
            }
        }

        public string Details
        {
            get
            {
                if (_details.IsNullOrEmpty())
                {
                    _details = QueryStringProperty(PlaidmlDeviceProperty.PLAIDML_DEVICE_DETAILS);
                }

                return _details;
            }
        }

        public DeviceConfig(Context ctx, DeviceEnumerator enumerator, ulong index) : base(ctx)
        {
            _ptr = plaidml.__Internal.PlaidmlGetDevconf(_context, enumerator, index);
            if (_ptr.IsZero())
            {
                ReportApiCallError("plaidml_get_devconf");
                throw new PlaidMLApiException<DeviceConfig>(this, "Could not get device config.");
            }

            Enumerator = enumerator;
            Index = index;
            IsAllocated = true;
        }

        public string QueryStringProperty(PlaidmlDeviceProperty property)
        {
            unsafe
            {
                ulong* sizeRequired = stackalloc ulong[1];
                bool r = plaidml.__Internal.PlaidmlQueryDevconf(_context, this, property, IntPtr.Zero, 0, sizeRequired);
                if (!r)
                {
                    ReportApiCallError("plaidml_query_dev_conf");
                    return string.Empty;
                }
                else if (*sizeRequired == 0)
                {
                    return string.Empty;
                }
                else
                {
                    string result = "";
                    ;
                    int bufferSize = (int) *sizeRequired;
                    IntPtr buffer = Marshal.AllocHGlobal(bufferSize);
                    r = plaidml.__Internal.PlaidmlQueryDevconf(_context, this, property, buffer, *sizeRequired,
                        sizeRequired);
                    if (!r)
                    {
                        ReportApiCallError("plaidml_query_dev_conf");
                        result = string.Empty;
                    }
                    else
                    {
                        result = Marshal.PtrToStringAnsi(buffer);
                    }

                    Marshal.FreeHGlobal(buffer);
                    return result;
                }
            }
        }
    }
}