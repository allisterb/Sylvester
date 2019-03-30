using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class DeviceTensor : Variable, ITermShape
    {
        public Device Device { get; protected set; }

        public DeviceBuffer DeviceBuffer { get; protected set; }

        public Shape Shape { get; protected set; }

        public int Rank => Convert.ToInt32(Shape.DimensionCount);

        public ulong DimensionCount => Shape.DimensionCount;

        public int[] Dimensions => Shape.Dimensions?.Select(d => Convert.ToInt32(d.length)).ToArray();

        public int[] Strides => Shape.Dimensions?.Select(d => Convert.ToInt32(d.stride)).ToArray();

        public string Label => Name;

        public string Id { get; set; }

        public DeviceTensor(Device device, Shape shape, string name, DeviceBuffer buffer = null) : base(device.Context,
            name)
        {
            if (buffer == null)
            {
                buffer = device.CreateBuffer(shape);
                if (!buffer.IsAllocated)
                {
                    Error("Could not allocate device buffer for tensor.");
                    return;
                }
            }

            _ptr = plaidml.__Internal.PlaidmlAllocTensor(_context, buffer, shape);
            if (_ptr.IsZero())
            {
                ReportApiCallError("plaidml_alloc_tensor");
                return;
            }

            Name = name;
            Device = device;
            DeviceBuffer = buffer;
            Shape = shape;
            DataType = Shape.DataType;
            IsAllocated = true;
        }


        public ITermShape CloneShape(string name) => new DeviceTensor(this.Device, this.Shape, name);

        public DeviceTensorView<T> CreateView<T>(MemoryMapType mapType)
            where T : unmanaged, IEquatable<T>, IComparable<T>, IConvertible
        {
            ThrowIfNotAllocated();
            return new DeviceTensorView<T>(this, mapType);
        }

        public IEnumerator<int> GetEnumerator()
        {
            for (int i = 0; i < Dimensions.Length; i++)
            {
                yield return Dimensions[i];
            }
        }

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    }
}