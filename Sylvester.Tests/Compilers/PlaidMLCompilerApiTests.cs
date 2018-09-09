using System;
using System.Collections.Generic;
using System.Linq;

using Xunit;

using Sylvester.Compiler.PlaidML;
using Sylvester.Compiler.PlaidML.Bindings;
using Sylvester.Compiler.PlaidML.Generator;

using Sylvester.Notation;
using Sylvester.Loggers;

namespace Sylvester.Tests.Compilers
{
    public class PlaidMLCompilerApiTests
    {
        protected Context testContext;

       
        public PlaidMLCompilerApiTests()
        {
            CompilerDriver.SetLogger(() => SerilogLogger.CreateDefaultLogger("Sylvester.Tests.log"));
            testContext = new Context();
        }

        [Fact]
        public void CanGetVersion()
        {
            string s = plaidml.PlaidmlGetVersion();
            Assert.False(string.IsNullOrEmpty(plaidml.PlaidmlGetVersion()));
        }

        [Fact]
        public void CanCreateContext()
        {
            Context ctx = new Context();
            Assert.True(ctx.IsAllocated);
            ctx.Free();
            Assert.False(ctx.IsAllocated);
        }

        [Fact]
        public void CanLoadSettings()
        {
            Settings settings = new Settings();
            Assert.True(settings.IsLoaded);
            Assert.True(settings.Dict.ContainsKey("platform"));
        }

        [Fact]
        public void CanGetValidDevices()
        {
            DeviceEnumerator e = new DeviceEnumerator(new Context());
            Assert.True(e.ValidDevices.Count > 0);
            DeviceConfig d = e.ValidDevices[0];
            Assert.True(d.Id.IsNotNullOrEmpty());
            Assert.True(d.Config.IsNotNullOrEmpty());
            Assert.True(d.Description.IsNotNullOrEmpty());
            Assert.True(d.Details.IsNotNullOrEmpty());
            Assert.True(e.GetConfigSource().IsNotNullOrEmpty());
            e.Free();
        }

        [Fact]
        public void CanOpenDevice()
        {
            Device device = new Device(testContext);
            Assert.True(device.IsAllocated);
            Assert.True(device.IsOpen);
            device.Close();
            Assert.True(device.IsClosed);
            device.Free();
            Assert.False(device.IsAllocated);
        }

        [Fact]
        public void CanConstructShape()
        {
            Compiler.PlaidML.Shape s = new Compiler.PlaidML.Shape(testContext, PlaidmlDatatype.PLAIDML_DATA_FLOAT32, 4);
            Assert.True(s.IsAllocated);
            Assert.Equal(PlaidmlDatatype.PLAIDML_DATA_FLOAT32, s.DataType);
            Assert.Equal(1ul, s.DimensionCount);
            (ulong size, long stride) = s.Dimensions[0];
            Assert.Equal(4ul, size);
            Assert.Equal(1L, stride);
        }

        [Fact]
        public void CanConstructBuffer()
        {
            Device device = new Device(testContext);
            Compiler.PlaidML.Shape s1 = new Compiler.PlaidML.Shape(testContext, PlaidmlDatatype.PLAIDML_DATA_FLOAT32, 4, 8, 8);
            device.CreateBuffer(s1);
            Assert.Single(device.Buffers);
            DeviceBuffer buffer = device.Buffers[0];
            Assert.Equal(8UL * 8 * 4 * 4, buffer.SizeInBytes);
            Compiler.PlaidML.Shape s2 = new Compiler.PlaidML.Shape(testContext, PlaidmlDatatype.PLAIDML_DATA_INT64, 13, 5);
            device.CreateBuffer(s2);
            buffer = device.Buffers[1];
            Assert.Equal((13UL * 5 * 8), buffer.SizeInBytes);
        }

        [Fact]
        public void CanConstructMapping()
        {
            Device device = new Device(testContext);
            Compiler.PlaidML.Shape s1 = new Compiler.PlaidML.Shape(testContext, PlaidmlDatatype.PLAIDML_DATA_FLOAT64, 4, 8, 8);
            device.CreateBuffer(s1);
            Assert.Single(device.Buffers);
            DeviceBuffer buffer = device.Buffers[0];
            Assert.Equal(8UL * 8 * 4 * 8, buffer.SizeInBytes);
            MemoryMapping m = new MemoryMapping(buffer);
            Span<long> span = m.GetSpan<long>();
            Assert.Equal((int)s1.ElementCount, span.Length);
        }

        [Fact]
        public void CanConstructTensorVariableView()
        {
            Device device = new Device(testContext);
            Compiler.PlaidML.Shape s1 = new Compiler.PlaidML.Shape(testContext, PlaidmlDatatype.PLAIDML_DATA_FLOAT64, 2, 3);
            DeviceTensor t = new DeviceTensor(device, s1, "t");
            DeviceTensorView<Int64> v = t.CreateView<Int64>(MemoryMapType.Discard);
            Int64[,] array = { { 0, 1, 3 }, { 4, 5, 6 } };
            v.CopyFromAndFree(array.Flatten<Int64>().ToArray());
            Assert.Throws<InvalidOperationException>(() => v.Free());
            DeviceTensorView<Int64> v2 = t.CreateView<long>(MemoryMapType.Retain);
            Assert.Equal(3, v2[2]);
            Assert.Equal(6, v2[5]);
        }

        [Fact]
        public void CanInvokeFunction()
        {
            Device device = new Device(testContext);
            string code = @"function (I[N]) -> (O) {
                                O[i: N] = +(I[k]), i - k < N;
                            }";
            Function f = new Function(testContext, code);
            DeviceTensor i = new DeviceTensor(device, new Compiler.PlaidML.Shape(testContext, PlaidmlDatatype.PLAIDML_DATA_INT32, 6), "I");
            DeviceTensor o = new DeviceTensor(device, new Compiler.PlaidML.Shape(testContext, PlaidmlDatatype.PLAIDML_DATA_INT32, 6), "O");

            Int32[] input_data = { 0, 1, 3, 4, 5, 6 };
            i.CreateView<Int32>(MemoryMapType.Discard).CopyFromAndFree(input_data);

            DeviceTensorView<Int32> v = i.CreateView<Int32>(MemoryMapType.Retain);
            Assert.Equal(3, v[2]);
            Invoker<int> invoker = new Invoker<int>(testContext, f, new Compiler.PlaidML.DeviceTensor[] { i },
                new Compiler.PlaidML.DeviceTensor[] { o });

            Compiler.PlaidML.Shape x = invoker.GetOutputShape("O");
            Assert.True(x.ElementCount == 6);
            Assert.True(invoker.AllVariablesSet);
            Invocation<Int32> inv = invoker.Invoke();
            DeviceTensorView<Int32> R = o.CreateView<Int32>(MemoryMapType.Retain);
            Assert.Equal(6, R.ElementCount);
            Assert.Equal(4, R[2]);
        }

        [Fact]
        public void CanGenerateTileFunction()
        {
            var A = Tensor.TwoD("A", (8, 17), "a", out Index a, out Index b);
            var B = Tensor.TwoD("B", (8, 17));
            TileGenerator g = new TileGenerator((A[a, b] * B[a, b]).ToTree());
            Assert.Equal(" = A[a, b] * B[a, b];", g.Text);
            g = new TileGenerator(A[b].ToTree());
            Assert.Equal(" = A[b];", g.Text);

            var (x, y) = new Vector("x", 2).Two();
            Assert.Equal("x", x.Name);
            Assert.Equal("y", y.Name);
            var (m, n) = new Scalar("m").Two();
            y.def = m * x + n;
            g = new TileGenerator(y.ToTree());
            Assert.Equal("Y = (M * X) + N;", g.Text);
        }


        [Fact]
        public void CanComposeFunction()
        {
            Device device = new Device(testContext);
            string code = @"function (I) -> (O) {
                                O = I * I;
                            }";
            Function f = new Function(testContext, code);
            Assert.True(f.IsAllocated);
            DeviceTensor i = new DeviceTensor(device, new Compiler.PlaidML.Shape(testContext, PlaidmlDatatype.PLAIDML_DATA_INT32, 6), 
                "I");
            DeviceTensor o = new DeviceTensor(device, new Compiler.PlaidML.Shape(testContext, PlaidmlDatatype.PLAIDML_DATA_INT32, 6), 
                "O");

            Int32[] input_data = { 0, 1, 3, 4, 5, 6 };
            i.CreateView<Int32>(MemoryMapType.Discard).CopyFromAndFree(input_data);
            var v = i.CreateView<Int32>(MemoryMapType.Retain);
            Assert.Equal(3, v[2]);
            Invoker<int> inv1 = new Invoker<int>(testContext, f, o, i);
            Assert.True(inv1.IsAllocated);
            Invocation<int> k = inv1.Invoke();
            Assert.True(k.IsAllocated);
            DeviceTensorView<Int32> R = o.CreateView<Int32>(MemoryMapType.Retain);
            Assert.Equal(6, R.ElementCount);
            Assert.Equal(9, R[2]);


            DeviceTensor co = new DeviceTensor(device, new Compiler.PlaidML.Shape(testContext, PlaidmlDatatype.PLAIDML_DATA_INT32, 6),
                "O");
            Composer c = new Composer(testContext);
            Applier a = new Applier(testContext, f);
            a.AddInputValue(i);
            var fo = a.AddOutputValue("O");
            Assert.True(fo.IsAllocated);
            Assert.True(c.AddInputPlaceholder("I", i.DimensionCount));
            Assert.True(c.AddOutputValue(co));
            Assert.True(c.AddDependency(a));
            Assert.True(c.AddUpdate(co, fo));
            Function gf = c.BuildFunction();
            Assert.True(gf.IsAllocated);
            Invoker<int> gi = new Invoker<int>(testContext, gf, co , i);
            Assert.True(gi.IsAllocated);
           
            Invocation<int> inv = gi.Invoke();
            Assert.True(inv.IsAllocated);
            R = co.CreateView<Int32>(MemoryMapType.Retain);
            Assert.Equal(6, R.ElementCount);
            Assert.Equal(9, R[2]);
        }

        [Fact]
        public void CanComputeGradient()
        {
            Device device = new Device(testContext);
            string code = @"function (I) -> (O) {
                                O = I * I;
                            }";
            Function f = new Function(testContext, code);
            Assert.True(f.IsAllocated);
            DeviceTensor i = new DeviceTensor(device, new Compiler.PlaidML.Shape(testContext, PlaidmlDatatype.PLAIDML_DATA_INT32, 6),
                "I");
            DeviceTensor o = new DeviceTensor(device, new Compiler.PlaidML.Shape(testContext, PlaidmlDatatype.PLAIDML_DATA_INT32, 6),
                "O");

            Int32[] input_data = { 0, 1, 3, 4, 5, 6 };
            i.CreateView<Int32>(MemoryMapType.Discard).CopyFromAndFree(input_data);
            var v = i.CreateView<Int32>(MemoryMapType.Retain);
            Assert.Equal(3, v[2]);
            Invoker<int> inv1 = new Invoker<int>(testContext, f, o, i);
            Assert.True(inv1.IsAllocated);
            Invocation<int> k = inv1.Invoke();
            Assert.True(k.IsAllocated);
            DeviceTensorView<Int32> R = o.CreateView<Int32>(MemoryMapType.Retain);
            Assert.Equal(6, R.ElementCount);
            Assert.Equal(9, R[2]);

            var gradients = new Dictionary<DeviceTensor, Value>();
            for (int n = 0; n < inv1.InputTensors.Count; n++)
            {
                Gradient gc = new Gradient(testContext, inv1.OutputValue);
                if (!gc.IsAllocated) continue;
                Value grad = gc.ComputeWrt(inv1.InputTensors[n]);
                if (!grad.IsAllocated) continue;
                gradients.Add(inv1.InputTensors[n], grad);
            }

            Composer c = new Composer(testContext);
         

            foreach (DeviceTensor t in inv1.InputTensors)
            {
                Assert.True(c.AddInputPlaceholder(t.Name, t.DimensionCount));
            }
            List<DeviceTensor> gradTensors = new List<DeviceTensor>();
            foreach(var g in gradients)
            {
                var gt = new DeviceTensor(g.Key.Device, g.Key.Shape, g.Value.Name);
                c.AddOutputValue(gt);
                c.AddUpdate(gt, g.Value);
                gradTensors.Add(gt);
            }
            Function gf = c.BuildFunction();
            Assert.True(gf.IsAllocated);
            Invoker<int> ginv = new Invoker<int>(testContext, gf, inv1.InputTensors.ToArray(), gradTensors.ToArray());
            Assert.True(ginv.IsAllocated);
            var k2 = ginv.Invoke();
            Assert.True(k2.IsAllocated);

            DeviceTensorView<Int32> RG = gradTensors.First().CreateView<Int32>(MemoryMapType.Retain);
        }
    }
}
