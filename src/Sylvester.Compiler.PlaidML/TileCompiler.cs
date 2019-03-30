using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Newtonsoft.Json;
using Sylvester.Compiler.PlaidML.Generator;

namespace Sylvester.Compiler.PlaidML
{
    public class TileCompiler : PlaidMLApi<TileCompiler>, ICompiler
    {
        public Dictionary<string, object> Options { get; }

        public IContext TensorContext { get; }

        public bool Initialized { get; protected set; }

        public CompilerStatus Status { get; protected set; }

        public string CompilerStatusMessage { get; protected set; }

        public Settings Settings => _context.Settings;

        public string SessionId { get; protected set; }

        public DeviceEnumerator DeviceEnumerator { get; protected set; }

        public int DeviceCount => DeviceEnumerator.Count;

        public Device KernelDevice { get; protected set; }

        public Dictionary<string, object> KernelDeviceProperties { get; protected set; }


        public TileCompiler(Dictionary<string, object> options = null) : base(new Context())
        {
            if (!_context.IsAllocated)
            {
                CompilerStatusMessage = _context.LastStatusString;
                // TODO: [vermorel] An exception should be thrown here.
                // REMARK: [allisterb] Throw PlaidMLApiException on failure.  
                throw new PlaidMLApiException<TileCompiler>(this, "Could not allocate Context.");
             
            }

            Options = options;
            SessionId = Settings.StartNewSession();
            DeviceEnumerator = new DeviceEnumerator(_context);
            if (!DeviceEnumerator.IsAllocated)
            {
                CompilerStatusMessage = DeviceEnumerator.LastStatusString;
                return;
            }

            if (DeviceEnumerator.ValidDevices.Count < 1)
            {
                Error("No valid devices available.");
                return;
            }

            IsAllocated = true;
            KernelDevice = OpenFirstDevice();
            KernelDeviceProperties = JsonConvert.DeserializeObject<Dictionary<string, object>>
                (KernelDevice.DeviceConfig.Details);
            Initialized = true;
        }

        public bool Compile<TKernel>(IKernel<TKernel> kernel, out IRunnable<TKernel> result)
            where TKernel : unmanaged, IEquatable<TKernel>, IComparable<TKernel>, IConvertible
        {
            result = null;
            ThrowIfNotInitialized();
            Status = CompilerStatus.Compiling;
            var g = new TileGenerator(kernel.ExpressionTree);
            if (!g.Success)
            {
                Status = CompilerStatus.ErrorGeneratingCode;
                CompilerStatusMessage = g.Text;
                return false;
            }

            var f = CreateFunction(g);
            if (!f.IsAllocated)
            {
                Status = CompilerStatus.ErrorGeneratingCode;
                CompilerStatusMessage = f.LastStatusString;
                return false;
            }

            var inputTensors = kernel.InputShapes
                .Select(i => CreateTensor(CreateShape<TKernel>(i.Dimensions), i.Label.ToUpper()))
                .ToArray();
            var outputTensor = CreateTensor(CreateShape<TKernel>(kernel.OutputShape.Dimensions),
                kernel.OutputShape.Label.ToUpper());

            var invoker = new Invoker<TKernel>(Context, f, outputTensor, inputTensors);
            if (!(invoker.IsAllocated && invoker.AllVariablesSet))
            {
                Status = CompilerStatus.ErrorGeneratingCode;
                CompilerStatusMessage = invoker.LastStatusString;
                return false;
            }
            else
            {
                result = invoker;
                Status = CompilerStatus.Success;
                return true;
            }
        }

        public bool Compile<TKernel>(IEnumerable<ITermShape> inputShapes, ITermShape outputShape, string code,
            out IRunnable<TKernel> result)
            where TKernel : unmanaged, IEquatable<TKernel>, IComparable<TKernel>, IConvertible
        {
            result = null;
            ThrowIfNotInitialized();
            Status = CompilerStatus.Compiling;
            var f = CreateFunction(code);
            if (!f.IsAllocated)
            {
                CompilerStatusMessage = f.LastStatusString;
                return false;
            }

            var inputTensors = inputShapes
                .Select(i => CreateTensor(CreateShape<TKernel>(i.Dimensions), i.Label.ToUpper()))
                .ToArray();
            var outputTensor = CreateTensor(CreateShape<TKernel>(outputShape.Dimensions),
                outputShape.Label.ToUpper());

            var invoker = new Invoker<TKernel>(Context, f, outputTensor, inputTensors);
            if (!(invoker.IsAllocated && invoker.AllVariablesSet))
            {
                CompilerStatusMessage = invoker.LastStatusString;
                return false;
            }

            result = invoker;
            return true;
        }

        public bool Compile<TKernel>(ITermShape inputShape, ITermShape outputShape, string code,
            out IRunnable<TKernel> result)
            where TKernel : unmanaged, IEquatable<TKernel>, IComparable<TKernel>, IConvertible
            => Compile(new ITermShape[] {inputShape}, outputShape, code, out result);

        public bool Compile<TVectorKernel>(int vectorLength, string code, out IRunnable<TVectorKernel> result)
            where TVectorKernel : unmanaged, IEquatable<TVectorKernel>, IComparable<TVectorKernel>, IConvertible
        {
            ITermShape input = new DeviceTensor(OpenFirstDevice(), CreateShape<TVectorKernel>(vectorLength), "I");
            ITermShape output = new DeviceTensor(OpenFirstDevice(), CreateShape<TVectorKernel>(vectorLength), "O");
            return Compile(input, output, code, out result);
        }

        public bool Compile<TVectorKernel>(int vectorCount, int vectorLength, string code,
            out IRunnable<TVectorKernel> result)
            where TVectorKernel : unmanaged, IEquatable<TVectorKernel>, IComparable<TVectorKernel>, IConvertible
        {
            var inputs = new ITermShape[vectorCount];
            for (int i = 0; i < inputs.Length; i++)
            {
                inputs[i] = new DeviceTensor(OpenFirstDevice(), CreateShape<TVectorKernel>(vectorLength),
                    "I" + i);
            }

            ITermShape output = new DeviceTensor(OpenFirstDevice(), CreateShape<TVectorKernel>(vectorLength), "O");
            return Compile(inputs, output, code, out result);
        }

        public Device OpenFirstDevice()
        {
            ThrowIfNotAllocated();

            if (DeviceEnumerator.Count == 0)
            {
                throw new InvalidOperationException("No devices were enumerated.");
            }

            return new Device(_context, DeviceEnumerator.ValidDevices[0]);
        }

        public Function CreateFunction(TileGenerator generator)
        {
            ThrowIfNotInitialized();
            return new Function(Context, generator.FunctionText);
        }

        public Function CreateFunction(string code)
        {
            ThrowIfNotAllocated();
            return new Function(_context, code);
        }

        public Shape CreateShape<T>(params int[] dimensions) where T : unmanaged
        {
            ThrowIfNotAllocated();
            var datatype = Shape.ToDataType<T>();
            return new Shape(_context, datatype, dimensions);
        }

        public DeviceTensor CreateTensor(Shape shape, string name)
        {
            ThrowIfNotAllocated();
            return new DeviceTensor(KernelDevice, shape, name);
        }

        public Gradient CreateGradient(DeviceTensor variable)
        {
            return new Gradient(_context, variable);
        }

        [DebuggerStepThrough]
        internal void ThrowIfNotInitialized()
        {
            if (!Initialized)
            {
                throw new InvalidOperationException("This compiler instance is not initialized.");
            }
        }
    }
}