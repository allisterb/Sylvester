using System;
using System.Collections.Generic;
using System.Linq;
using Sylvester.Notation;
using Sylvester.Trees;

namespace Sylvester.Compiler
{
    public partial class Kernel<T> : IKernel<T> where T : unmanaged, IEquatable<T>, IComparable<T>, IConvertible
    {
        public DeviceType DeviceType { get; protected set; }

        public IExpressionTree ExpressionTree => Tree;

        public ITermShape OutputShape { get; set; }

        public IReadOnlyList<ITermShape> InputShapes { get; protected set; }

        public ExpressionTree Tree { get; protected set; }

        public IReadOnlyList<Tensor> Tensors => 
            Tree.TensorNodes.Distinct(Tree).Select(n => n.ValueAs<Tensor>()).ToList();

        public IReadOnlyList<Tensor> InputTensors => Tree.InputVariableNodes.Select(n => n.ValueAs<Tensor>()).ToList();

        public Tensor OutputTensor => Tree.OutputTensor;

        public ICompiler Compiler { get; protected set; }

        public IRunnable<T> CompilerResult { get; protected set; }

        public bool CompileSuccess { get; protected set; }

        public bool CompileBeforeRun { get; set; }

        public Func<Var<T>[], Var<T>> Func
        {
            get
            {
                if (!CompileSuccess)
                {
                    return null;
                }

                if (CompileBeforeRun || !CompileSuccess)
                {
                    Compile();
                }

                return new Func<Var<T>[], Var<T>>((input) =>
                {
                    if (input.Length != this.InputShapes.Count)
                    {
                        throw new ArgumentException($"The kernel has {InputShapes.Count} input parameters but "
                                                    + $"{input.Length} arguments were used.");
                    }

                    for (int i = 0; i < input.Length; i++)
                    {
                        if (input[i].Tensor == null)
                        {
                            input[i].Tensor = InputTensors[i];
                        }
                    }

                    var output = OutputTensor.Var(new T[OutputTensor.ElementCount]);
                    var runStatus = CompilerResult.Run(output, input);
                    if (runStatus == RunStatus.Success)
                    {
                        return output;
                    }
                    else
                    {
                        throw new RunException<T>(this, CompilerResult, runStatus, "Run did not complete successfully.");
                    }
                });
            }
        }

        protected ITermShape GeneratedOutputShape { get; set; }

        public Kernel(Tensor output)
        {
            if (!output.IsDefined)
            {
                throw new ArgumentException
                    ($"The output tensor {output.Label} must be assigned an input expression.");
            }

            Tree = output.ToTree();
            InputShapes = InputTensors;
            OutputShape = output;
        }

        public Kernel(Tensor output, ICompiler compiler, DeviceType deviceType = DeviceType.CPU) : this(output)
        {
            Compiler = compiler;
            DeviceType = deviceType;
        }

        public Kernel(Tensor output, TensorExpression expr)
        {
            output.def = expr;
            Tree = output.ToTree();
            InputShapes = InputTensors;
            OutputShape = output;
        }
        public Kernel(Tensor output, TensorExpression expr, ICompiler compiler, DeviceType deviceType = DeviceType.CPU)
            : this(output, expr)
        {
            Compiler = compiler;
            DeviceType = deviceType;
        }

        public Kernel(TensorExpression expr, ICompiler compiler, DeviceType deviceType = DeviceType.CPU)
            : this(new Tensor("O", expr))
        {
            Compiler = compiler;
            DeviceType = deviceType;
        }

        public ITermShape this[int index]
        {
            get
            {
                if (index < 0 || index > InputShapes.Count - 1)
                {
                    throw new IndexOutOfRangeException();
                }

                return InputShapes[index];
            }
        }

        public ITermShape this[Tensor index]
        {
            get => InputShapes.SingleOrDefault(t => t.Label == index.Name) ??
                   throw new ArgumentException($"The kernel does not contain an input variable bound to tensor "
                                               + $"{index.Label}");
        }


        public bool Compile()
        {
            CompileSuccess = Compiler.Compile(this, out IRunnable<T> result);
            if (CompileSuccess)
            {
                CompilerResult = result;
            }

            return CompileSuccess;
        }

        public IRunnable<T> Compile(out CompilerStatus status)
        {
            if (this.Compile())
            {
                status = this.Compiler.Status;
                return CompilerResult;
            }
            else
            {
                status = this.Compiler.Status;
                return null;
            }
        }
       

        protected void ThrowIfNotCompileSuccess()
        {
            if (!CompileSuccess)
            {
                throw new InvalidOperationException("The kernel was not compiled successfully.");
            }
        }
    }
}