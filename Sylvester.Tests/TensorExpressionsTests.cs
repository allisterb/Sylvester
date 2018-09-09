using System;
using System.Collections.Generic;
using System.Text;
using Xunit;

using Sylvester.Loggers;
using Sylvester.Notation;
using static Sylvester.Notation.Operators;

using Sylvester.Compiler;
using Sylvester.Compiler.PlaidML;
namespace Sylvester.Tests
{
    public class TensorExpressionTests
    {
        public TensorExpressionTests()
        {
            CompilerDriver.SetLogger(() => SerilogLogger.CreateDefaultLogger("Sylvester.Tests.log"));
        }

        [Fact]
        public void CanCompileLinearRegressionKernel()
        {
            TileCompiler compiler = new TileCompiler();
            var (yactual, x) = new Vector(5, out Index i).Two("yactual", "x");
            Scalar a = new Scalar("a"), b = new Scalar("b");
            var ypred = a * x + b;
            var yloss = MEAN[SQUARE[yactual - ypred]];
            Kernel<int> loss = new Kernel<int>(yloss, compiler);
            Assert.True(loss.Compile());
        }
    }
}
