using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Xunit;
using Sylvester.Compiler;
using Sylvester.Notation;
using Sylvester.Loggers;
using static Sylvester.Notation.Operators;
using Sylvester.Compiler.PlaidML;

namespace Sylvester.Tests
{
    public class TensorOperatorsTests
    {
        public TensorOperatorsTests()
        {
            CompilerDriver.SetLogger(() => SerilogLogger.CreateDefaultLogger("Sylvester.Tests.log"));
        }

        [Fact]
        public void CanContructContractionOperation()
        {
            var (x, ypred, yactual, yerror, yloss) = new Vector(5, out Index i).Five("x", "ypred", "yactual", "yerror",
                "yloss");
            var (a, b) = new Scalar().Two("a", "b");

            ypred.def = a * x + b;
            Assert.Equal(3, ypred.ElementwiseDefinition.TensorReferences.Count);
            yerror.def = SQUARE[yactual - ypred];
            Assert.Equal(4, yerror.ElementwiseDefinition.TensorReferences.Count); 
            yloss[i, i / 2] = SUM[yerror[i]];
            Assert.True(yloss.IsDefined);
            TensorContraction c = yloss.ContractionDefinition.Expression;
            Assert.True(c.TensorReferences.Single() == yerror);
        }

        [Fact]
        public void CanConstructMeanOperation()
        {
            var (M, N, O) = new Matrix("M", 2, 2, out Index i, out Index j).Three();
            Scalar S = new Scalar("S")
            {
                def = MEAN[M]
            };
            Assert.True(S.IsDefined);
            Assert.Equal(0, S.Rank);
            Kernel<int> km = new Kernel<int>(S, new TileCompiler());
            var r = km.Compile(out CompilerStatus status);
            Assert.Equal(CompilerStatus.Success, status);
            //var rs = r.Run(N.Var(new int[4]), M.Var(3, 3, 3, 3));
        }
    }
}
