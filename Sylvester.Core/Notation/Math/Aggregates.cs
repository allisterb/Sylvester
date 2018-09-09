using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;

namespace Sylvester.Notation
{
    public static partial class Math
    {
        public static TensorIndexExpression Sum(TensorIndexExpression l) => 
            new TensorIndexExpression(GetOpMethodCall("Sum", l));

        public static TensorIndexExpression Sum(TensorExpression l) =>
            new TensorIndexExpression(GetOpMethodCall("Sum", (TensorIndexExpression) l), new Dimension(0));

       
        public static TensorIndexExpression Product(TensorIndexExpression l) =>
           new TensorIndexExpression(GetOpMethodCall("Product", l));

        public static TensorIndexExpression Product(TensorExpression l) =>
          new TensorIndexExpression(GetOpMethodCall("Product", (TensorIndexExpression)l), new Dimension(0));


        public static TensorIndexExpression Max(TensorIndexExpression l) =>
            new TensorIndexExpression(GetOpMethodCall("Max", l));

        public static TensorIndexExpression Max(TensorExpression l) =>
          new TensorIndexExpression(GetOpMethodCall("Max", (TensorIndexExpression)l), new Dimension(0));

    
        public static TensorIndexExpression Min(TensorIndexExpression l) =>
            new TensorIndexExpression(GetOpMethodCall("Min", l));

        public static TensorIndexExpression Min(TensorExpression l) =>
          new TensorIndexExpression(GetOpMethodCall("Min", (TensorIndexExpression)l), new Dimension(0));


        public static TensorIndexExpression Mean(TensorIndexExpression l)
        {
            var mulExpr = l.GetDimensionProductExpression();
            return Sum(l) / mulExpr;
        }
    }


    public partial class TensorExpression
    {
        private static TensorIndexExpression Op_Sum(TensorIndexExpression l) => null;
        private static TensorIndexExpression Op_Product(TensorIndexExpression l) => null;
        private static TensorIndexExpression Op_Max(TensorIndexExpression l) => null;
        private static TensorIndexExpression Op_Min(TensorIndexExpression l) => null;
    }
}
