namespace Sylvester.Trees
{
    public enum TensorOp
    {
        NoOp = 1,

        Assign,
        ElementWiseAssign,
        IndexedAssign,
        Index,

        Sum,
        Product,
        Max,
        Min,
        
        Add,
        Sub,
        Mul,
        Div,
        Square,
        Pow,

        Sqrt,
        Log,
        Exp,
        Sin,
        Cos,
        Tanh,
        Sigmoid
    }
}