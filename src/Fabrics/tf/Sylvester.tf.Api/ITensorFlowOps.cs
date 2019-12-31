using System;
using System.Collections.Generic;
using System.Text;

namespace TensorFlow
{
    public interface ITensorFlowOps
    {
        #region Arithmetic
        TF_Output Add(TF_Output l, TF_Output r, string opName = null);
        TF_Output Sub(TF_Output l, TF_Output r, string opName = null);
        TF_Output Mul(TF_Output l, TF_Output r, string opName = null);
        TF_Output Div(TF_Output l, TF_Output r, string opName = null);
        #endregion
    }
}
