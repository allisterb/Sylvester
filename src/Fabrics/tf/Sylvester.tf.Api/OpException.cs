using System;
using System.Collections.Generic;
using System.Text;

namespace TensorFlow
{
    public class OpException : Exception
    {
        public OpException(TF_Operation op, TF_Status status) : 
            base(string.Format("An error occurred during operation {0}: {1}", c_api.TF_OperationName(op) ?? "(unknown op)", 
                tf_status.TF_Message(status) ?? "(unknown status)")) 
        {}
        public OpException(TF_Operation op) :
            base(string.Format("An error occurred during operation {0}.", c_api.TF_OperationName(op) ?? "(unknown op)"))
        { }
    }
}
