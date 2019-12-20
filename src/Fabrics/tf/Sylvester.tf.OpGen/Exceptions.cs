using System;
using System.Collections.Generic;
using System.Text;

using Serilog;
using ProtoBuf;
using TensorFlow;
using tensorflow;

namespace Sylvester.tf.OpGen
{
    public class OpGenException : Exception
    {
        public OpGenException(OpDef op, string msg) : base($"Error generating code for op {op.name}: {msg}.") {}
    }

    public class UnknownTypeException : Exception
    {
        public UnknownTypeException(string name) : base($"Generator does not handle TensorFlow type {name} yet.") { }
    }
}
