#region Attribution
// Contains code from TensorFlowSharp - https://github.com/migueldeicaza/TensorFlowSharp/
// TensorFlowSharp is authored by Miguel de Icaza and licensed under the MIT License: https://github.com/migueldeicaza/TensorFlowSharp/blob/master/LICENSE
#endregion

using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

using System.Linq;

namespace TensorFlow
{
	public unsafe partial  class c_api
	{
		public static void TF_SetAttrString(TF_OperationDescription desc, string attr_name, string attr_value)
		{
			ulong length = Convert.ToUInt64(attr_value.Length);
			var ptr = Marshal.StringToHGlobalAnsi(attr_value);
			TF_SetAttrString(desc, attr_name, ptr, length);
		}

		public static void TF_SetAttrStringList(TF_OperationDescription desc, string attr_name, string[] attr_values)
		{
			IntPtr[] ptrs = new IntPtr[attr_values.Length];
			ulong[] lengths = new ulong[attr_values.Length];
			for (int i = 0; i < attr_values.Length; i++)
			{
				ptrs[i] = Marshal.StringToHGlobalAnsi(attr_values[i]);
				lengths[i] = Convert.ToUInt64(attr_values[i].Length);
			}
			
			fixed (IntPtr* p = &ptrs[0])
			{
				TF_SetAttrStringList(desc, attr_name, (void **) p, ref lengths[0], attr_values.Length);
			}
		}

		public static string[] TF_FunctionNames(global::TensorFlow.TF_Function[] funcs)
		{
			string[] names = new string[funcs.Length];
			for(int i = 0; i < names.Length; i++)
			{
				names[i] = c_api.TF_FunctionName(funcs[i]);
			}
			return names;
		}
		public static void TF_SetAttrFuncName(TF_OperationDescription desc, string attr_name, string attr_value)
		{
			TF_SetAttrFuncName(desc, attr_name, attr_value, Convert.ToUInt64(attr_value.Length));
		}

		public static void TF_SetAttrFuncNames(TF_OperationDescription desc, string attr_name, string[] attr_values)
		{
			for (int i = 0; i < attr_values.Length; i++)
			{
				TF_SetAttrFuncName(desc, attr_name, attr_values[i], Convert.ToUInt64(attr_values[i].Length));
			}
		}
		public static void TF_SetAttrTypeList(TF_OperationDescription desc, string attr_name, TF_DataType[] values)
		{
			fixed (TF_DataType* ptr = &values[0])
			{
				TF_SetAttrTypeList(desc, attr_name, ptr, values.Length);
			}
		}

		public static void TF_SetAttrShapeList(TF_OperationDescription desc, string attr_name, long[][] dims)
		{
			int[] num_dims = new int[dims.GetLength(0)];
			IntPtr[] ptr_dims = new IntPtr[dims.GetLength(0)];
			for (int i = 0; i < dims.GetLength(0); i++)
			{
				num_dims[i] = dims[i].Length;
				ptr_dims[i] = Marshal.AllocHGlobal(sizeof(int) * dims[i].Length);
				Marshal.Copy(dims[i], 0, ptr_dims[i], dims[i].Length);
			}
			fixed (IntPtr* ptr = &ptr_dims[0])
			{
				c_api.TF_SetAttrShapeList(desc, attr_name, (long**)ptr, ref num_dims[0], dims.Length);
			}
		}
	}

	public unsafe partial class TF_Graph : ITensorFlowOps
    {
		#region Properties
		public TF_Operation[] Dependencies { get; set; }

		public string NameScope { get; internal set; }
		#endregion

		#region Methods
		public void SetNameScope(string s)
		{
			
			NameScope = s;
			
		}

		public string MakeName(string opName, string customOpName = null)
		{
			if (string.IsNullOrEmpty(customOpName))
			{
				return MakeUniqueName(string.IsNullOrEmpty(NameScope) || NameScope == "_" ? opName : NameScope + "/" + opName);
			}
			else
			{
				return MakeUniqueName(string.IsNullOrEmpty(NameScope) || NameScope == "_"  ? customOpName : NameScope + "/" + customOpName);
			}	
		}

		public string MakeUniqueName(string name)
		{
			int val = 0;
			if (ids.TryGetValue(name, out val))
			{
				val++;
			}
			ids[name] = val;
			return name + "_" + val;
		}

		public string GetName(string name)
		{
			if(string.IsNullOrEmpty(NameScope))
			{
				return name +"_" + ids[name];
			}
			else if (name.StartsWith(NameScope))
			{
				return name;
			}
			else
			{
				var n = this.NameScope + "/" + name;
				return n + "_" + ids[n];
			}
		}

		public static TF_Graph Import(byte[] buffer, TF_ImportGraphDefOptions options, out List<TF_Operation> ops, out TF_Status status)
		{
			var b = new Buffer(buffer);
			var graph = c_api.TF_NewGraph();
			status = tf_status.TF_NewStatus();
			ops = null;
			c_api.TF_GraphImportGraphDef(graph, b, options, status);
			if (graph != null)
			{
				ulong pos = 0;
				TF_Operation op;
				ops = new List<TF_Operation>();
				while((op = c_api.TF_GraphNextOperation(graph, ref pos)) != null)
				{
					ops.Add(op);
				}
			}
			if (ops != null && ops.Count > 0)
			{
				var names = ops.Select(o => new { Name = c_api.TF_OperationName(o), Type = c_api.TF_OperationOpType(o) });
			}
			return graph;
		}

		public static TF_Graph Import(string filePath, TF_ImportGraphDefOptions options, out List<TF_Operation> ops, out TF_Status status) => 
			Import(File.ReadAllBytes(filePath), options, out ops, out status);
	
		#endregion

		#region Fields
		Dictionary<string, int> ids = new Dictionary<string, int>();
        #endregion
	}

	public unsafe partial class TF_Output
	{
		public TF_Output(TF_Operation oper, int index) : this()
		{
			Oper = oper;
			Index = index;
		}
	}

	public unsafe partial class TF_Session
	{
		[System.Security.SuppressUnmanagedCodeSecurity]
		[DllImport("TensorFlow", CallingConvention = global::System.Runtime.InteropServices.CallingConvention.Cdecl, EntryPoint = "TF_SessionRun")]
		static extern unsafe void TF_SessionRun2(IntPtr session, IntPtr run_options, IntPtr inputs, IntPtr input_values, int ninputs, IntPtr outputs, IntPtr output_values, int noutputs, IntPtr target_opers, int ntargets, IntPtr run_metadata, [Out] IntPtr status);

		public static TF_Session New(TF_Graph graph)
		{
			var status = tf_status.TF_NewStatus();
			var opts = c_api.TF_NewSessionOptions();
			var s =  c_api.TF_NewSession(graph, opts, status) ?? throw new Exception($"Could not create new TF_Session for graph {graph.NameScope}.");
			if (tf_status.TF_GetCode(status) == TF_Code.TF_OK)
			{
				//c_api.TF_DeleteSessionOptions(opts);
				return s;
			}
			else
			{
				throw new Exception($"Could not create new TF_Session for graph {graph.NameScope}: {tf_status.TF_Message(status)}.");
			}
		}

		public TF_Status Run(TF_Output[] inputs, TF_Tensor[] inputValues, TF_Output[] outputs, TF_Tensor[] outputValues, TF_Operation[] targets = null, TF_Buffer runMetadata = null, TF_Buffer runOptions = null)
		{
			var status = tf_status.TF_NewStatus();
			var _inputs = inputs.Select(i => i.__Instance).ToArray();
			var _inputValues = inputValues.Select(i => i.__Instance).ToArray();
			var _outputs = outputs.Select(i => i.__Instance).ToArray();
			var _outputValues = outputValues.Select(i => i.__Instance).ToArray();
			var _targets = targets?.Select(i => i.__Instance).ToArray();
			
			var ro = new Buffer(new byte[0]);
			fixed (IntPtr* _i = _inputs)
			fixed (IntPtr* _iv = _inputValues)
			fixed (IntPtr* _o = _outputs)
			fixed (IntPtr* _ov = _outputValues)
			fixed (IntPtr* _t = _targets)
			{
				TF_SessionRun2(
					this.__Instance,
					IntPtr.Zero,
					inputs[0].__Instance,
					inputValues[0].__Instance,
					0,
					outputs[0].__Instance,
					outputValues[0].__Instance,//outputValues[0].__Instance,
					0,
					targets[0].__Instance,
					1,
					IntPtr.Zero,
					status.__Instance
				);
			}
			//c_api.TF_SessionRun
			//TF_SessionRun2(this, null, inputs, inputValues, inputs.Length, outputs, outputValues, outputs.Length, null, 0, null, status);
			var msg = tf_status.TF_Message(status);
			return status;
		}
	}
}
