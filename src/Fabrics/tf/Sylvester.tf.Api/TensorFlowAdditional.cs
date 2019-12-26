using System;
using System.Collections.Generic;
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
	public unsafe partial class TF_Graph
    {
        #region Constructors
        public TF_Graph(IntPtr ptr)
		{
			TF_Graph.__CreateInstance(ptr);
		}
		#endregion

        #region Properties
        public TF_Operation[] Dependencies { get; internal set; }

		public string NameScope { get; internal set; }
		#endregion

		#region Methods
		public void SetNameScope(string s)
		{
			if (string.IsNullOrEmpty(NameScope))
			{
				NameScope = s;
			}
			else
			{
				throw new InvalidOperationException($"The scope name for this graph is already set to {NameScope}");
			}
		}

		public string MakeName(string opName, string customOpName = null)
		{
			if (string.IsNullOrEmpty(customOpName))
			{
				return MakeUniqueName(NameScope == "" ? opName : NameScope + "/" + opName);

			}
			else
			{
				return MakeUniqueName(NameScope == "" ? opName : NameScope + "/" + customOpName);
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
}
