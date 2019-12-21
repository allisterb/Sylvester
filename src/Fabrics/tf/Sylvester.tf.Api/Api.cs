using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;

using System.Linq;

namespace TensorFlow
{
	public unsafe partial  class c_api
	{
		public static void TF_SetAttrString(global::TensorFlow.TF_OperationDescription desc, string attr_name, string attr_value)
		{
			ulong length = Convert.ToUInt64(attr_value.Length);
			var ptr = Marshal.StringToHGlobalAnsi(attr_value);
			TF_SetAttrString(desc, attr_name, ptr, length);
		}

		public static void TF_SetAttrStringList(global::TensorFlow.TF_OperationDescription desc, string attr_name, string[] attr_values)
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

		public static void TF_SetAttrTypeList(global::TensorFlow.TF_OperationDescription desc, string attr_name, global::TensorFlow.TF_DataType[] values)
		{
			fixed (TF_DataType* ptr = &values[0])
			{
				TF_SetAttrTypeList(desc, attr_name, ptr, values.Length);
			}
		}

		public static void TF_SetAttrShapeList(TF_OperationDescription desc, string attr_name, long[][] dims)
		{
			int[] num_dims = new int[dims.Length];
			for (int i = 0; i < dims.Length; i++)
			{
				num_dims[i] = dims[i].Length;
			}
			fixed(long* ptr = &dims[0][0])
			{
				c_api.TF_SetAttrShapeList(desc, attr_name, (long**)ptr, ref num_dims[0], dims.Length);
			}
		}
	}
	public unsafe partial class TF_Graph
    {
		public TF_Operation[] Dependencies { get; internal set; }

		public string CurrentNameScope { get; internal set; }

		
		internal string MakeName(string operName, string userName)
		{
			if (userName == null)
			{
				var k = CurrentNameScope == "" ? operName : CurrentNameScope + "/" + operName;

				return MakeUnique(k);
			}
			if (CurrentNameScope == "")
				return userName;
			return CurrentNameScope + "/" + userName;
		}

		string MakeUnique(string name)
		{
			int val = 0;

			if (!values.TryGetValue(name, out val))
				val = 0;
			else
				val++;
			values[name] = val;
			return name + val;
		}

		#region Fields
		Dictionary<string, int> values = new Dictionary<string, int>();
		internal int LastId;
        #endregion
        internal int GetNextId()
		{
			return LastId++;
		}

	}

	public unsafe partial class TF_Output
	{
		public TF_Output(TF_Operation oper, int index) : this()
		{
			Oper = oper;
			Index = index;
		}
	}

	/// <summary>
	/// TFGraph variable dependencies handle.
	/// </summary>
	/// <remarks>
	/// Instances of this class, when disposed, restore <see cref="TFGraph.CurrentDependencies"/>
	/// to the value it had before the <see cref="TFGraph.WithDependencies(TFOperation[])"/> method
	/// was called.
	/// </remarks>
	/// <seealso cref="System.IDisposable" />
	public class Dependencies : IDisposable
	{
		TF_Graph container;
		TF_Operation[] parentDependencies;
		TF_Operation[] dependencies;

		internal Dependencies(TF_Graph container, TF_Operation[] dependencies)
		{
			this.container = container;
			this.parentDependencies = container.Dependencies;
			this.dependencies = dependencies;

			container.Dependencies = container.Dependencies.Concat(dependencies).Distinct().ToArray();
		}

		/// <summary>
		/// Pops the variable dependencies to the previous dependencies in use.
		/// </summary>
		/// <remarks>Call <see cref="Dispose"/> when you are finished using the <see cref="T:TensorFlow.TFDependencies"/>
		/// to restore the previous variable dependencies in use in the <see cref="T:TensorFlow.TFGraph"/>.
		/// </remarks>
		public void Dispose()
		{
			container.Dependencies = parentDependencies;
		}
	}
}
