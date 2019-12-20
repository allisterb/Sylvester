using System;
using System.Collections.Generic;
using System.Text;

using System.Linq;

namespace TensorFlow
{
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
}
