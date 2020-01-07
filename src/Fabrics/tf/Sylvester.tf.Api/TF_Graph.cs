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

using Google.Protobuf;
using Protobuf.Text;
using Tensorflow;

namespace TensorFlow
{
	public unsafe partial class TF_Graph : TF_Native, ITensorFlowOps
	{
		#region Overriden members
		public override IntPtr NativePtr => this.__Instance;
		public override bool OwnsNativeMemory => this.__ownsNativeInstance;
		public override void Delete()
		{
			if (!this.__ownsNativeInstance)
			{
				c_api.TF_DeleteGraph(this);
				this.__Instance = IntPtr.Zero;
			}
		}
		#endregion

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
				return MakeUniqueName(string.IsNullOrEmpty(NameScope) || NameScope == "_" ? customOpName : NameScope + "/" + customOpName);
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
			if (string.IsNullOrEmpty(NameScope))
			{
				return name + "_" + ids[name];
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

		internal TF_Buffer ToGraphDefBuffer()
		{
			var buf = c_api.TF_NewBuffer();
			var status = tf_status.TF_NewStatus();
			c_api.TF_GraphToGraphDef(this, buf, status);
			return buf;
		}

		public GraphDef ToGraphDef()
		{
			TF_Buffer b = this.ToGraphDefBuffer();
			var stream = CodedInputStream.CreateWithLimits((UnmanagedMemoryStream)b, 256 * 1024 * 1024, 100);
			var def =  GraphDef.Parser.ParseFrom(stream);
			b.Delete();
			return def;
		}

		public void ExportToBinaryFile(string filePath) => ToGraphDef().WriteTo(File.Open(filePath, FileMode.Create));

		public void Export(string filePath) => File.WriteAllText(filePath, ToGraphDef().ToText());

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
				while ((op = c_api.TF_GraphNextOperation(graph, ref pos)) != null)
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

		public static GraphDef Import(string filePath) => GraphDef.Parser.ParseFrom(CodedInputStream.CreateWithLimits(File.OpenRead(filePath), 256 * 1024 * 1024, 100));

		
		#endregion

		#region Fields
		Dictionary<string, int> ids = new Dictionary<string, int>();
		#endregion
	}
}