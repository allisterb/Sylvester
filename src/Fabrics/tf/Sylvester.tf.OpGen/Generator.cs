using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Linq;
using System.Text;

using Serilog;
using ProtoBuf;
using TensorFlow;
using tensorflow;

namespace Sylvester.tf.OpGen
{
    public class Generator
    {
		#region Constructors
		public Generator(string outputFile = "Ops.g.cs")
		{
			OutputFile = new FileInfo(outputFile);
			if (OutputFile.Exists)
			{
				L.Warning("The output file {0} exists and will be overwritten.", OutputFile.FullName);
			}
			else
			{
				L.Information("Using output file {0}.", OutputFile.FullName);
			}
			var status = tf_status.TF_NewStatus();
			TF_Buffer opsBuffer = c_api.TF_GetAllOpList();
			if (tf_status.TF_GetCode(status) != TF_Code.TF_OK)
			{
				L.Error("Could not get op list: {0}.", tf_status.TF_Message(status));
				Program.Exit(Program.ExitResult.TF_ERROR);

			}
			var ret = new byte[(int)opsBuffer.Length];
			Marshal.Copy(opsBuffer.Data, ret, 0, (int)opsBuffer.Length);
			OpDefs =  Serializer.Deserialize<List<OpDef>>(new MemoryStream(ret));
			ApiDefMap = c_api.TF_NewApiDefMap(opsBuffer, status);
			if (tf_status.TF_GetCode(status) != TF_Code.TF_OK)
			{
				L.Error("Could not create new ApiDefMap: {0}.", tf_status.TF_Message(status));
				Program.Exit(Program.ExitResult.TF_ERROR);
			}
			outputWriter = new StringWriter(OutputBuilder);
			L.Information("Created generator.");
		}
		#endregion

		#region Properties
		ILogger L = Log.Logger;
		public TF_ApiDefMap ApiDefMap { get; }
		public List<OpDef> OpDefs { get; }
		public StringBuilder OutputBuilder { get; } = new StringBuilder();
		public string Output => OutputBuilder.ToString();

		public FileInfo OutputFile { get; }
		#endregion

        #region Methods
		public void Run(string[] dirs, string opName)
		{			
			if (dirs.Length > 0)
			{
				UpdateApis(dirs);
			}
			p("using System;\n");
			pi("namespace TensorFlow {");
			pi("public partial class TF_Graph {");
			if (!string.IsNullOrEmpty(opName))
			{
				L.Information("Generating code for op {0}.", opName);
				var op = OpDefs.First(o => o.name == opName);
				try
				{
					Generate(op);
				}
				catch (OpGenException e)
				{
					L.Error(e.Message);
				}
			}
			else
			{
				foreach( var op in OpDefs)
				{
					// Skip internal operations
					if (op.name.StartsWith("_"))
						continue;

					// Ignore functions where we lack a C# type mapping
					if (op.attr.Any(attr => CSharpType(attr.type) == null))
					{
						var attr = op.attr.First(a => CSharpType(a.type) == null);

						L.Error($"Skipping {op.name} due to attribute {attr.type} {attr.name} lacking a mapping to a C# type");
						continue;
					}
					if (op.output_arg.Any(arg => IsListArg(arg)))
					{
					
							L.Error($"Skipping {op.name} due to output arg ({op.output_arg.First(arg => IsListArg(arg)).name}) being a list type.");
							continue;
						
					}
							/*
							var def = ApiDefMap.(oper.name);

							// Undocumented operation, perhaps we should not surface
							if (def.Summary == "")
								continue;
							*/
							try
							{
						Generate(op);
					}
					catch(OpGenException e)
					{
						L.Error(e.Message);
						continue;
					}
					catch(UnknownTypeException te)
					{
						L.Error(te.Message);
						continue;
					}
				}
			}
			pd("}");
			pd("}");
			outputWriter.Close();
			outputWriter.Flush();
			File.WriteAllText(OutputFile.FullName, Output);
			L.Information("Wrote {0} characters to {1}.", OutputBuilder.Length, OutputFile.FullName);
		}

		void UpdateApis(string[] dirs)
		{
			foreach (var dir in dirs)
			{
				var files = Directory.GetFiles(dir);
				L.Information("Adding {0} op defs from directory {1}.", files.Length, dir);
				foreach (var f in Directory.GetFiles(dir))
				{
					var s = File.ReadAllText(f);
					PutApiDef(s);
				}
			}
		}

		public unsafe ApiDef GetApiDef(string name)
		{
			var status = tf_status.TF_NewStatus();
			var buffer = c_api.TF_ApiDefMapGet(ApiDefMap, name, (ulong) name.Length, status);
			if (tf_status.TF_GetCode(status) != TF_Code.TF_OK)
			{
				L.Error("Could not get api definition {0} from map.", tf_status.TF_Message(status));
				return null;
			}
			var ret = new byte[(int)buffer.Length];
			Marshal.Copy(buffer.Data, ret, 0, (int) buffer.Length);
			var str = new MemoryStream(ret);
			return Serializer.Deserialize<ApiDef>(str);	
		}

		public unsafe bool PutApiDef(string text)
		{
			var status = tf_status.TF_NewStatus();
			c_api.TF_ApiDefMapPut(ApiDefMap, text, (ulong)text.Length, status);
			if (tf_status.TF_GetCode(status) != TF_Code.TF_OK)
			{
				L.Error("Could not put api definition {0} in map.", tf_status.TF_Message(status));
				return false;
			}
			else
			{
				return true;
			}
		}

		//
		// Maps a TensorFlow type to a C# type
		//
		string CSharpType(string tfType)
		{
			bool list = false;
			string cstype;

			if (tfType.StartsWith("list("))
			{
				list = true;
				tfType = tfType.Substring(5, tfType.Length - 6);
			}
			switch (tfType)
			{
				case "int":
					cstype = "long"; break;
				case "float":
					cstype = "float"; break;
				case "bool":
					cstype = "bool"; break;
				case "type":
					cstype = "TF_DataType"; break;
				case "shape":
					cstype = "long[]"; break;
				case "tensor":
					cstype = "TF_Tensor"; break;
				case "string":
					cstype = "string"; break;
				default:
					return null;
			}

			return cstype + (list ? "[]" : "");
		}

		bool IsReferenceType(string tfType)
		{
			if (tfType.StartsWith("list("))
				return true;
			if (tfType == "tensor" || tfType == "string" || tfType == "shape")
				return true;
			return false;
		}

		// Maps a parameter name to a C# acceptable name, to avoid clashes with 
		// language keywords
		string ParamMap(string paramName)
		{
			switch (paramName)
			{
				case "out":
					return "output";
				case "params":
					return "parameters";
				case "ref":
					return "reference";
				case "event":
					return "evnt";
			}
			return paramName;
		}

		// Determines if the specified ArgDef represents a TensorFlow list
		bool IsListArg(OpDef.ArgDef arg)
		{
			return arg.type_list_attr != "" || arg.number_attr != "";
		}

		// 
		// These values are the result of calling SetupArguments
		//

		List<OpDef.AttrDef> required_attrs, optional_attrs;
		bool have_return_value;

		void SetupArguments(OpDef def)
		{
			// Attributes related to the InputArg's type are inferred automatically
			// and are not exposed to the client.
			var inferred_input_args = new Dictionary<string, bool>();
			required_attrs = new List<OpDef.AttrDef>();
			optional_attrs = new List<OpDef.AttrDef>();

			foreach (var argdef in def.input_arg)
			{
				if (argdef.type_attr != "")
					inferred_input_args[argdef.type_attr] = true;
				else if (argdef.type_list_attr != "")
					inferred_input_args[argdef.type_list_attr] = true;
				if (argdef.number_attr != "")
					inferred_input_args[argdef.number_attr] = true;
			}
			foreach (var attr in def.attr)
			{
				if (inferred_input_args.ContainsKey(attr.name))
					continue;
				if (attr.default_value == null)
					required_attrs.Add(attr);
				else
					optional_attrs.Add(attr);
			}
			have_return_value = def.output_arg.Count > 0;
		}

		// Generates arguments:
		//   * Input arguments (TFOutput or TFOutput [])
		//   * All required attributes
		//   * variadic optional arguments
		string FillArguments(OpDef def)
		{
			var sb = new StringBuilder();
			string comma = "";
			foreach (var inarg in def.input_arg)
			{
				string type = "TF_Output" + (IsListArg(inarg) ? "[]" : "");

				sb.AppendFormat($"{comma}{type} {ParamMap(inarg.name)}");
				comma = ", ";
			}
			foreach (var attr in required_attrs)
			{
				sb.AppendFormat($"{comma}{CSharpType(attr.type)} {ParamMap(attr.name)}");
				comma = ", ";
			}

#if false
		if (!return_is_tfoutput) {
			foreach (var arg in def.output_arg) {
				string type = "TFOutput" + (IsListArg (arg) ? "[]" : "");

				sb.AppendFormat ($"{comma}ref {type} {ParamMap (arg.name)}");
				comma = ", ";
			}
		}
#endif
			int n = 0;
			foreach (var attr in optional_attrs)
			{
				bool reftype = IsReferenceType(attr.type);
				var cstype = CSharpType(attr.type);
				var cstypesuffix = reftype ? "" : "?";

				sb.AppendFormat($"{comma}{cstype}{cstypesuffix} {attr.name} = null");
				comma = ", ";
			}
			if (sb.Length != 0)
				sb.Append(", ");
			return sb.ToString();
		}

		void Comment(string text)
		{
			if (text == null || text == "")
				return;
			var lines = text.Split('\n');
			var open = true;

			string Quote(string input)
			{
				var p = input.IndexOf('`');
				if (p == -1)
					return input;
				var res = new StringBuilder();
				foreach (var c in input)
				{
					if (c == '`')
					{
						res.Append(open ? "<c>" : "</c>");
						open = !open;
					}
					else
						res.Append(c);
				}
				return res.ToString();
			}

			bool blockOpen = true;
			foreach (var line in lines)
			{
				if (line.IndexOf("in image height coordinates.") != -1)
				{
					Console.WriteLine("Hello");
				}

				var line2 = line.Trim().Replace("<", "&lt;").Replace(">", "&gt;").Replace("&", "&amp;");

				if (line2.StartsWith("```"))
				{
					p("///    " + (blockOpen ? "<code>" : "</code>"));
					blockOpen = !blockOpen;
					if (line2 == "```python" || line2 == "```c++" || line2 == "```")
						continue;
					// Handle some broken comments in the api specs, they sometimes missuse the 

					line2 = line2.Substring(3);
					if (line2.EndsWith("```"))
					{
						var line3 = line2.Substring(0, line2.Length - 3);
						p($"///    {Quote(line3)}");
						p("///    " + (blockOpen ? "<code>" : "</code>"));
						blockOpen = !blockOpen;
						continue;
					}
				}
				p($"///   {Quote(line2)}");

			}
		}


		// Produces the C# inline documentation
		void GenDocs(OpDef oper)
		{
			var api = GetApiDef(oper.name);
			p("/// <summary>");
			Comment(api.Summary);
			p("/// </summary>");
			foreach (var input in api.InArgs)
			{
				p($"/// <param name=\"{ParamMap(input.Name)}\">");
				Comment(input.Description);
				p($"/// </param>");
			}
#if DOCS
		if (!return_is_tfoutput) {
			foreach (var attr in oper.output_arg) {
				if (String.IsNullOrEmpty (attr.description))
					continue;
				p ($"/// <param name=\"{ParamMap (attr.name)}\">");
				Comment (attr.description);
				p ($"/// </param>");
			}
		}
#endif
			p("/// <param name=\"operName\">");
			p($"///   If specified, the created operation in the graph will be this one, otherwise it will be named '{oper.name}'.");
			p("/// </param>");
			foreach (var attr in optional_attrs)
			{
				p($"/// <param name=\"{ParamMap(attr.name)}\">");
				Comment("Optional argument");

				Comment(api.Attrs.Where(x => x.Name == attr.name).FirstOrDefault().Description);
				p($"/// </param>");
			}
			foreach (var attr in required_attrs)
			{
				p($"/// <param name=\"{ParamMap(attr.name)}\">");
				Comment(api.Attrs.Where(x => x.Name == attr.name).FirstOrDefault().Description);
				p($"/// </param>");
			}
			p($"/// <returns>");
			if (have_return_value)
			{
				if (oper.output_arg.Count == 1)
				{
					Comment(api.OutArgs.First().Description);
					Comment("The TF_Operation can be fetched from the resulting TF_Output, by fethching the Operation property from the result.");
				}
				else
				{
					Comment("Returns a tuple with multiple values, as follows:");
					foreach (var arg in oper.output_arg)
					{
						var oapi = api.OutArgs.Where(x => x.Name == arg.name).FirstOrDefault();
						Comment(ParamMap(arg.name) + ": " + oapi.Description);
					}

					Comment("The TF_Operation can be fetched from any of the TFOutputs returned in the tuple values, by fethching the Operation property.");
				}
			}
			else
			{
				Comment("Returns the description of the operation");
			}
			p($"/// </returns>");

			if (!String.IsNullOrEmpty(api.Description))
			{
				p("/// <remarks>");
				Comment(api.Description);
				p("/// </remarks>");
			}
		}

		void SetAttribute(string type, string attrName, string csAttrName)
		{
			if (type == "shape")
			{
				p($"c_api.TF_SetAttrShape (desc, \"{attrName}\", ref {csAttrName}[0], {csAttrName}.Length);");
				return;
			}
			if (type.StartsWith("list(shape"))
			{
				p($"c_api.TF_SetAttrShapeList (desc, \"{attrName}\", {csAttrName});");
				return;
			}

			var cstype = CSharpType(type);
			switch (cstype)
			{
				case "long":
					p($"c_api.TF_SetAttrInt (desc, \"{attrName}\", {csAttrName});");
					break;
				case "long[]":
					p($"c_api.TF_SetAttrIntList (desc, \"{attrName}\", ref {csAttrName}[0], {csAttrName}.Length);");
					break;
				case "string":
					p($"c_api.TF_SetAttrString (desc, \"{attrName}\", {csAttrName});");
					break;
				case "string[]":
					p($"c_api.TF_SetAttrStringList (desc, \"{attrName}\", {csAttrName});");
					break;
				case "float":
					p($"c_api.TF_SetAttrFloat (desc, \"{attrName}\", {csAttrName});");
					break;
				case "float[]":
					p($"c_api.TF_SetAttrFloatList (desc, \"{attrName}\", ref {csAttrName}[0], {csAttrName}.Length);");
					break;
				case "bool":
					p($"c_api.TF_SetAttrBool (desc, \"{attrName}\", Convert.ToByte({csAttrName}));");
					break;
				case "bool[]":
					p($"c_api.TF_SetAttrBoolList (desc, \"{attrName}\", ref {csAttrName}[0], {csAttrName}.Length);");
					break;
				case "TF_DataType":
					p($"c_api.TF_SetAttrType (desc, \"{attrName}\", {csAttrName});");
					break;
				case "TF_DataType[]":
					p($"c_api.TF_SetAttrTypeList (desc, \"{attrName}\", {csAttrName});");
					break;
				// This should pass the cstatus, but requires the 
				// function to take a TFStatus as well, so need to weave that
				// in the parameters
				case "TF_Tensor":
					p($"c_api.TF_SetAttrTensor (desc, \"{attrName}\", {csAttrName}, status);");
					break;
				case "TF_Tensor[]":
					p($"c_api.TF_SetTensorList (desc, \"{attrName}\", ref {csAttrName}[0], {csAttrName}.Length, status);");
					break;
				default:
					throw new UnknownTypeException(cstype);
			}
		}

		/// <summary>
		/// Generate the specified oper.
		/// </summary>
		/// <param name="oper">Oper.</param>
		public void Generate(OpDef oper)
		{
			SetupArguments(oper);
			GenDocs(oper);

			var name = oper.name;
			string retType;

			if (have_return_value)
			{
				if (oper.output_arg.Count > 1)
				{
					var rb = new StringBuilder("(");
					foreach (var arg in oper.output_arg)
					{
						rb.AppendFormat("TF_Output{0} {1}, ", IsListArg(arg) ? "[]" : "", ParamMap(arg.name));
					}
					rb.Remove(rb.Length - 2, 2);
					rb.Append(")");
					retType = rb.ToString();
				}
				else
					retType = "TF_Output" + (IsListArg(oper.output_arg.First()) ? "[]" : "");
			}
			else
				retType = "TF_Operation";

			p($"public {retType} {name} ({FillArguments(oper)}string operName = null)");
			pi("{");
			bool needStatus = required_attrs.Concat(optional_attrs).Any(attr => attr.type.Contains("TF_Tensor"));
			p("var status = tf_status.TF_NewStatus();");
			p($"var desc = c_api.TF_NewOperation(this, \"{oper.name}\", MakeName (\"{oper.name}\", operName));");
			foreach (var arg in oper.input_arg)
			{
				if (IsListArg(arg))
					p($"c_api.TF_AddInputList(desc, {ParamMap(arg.name)}[0], {ParamMap(arg.name)}.Length);");
				else
					p($"c_api.TF_AddInput(desc, {ParamMap(arg.name)});");
			}

			pi("foreach ( TF_Operation control in Dependencies )");
			p("c_api.TF_AddControlInput(desc, control);");
			pd("");

			// If we have attributes
			if (required_attrs.Count > 0 || optional_attrs.Count > 0)
			{
				foreach (var attr in required_attrs)
				{
					SetAttribute(attr.type, attr.name, ParamMap(attr.name));
				}

				foreach (var attr in optional_attrs)
				{
					var reftype = IsReferenceType(attr.type);
					var csattr = ParamMap(attr.name);
					if (reftype)
						pi($"if ({csattr} != null)");
					else
						pi($"if ({csattr}.HasValue)");
					SetAttribute(attr.type, attr.name, csattr + (reftype ? "" : ".Value"));
					pd("");

				}
			}

			p("var op = c_api.TF_FinishOperation(desc, status);");
			if (oper.output_arg.Count() > 0)
				p("int _idx = 0;");
			if (oper.output_arg.Any(x => IsListArg(x)))
				p("int _n = 0;");
			foreach (var arg in oper.output_arg)
			{
				if (IsListArg(arg))
				{
					throw new OpGenException(oper, "List output type not yet supported");
					/*var outputs = new StringBuilder();
					p($"_n = op.OutputListLength (\"{ParamMap(arg.name)}\");");
					p($"var {ParamMap(arg.name)} = new TF_Output [_n];");
					pi("for (int i = 0; i < _n; i++)");
					p($"{ParamMap(arg.name)} [i] = new TF_Output (op, _idx++);");
					pd("");*/
				}
				else
				{
					p($"var {ParamMap(arg.name)} = new TF_Output (op, _idx++);");
				}
			}

			if (have_return_value)
			{
				if (oper.output_arg.Count == 1)
				{
					p($"return {ParamMap(oper.output_arg.First().name)};");
				}
				else
				{
					;
					p("return (" + oper.output_arg.Select(x => ParamMap(x.name)).Aggregate((i, j) => (i + ", " + j)) + ");");
				}
			}
			else
			{
				p("return op;");
			}
			pd("}\n");
		}

		void pi(string fmt, params object[] args)
		{
			p(fmt, args);
			indent++;
		}

		void pd(string fmt, params object[] args)
		{
			indent--;
			p(fmt, args);
		}

		void p(string fmt, params object[] args)
		{
			for (int i = 0; i < indent; i++)
				outputWriter.Write("\t");
			if (args.Length == 0)
				outputWriter.WriteLine(fmt);
			else
				outputWriter.WriteLine(fmt, args);
		}

		#endregion

		#region Fields
		StringWriter outputWriter;
		int indent = 0;
        #endregion
    }
}
