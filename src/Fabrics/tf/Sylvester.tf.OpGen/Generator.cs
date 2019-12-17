using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

using ProtoBuf;
using TensorFlow;
using tensorflow;

namespace Sylvester.tf.OpGen
{
    public class Generator
    {
		public static List<OpDef> GetOpsList()
		{
			unsafe
			{
				var status = tf_status.TF_NewStatus();
				TF_Buffer buffer = c_api.TF_GetAllOpList();
				var handle = c_api.TF_NewApiDefMap(buffer, status);
				if (tf_status.TF_GetCode(status) != TF_Code.TF_OK)
				{
					throw new Exception($"Could not create ApiDefMap: {tf_status.TF_Message(status)}");
				}
				var ret = new byte[(int) buffer.Length];
				Marshal.Copy(buffer.Data, ret, 0, (int) buffer.Length);
				return Serializer.Deserialize<List<OpDef>>(new MemoryStream(ret));
			}
		}
		/*
		public unsafe ApiDef Get(string name)
		{
			var status = tf_status.TF_NewStatus();
			var ptr = c_api.TF_ApiDefMapGet//TF_ApiDefMapGet(handle, name, (IntPtr)name.Length, status);
				if (status.Error)
					return null;
				var ret = new byte[(int)ptr->length];
				Marshal.Copy(ptr->data, ret, 0, (int)ptr->length);
				var str = new MemoryStream(ret);
				return Serializer.Deserialize<ApiDef>(str);
			
		}
		*/
		#region Fields
		//ApiDefMap apimap;
		//TF_Status status;
		//TF_ApiDefMap handle;
		// The output file
		StreamWriter output;
		int indent = 0;
        #endregion

        #region Methods
        // Convenience methods to generate output
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
				output.Write("\t");
			if (args.Length == 0)
				output.WriteLine(fmt);
			else
				output.WriteLine(fmt, args);
		}
        #endregion
    }
}
