using System;

namespace TensorFlow {
	public partial class TF_Graph {
		/// <summary>
		/// </summary>
		/// <param name="x">
		/// </param>
		/// <param name="y">
		/// </param>
		/// <param name="operName">
		///   If specified, the created operation in the graph will be this one, otherwise it will be named 'Add'.
		/// </param>
		/// <returns>
		///   The TF_Operation can be fetched from the resulting TF_Output, by fethching the Operation property from the result.
		/// </returns>
		public TF_Output Add (TF_Output x, TF_Output y, string operName = null)
		{
			var status = tf_status.TF_NewStatus();
			var desc = c_api.TF_NewOperation(this, "Add", MakeName ("Add", operName));
			c_api.TF_AddInput(desc, x);
			c_api.TF_AddInput(desc, y);
			foreach ( TF_Operation control in Dependencies )
				c_api.TF_AddControlInput(desc, control);
			
			var op = c_api.TF_FinishOperation(desc, status);
			int _idx = 0;
			var z = new TF_Output (op, _idx++);
			return z;
		}

	}
}
