using System;

using Microsoft.ML;
using Sylvester.Data;

namespace Sylvester.DataFrame.MLnet
{
    public class DataFrameView : IDataView
    {
        public DataFrameView(Frame frame)
        {
            var builder = new DataViewSchema.Builder();
            //builder.AddColumn()
            var ann = new DataViewSchema.Annotations.Builder();
            
        }


        private bool _canShuffle;
        private DataViewSchema _schema;
    }
}
