using System;
using System.Collections.Generic;

using Microsoft.ML;
using Microsoft.ML.Data;


namespace Sylvester.Data
{
    public class DataFrameView : FrameV<int>, IDataView
    {
        public DataFrameView(Frame frame) : base(frame.Rows, (index, view) => index)
        {
            var builder = new DataViewSchema.Builder();
            foreach(IColumn c in frame.Columns)
            {
                var ann = new DataViewSchema.Annotations.Builder();
                a
                builder.AddColumn(c.Label, NumberDataViewType)
            }
            //builder.AddColumn()
            //var ann = new DataViewSchema.Annotations.Builder();
            //ann.("foo", NumberDataViewType.)
            //ann.Add("foo")
            
        }


        private bool _canShuffle;
        private DataViewSchema _schema;

        private DataViewType GetColumnDataViewType(Type type)
        {
            switch(type.Name)
            {
                case "String":
                    return 
                    break;
                case "DateTime":
                    Add(new Cd((DateTime[])data, label));
                    break;
                case "Byte":
                    Add(new Cn<byte>((byte[])data, label));
                    break;
                case "SByte":
                    Add(new Cn<sbyte>((sbyte[])data, label));
                    break;
                case "UInt16":
                    Add(new Cn<ushort>((ushort[])data, label));
                    break;
                case "Int16":
                    Add(new Cn<short>((short[])data, label));
                    break;
                case "UInt32":
                    Add(new Cn<uint>((uint[])data, label));
                    break;
                case "Int32":
                    Add(new Cn<int>((int[])data, label));
                    break;
                case "UInt64":
                    Add(new Cn<ulong>((ulong[])data, label));
                    break;
                case "Int64":
                    Add(new Cn<long>((long[])data, label));
                    break;
                case "Single":
                    Add(new Cn<float>((float[])data, label));
                    break;
                case "Double":
                    Add(new Cn<double>((double[])data, label));
                    break;
                case "Decimal":
                    Add(new Cn<decimal>((decimal[])data, label));
                    break;
                case "Boolean":
                    Add(new Cn<bool>((bool[])data, label));
                    break;
            }

        }
    }
}
