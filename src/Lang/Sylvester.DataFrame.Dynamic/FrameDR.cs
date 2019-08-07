using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Dynamic;
using System.Runtime.CompilerServices;
using System.Linq;
using System.Text;

using Microsoft.CSharp.RuntimeBinder;

namespace Sylvester.Data
{
    public class FrameDR : DynamicObject, IEnumerable
    {
        public FrameDR()
        {
            Enumerator = new FrameDREnumerator(this);
        }

        public FrameDR(Frame f, int index, params ISeries[] series) : this()
        {
            Frame = f;
            Series = series ?? throw new ArgumentNullException();
            Index = index;
            for (int i = 0; i < Series.Length; i++)
            {
                SeriesColumns.Add(Series[i].Label, Series[i]);
            }
            foreach (var c in SeriesColumns)
            {
                AddCallSite(c.Key);
            }
        }

        public FrameDR(Dictionary<string, dynamic> values) : this()
        {
            this.CustomColumns = values ?? throw new ArgumentNullException("values");
        }

        public FrameDR(params ValueTuple<string, dynamic>[] values) :this()
        {
            CustomColumns = new Dictionary<string, dynamic>();
            for (int i = 0; i < values.Length; i++)
            {
                CustomColumns.Add(values[i].Item1, values[i].Item2);
            }
        }
        public Frame Frame { get; }

        public ISeries[] Series { get; }

        public Dictionary<string, ISeries> SeriesColumns { get; } = new Dictionary<string, ISeries>();

        public Dictionary<string, dynamic> CustomColumns { get; } = new Dictionary<string, dynamic>();

        public int Index { get; }

        internal Dictionary<string, CallSite<Func<CallSite, object, object>>> CallSites =
            new Dictionary<string, CallSite<Func<CallSite, object, object>>>();

        public FrameDREnumerator Enumerator { get; }

        public dynamic this[string column]
        {
            get
            {
                if (SeriesColumns.ContainsKey(column))
                {
                    return SeriesColumns[column].GetVal(Index); ;
                }
                else if (CustomColumns.ContainsKey(column))
                {
                    return CustomColumns[column];
                }
                else throw new IndexOutOfRangeException($"Column {column} does not exist.");
                
            }
            set
            {
                if (SeriesColumns.ContainsKey(column))
                {
                    SeriesColumns[column].SetVal(Index, value); ;
                }
                else if (CustomColumns.ContainsKey(column))
                {
                    CustomColumns[column] = value;
                }
                else throw new IndexOutOfRangeException($"Column {column} does not exist.");

            }
        }

        public dynamic this[int i]
        {
            get
            {
                if (i <= (SeriesColumns.Count - 1))
                {
                    return Series[i].GetVal(Index);
                }
                else if (i <= SeriesColumns.Count + CustomColumns.Count - 1)
                {
                    return CustomColumns.Values.ElementAt(i - SeriesColumns.Count);
                }
                else throw new IndexOutOfRangeException($"Index {i} does not exist.");

            }

            set
            {
                if (i <= (SeriesColumns.Count - 1))
                {
                    Series[i].SetVal(Index, value);
                }
                else if (i <= SeriesColumns.Count + CustomColumns.Count - 1)
                {
                    CustomColumns[CustomColumns.Keys.ElementAt(i - SeriesColumns.Count)] = value;
                }
                else throw new IndexOutOfRangeException($"Index {i} does not exist.");
            }
        }
        public IEnumerator GetEnumerator() => Enumerator;

        public override bool TryGetMember(GetMemberBinder binder, out object result)
        {
            result = null;
            if (SeriesColumns.ContainsKey(binder.Name))
            {
                if (!CallSites.ContainsKey(binder.Name))
                {
                    AddCallSite(binder.Name);
                }
                result = GetSeriesMember(binder.Name);
                return true;
            }
            else if (CustomColumns.ContainsKey(binder.Name))
            {
                result = CustomColumns[binder.Name];
                return true;
            }
            else
            {
                return false;
            }
        }
        public override bool TrySetMember(SetMemberBinder binder, object value)
        {
            if (SeriesColumns.ContainsKey(binder.Name))
            {
                ISeries s = (ISeries)CallSites[binder.Name].Target(CallSites[binder.Name], this.Frame);
                return s.SetVal(Index, value);
            }
            else if (CustomColumns.ContainsKey(binder.Name))
            {
                return CustomColumns[binder.Name] = value;
            }
            else
            {
                CustomColumns.Add(binder.Name, value);
                return true;
            }
        }

        public FrameDR Ser(params ISeries[] series) => new FrameDR(this.Frame, this.Index, series);

        public FrameDR Ser(params string[] series) => Ser(Series.Where(s => series.Contains(s.Label) || series.Contains(s.Label)).ToArray());

        public FrameDR Ser(params int[] series) => Ser(series.Select(i => Series[i]).ToArray());

        public FrameDR Ex(params ISeries[] series) => new FrameDR(this.Frame, this.Index, 
            this.Series.Except(series).ToArray());

        public FrameDR Ex(params string[] series) => new FrameDR(this.Frame, this.Index,
            this.Series.Where(s => !series.Contains(s.Label)).ToArray());

        public FrameDR Add(Dictionary<string, dynamic> values)
        {
            foreach(var kv in values)
            {
                if (CustomColumns.ContainsKey(kv.Key))
                {
                    CustomColumns[kv.Key] = kv.Value;
                }
                else
                {
                    CustomColumns.Add(kv.Key, kv.Value);
                }
                
            }
            return this;
        }

        public FrameDR Add(params ValueTuple<string, dynamic>[] values)
        {
            for (int i = 0; i < values.Length; i++)
            {
                if (CustomColumns.ContainsKey(values[i].Item1))
                {
                    CustomColumns[values[i].Item1] = values[i].Item2;
                }
                else
                {
                    CustomColumns.Add(values[i].Item1, values[i].Item2);
                }
            }
            return this;
        }

        public FrameDR Sel(params string[] columns)
        {
            var notfound = columns.Where(c => !SeriesColumns.ContainsKey(c) || !CustomColumns.ContainsKey(c));
            if (notfound.Count() != 0)
            {
                throw new ArgumentException($"The following columns do not exist in the row: {notfound.Aggregate((s1, s2) => s1 + "," + s2)}.");
            }
            var series = SeriesColumns.Where(c => columns.Contains(c.Key));
            FrameDR dr = null;
            if (series.Count() != 0)
            {
                dr = new FrameDR(this.Frame, this.Index, series.Select(c => c.Value).ToArray());

            }
            else
            {
                dr = new FrameDR();
            }
            var custom = CustomColumns.Where(c => columns.Contains(c.Key));
            foreach (var c in custom)
            {
                dr.Add((c.Key, c.Value));
            }
            
            return dr;
        }

        internal dynamic GetSeriesMember(string propName)
        {
            ISeries s = (ISeries) CallSites[propName].Target(CallSites[propName], this.Frame);
            return s.GetVal(Index);
        }

        private void AddCallSite(string propName)
        {
            var binder = Binder.GetMember(CSharpBinderFlags.None, propName, this.GetType(),
                new List<CSharpArgumentInfo>{
                       CSharpArgumentInfo.Create(CSharpArgumentInfoFlags.None, null)});
            var callsite = CallSite<Func<CallSite, object, object>>.Create(binder);
            CallSites.Add(propName, callsite);
        }

        private object _lock = new object();
    }

    public class FrameDREnumerator : IEnumerator
    {
        public FrameDREnumerator(FrameDR r)
        {
            row = r;
        }

        FrameDR row;
        int position = -1;

        public bool MoveNext() => (++position < (row.SeriesColumns.Count + row.CustomColumns.Count));

        public void Reset() => position = -1;

        dynamic IEnumerator.Current => row[position];
    }   
}
