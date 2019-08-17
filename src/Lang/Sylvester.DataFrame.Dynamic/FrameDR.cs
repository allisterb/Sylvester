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
    public class FrameDR : DynamicObject, IRow
    {
        #region Constructors
        public FrameDR()
        {
            Enumerator = new FrameDREnumerator(this);
        }

        public FrameDR(Frame f, int index, params IColumn[] columns) : this()
        {
            Frame = f;
            FrameColumnsArray = columns ?? throw new ArgumentNullException();
            Index = index;
            for (int i = 0; i < FrameColumnsArray.Length; i++)
            {
                FrameColumns.Add(FrameColumnsArray[i].Label, FrameColumnsArray[i]);
            }
            foreach (var c in FrameColumns)
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
        #endregion

        #region Properties
        public Frame Frame { get; }

        public IColumn[] FrameColumnsArray { get; }

        public IEnumerable<string> ColumnLabels
        {
            get
            {
                return FrameColumns.Keys.Concat(CustomColumns.Keys);
            }
        }

        public Dictionary<string, IColumn> FrameColumns { get; } = new Dictionary<string, IColumn>();

        public Dictionary<string, dynamic> CustomColumns { get; } = new Dictionary<string, dynamic>();

        public int Index { get; }

        internal Dictionary<string, CallSite<Func<CallSite, object, object>>> CallSites =
            new Dictionary<string, CallSite<Func<CallSite, object, object>>>();

        public FrameDREnumerator Enumerator { get; }

        public dynamic this[string column]
        {
            get
            {
                if (FrameColumns.ContainsKey(column))
                {
                    return FrameColumns[column].GetVal(Index); ;
                }
                else if (CustomColumns.ContainsKey(column))
                {
                    return CustomColumns[column];
                }
                else throw new IndexOutOfRangeException($"Column {column} does not exist.");
                
            }
            set
            {
                if (FrameColumns.ContainsKey(column))
                {
                    FrameColumns[column].SetVal(Index, value); ;
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
                if (i <= (FrameColumns.Count - 1))
                {
                    return FrameColumnsArray[i].GetVal(Index);
                }
                else if (i <= FrameColumns.Count + CustomColumns.Count - 1)
                {
                    return CustomColumns.Values.ElementAt(i - FrameColumns.Count);
                }
                else throw new IndexOutOfRangeException($"Index {i} does not exist.");

            }

            set
            {
                if (i <= (FrameColumns.Count - 1))
                {
                    FrameColumnsArray[i].SetVal(Index, value);
                }
                else if (i <= FrameColumns.Count + CustomColumns.Count - 1)
                {
                    CustomColumns[CustomColumns.Keys.ElementAt(i - FrameColumns.Count)] = value;
                }
                else throw new IndexOutOfRangeException($"Index {i} does not exist.");
            }
        }
        #endregion

        #region Overriden members
        public override bool TryGetMember(GetMemberBinder binder, out object result)
        {
            result = null;
            if (FrameColumns.ContainsKey(binder.Name))
            {
                if (!CallSites.ContainsKey(binder.Name))
                {
                    AddCallSite(binder.Name);
                }
                result = GetFrameColumnMember(binder.Name);
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
            if (FrameColumns.ContainsKey(binder.Name))
            {
                IColumn s = (IColumn)CallSites[binder.Name].Target(CallSites[binder.Name], this.Frame);
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
        #endregion

        #region Methods
        public IEnumerator GetEnumerator() => Enumerator;

        public FrameDR Sel(params IColumn[] columns) => new FrameDR(this.Frame, this.Index, columns);

        public FrameDR Sel(params string[] columns)
        {
            var notfound = columns.Where(c => !FrameColumns.ContainsKey(c) || !CustomColumns.ContainsKey(c));
            if (notfound.Count() != 0)
            {
                throw new ArgumentException($"The following columns do not exist in the row: {notfound.Aggregate((s1, s2) => s1 + "," + s2)}.");
            }
            var frameColumns = FrameColumns.Where(c => columns.Contains(c.Key));
            FrameDR dr = null;
            if (columns.Count() != 0)
            {
                dr = new FrameDR(this.Frame, this.Index, frameColumns.Select(c => c.Value).ToArray());
            }
            else
            {
                dr = new FrameDR();
            }
            var customColumns = CustomColumns.Where(c => columns.Contains(c.Key));
            foreach (var c in customColumns)
            {
                dr.Add((c.Key, c.Value));
            }

            return dr;
        }

        public FrameDR Sel(params int[] columns) => Sel(columns.Select(i => FrameColumnsArray[i]).ToArray());

        public FrameDR Ex(params string[] columns)
        {
            var dr = new FrameDR(this.Frame, this.Index,
              this.FrameColumns.Where(s => !columns.Contains(s.Key)).Select(c => c.Value).ToArray());
            foreach (var kv in this.CustomColumns.Where(s => !columns.Contains(s.Key)))
            {
                dr.Add((kv.Key, kv.Value));
            }
            return dr;
        }

        public FrameDR Ex(params IColumn[] columns) => new FrameDR(this.Frame, this.Index, 
            this.FrameColumnsArray.Except(columns).ToArray());

        public IRow Cols(params string[] columns) => Sel(columns);

        public IRow ColsEx(params string[] columns) => Ex(columns);

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

        public FrameDR Add(Dictionary<string, dynamic> values)
        {
            foreach (var kv in values)
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

        internal dynamic GetFrameColumnMember(string propName)
        {
            IColumn s = (IColumn) CallSites[propName].Target(CallSites[propName], this.Frame);
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
        #endregion

        #region Fields
        private object _lock = new object();
        #endregion
    }

    public class FrameDREnumerator : IEnumerator
    {
        public FrameDREnumerator(FrameDR r)
        {
            row = r;
        }

        FrameDR row;
        int position = -1;

        public bool MoveNext() => (++position < (row.FrameColumns.Count + row.CustomColumns.Count));

        public void Reset() => position = -1;

        dynamic IEnumerator.Current => row[position];
    }   
}
