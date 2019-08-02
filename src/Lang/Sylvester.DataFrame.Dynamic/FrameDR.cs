using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Dynamic;
using System.Runtime.CompilerServices;
using System.Linq;
using System.Text;

using Microsoft.CSharp.RuntimeBinder;

namespace Sylvester
{
    public class FrameDR : DynamicObject, IEnumerable
    {
        public FrameDR(Frame f, int index, params ISeries[] series)
        {
            Frame = f;
            Series = series ?? throw new ArgumentNullException();
            Index = index;
            Columns = new Dictionary<string, ISeries>(Series.Length);
            for (int i = 0; i < Series.Length; i++)
            {
                Columns.Add(Series[i].Label, Series[i]);
            }
            foreach (var c in Columns)
            {
                AddCallSite(c.Key);
            }
            Enumerator = new FrameDREnumerator(this);
        }

        public Frame Frame { get; }

        public ISeries[] Series { get; }

        public Dictionary<string, ISeries> Columns { get; }
        public int Index { get; }

        internal Dictionary<string, CallSite<Func<CallSite, object, object>>> CallSites =
            new Dictionary<string, CallSite<Func<CallSite, object, object>>>();

        public FrameDREnumerator Enumerator { get; }

        public dynamic this[string column] => Columns[column].GetVal(Index);

        public dynamic this[int i]
        {
            get => Series[i].GetVal(Index);
        }
        public IEnumerator GetEnumerator() => Enumerator;

        public override bool TryGetMember(GetMemberBinder binder, out object result)
        {
            result = null;
            if (Columns.ContainsKey(binder.Name))
            {
                if (!CallSites.ContainsKey(binder.Name))
                {
                    AddCallSite(binder.Name);
                }
                result = GetMember(binder.Name);
                return true;
            }
            else
            {
                return false;
            }
        }
        public override bool TrySetMember(SetMemberBinder binder, object value) => false;

        public FrameDR Sel(params ISeries[] series) => new FrameDR(this.Frame, this.Index, series);

        public FrameDR Sel(params string[] series) => Sel(Series.Where(s => series.Contains(s.Label)).ToArray());

        public FrameDR Sel(params int[] series) => Sel(series.Select(i => Series[i]).ToArray());

        internal dynamic GetMember(string propName)
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

        public bool MoveNext() => (++position < row.Columns.Count);

        public void Reset() => position = -1;

        dynamic IEnumerator.Current => row[position];
    }   
}
