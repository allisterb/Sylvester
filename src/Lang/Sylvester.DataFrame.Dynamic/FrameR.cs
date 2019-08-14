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
    public class FrameR : DynamicObject, IEnumerable
    {
        public FrameR(Frame f, int index)
        {
            Frame = f;
            Index = index;
            foreach (var c in Frame.ColumnLabels)
            {
                AddCallSite(c.Key);
            }
            Enumerator = new FrameREnumerator(this);
        }
        public Frame Frame { get; }

        public int Index { get; }

        internal Dictionary<string, CallSite<Func<CallSite, object, object>>> CallSites =
            new Dictionary<string, CallSite<Func<CallSite, object, object>>>();

        public FrameREnumerator Enumerator { get; }

        public dynamic this[string column] => Frame.ColumnLabels[column].GetVal(Index);

        public dynamic this[int i]
        {
            get => Frame.Columns[i].GetVal(Index);
        }
        public IEnumerator GetEnumerator() => Enumerator;

        public override bool TryGetMember(GetMemberBinder binder, out object result)
        {
            result = null;
            if (Frame.ColumnLabels.ContainsKey(binder.Name))
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

        public FrameDR Col(params IColumn[] columns) => new FrameDR(this.Frame, this.Index, columns);

        public FrameDR Col(params string[] columns) => Col(Frame.Columns.Where(s => columns.Contains(s.Label)).ToArray());

        public FrameDR Col(params int[] columns) => Col(columns.Select(i => Frame.Columns[i]).ToArray());

        internal dynamic GetMember(string propName)
        {
            IColumn c = (IColumn) CallSites[propName].Target(CallSites[propName], this.Frame);
            return c.GetVal(Index);
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

    public class FrameREnumerator : IEnumerator
    {
        public FrameREnumerator(FrameR r)
        {
            row = r;
        }

        FrameR row;
        int position = -1;

        public bool MoveNext() => (++position < row.Frame.ColumnLabels.Count);

        public void Reset() => position = -1;

        dynamic IEnumerator.Current => row[position];
    }   
}
