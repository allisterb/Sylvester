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
    public class FrameR : DynamicObject, IEnumerable
    {
        public FrameR(Frame f, int index)
        {
            Frame = f;
            Index = index;
            foreach (var c in Frame.Columns)
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

        public dynamic this[string column] => Frame.Columns[column].GetVal(Index);

        public dynamic this[int i]
        {
            get => Frame.Series[i].GetVal(Index);
        }
        public IEnumerator GetEnumerator() => Enumerator;

        public override bool TryGetMember(GetMemberBinder binder, out object result)
        {
            result = null;
            if (Frame.Columns.ContainsKey(binder.Name))
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

    public class FrameREnumerator : IEnumerator
    {
        public FrameREnumerator(FrameR r)
        {
            row = r;
        }

        FrameR row;
        int position = -1;

        public bool MoveNext() => (++position < row.Frame.Columns.Count);

        public void Reset() => position = -1;

        dynamic IEnumerator.Current => row[position];
    }   
}
