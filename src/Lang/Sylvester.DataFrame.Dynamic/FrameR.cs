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
        public FrameR(Frame f, int index, List<ISeries> columns)
        {
            Frame = f;
            Index = index;
            _Columns = new Dictionary<string, ISeries>();
            {
                for (int i = 0; i < columns.Count; i++)
                {
                    _Columns.Add(columns[i].Label, columns[i]);
                }
            }
            foreach (var c in _Columns)
            {
                AddCallSite(c.Key);
            }
        }
        public Frame Frame { get; }

        public int Index { get; }

        internal Dictionary<string, ISeries> _Columns { get; }

        internal Dictionary<string, CallSite<Func<CallSite, object, object>>> CallSites =
            new Dictionary<string, CallSite<Func<CallSite, object, object>>>();
        public dynamic this[string column] => _Columns[column].GetVal(Index);

        public dynamic this[int i]
        {
            get => _Columns.Values.ElementAt(i).GetVal(Index);
        }
        public IEnumerator GetEnumerator() => _Columns.Values.GetEnumerator();

        public override bool TryGetMember(GetMemberBinder binder, out object result)
        {
            result = null;
            if (_Columns.ContainsKey(binder.Name))
            {
                result = GetMember(binder.Name);
                return true;
            }
            else
            {
                return false;
            }
        }
        public override bool TrySetMember(SetMemberBinder binder, object value) => false;

        internal object GetMember(string propName)
        {
            ISeries s = (ISeries) CallSites[propName].Target(CallSites[propName], this.Frame);
            return s.GetVal(Index);
        }

        internal void AddColumn(string colName, ISeries column)
        {
            lock (_lock)
            {
                _Columns.Add(colName, column);
                AddCallSite(colName);
            }
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
}
