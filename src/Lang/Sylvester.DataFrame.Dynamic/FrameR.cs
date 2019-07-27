using System;
using System.Collections;
using System.Collections.Generic;
using System.Dynamic;
using System.Runtime.CompilerServices;
using System.Linq;
using System.Text;

using Microsoft.CSharp.RuntimeBinder;

namespace Sylvester
{
    public class FrameR : DynamicObject, IEnumerable
    {
        public FrameR(Frame f, int index, Dictionary<string, ISeries> columns)
        {
            Frame = f;
            Index = index;
            _Columns = columns;
        }

        public Frame Frame { get; }

        public int Index { get; }

        internal Dictionary<string, ISeries> _Columns { get; }

        public dynamic this[string column] => _Columns[column].GetVal(Index);

        public dynamic this[int i]
        {
            get => _Columns.Values.ElementAt(i).GetVal(Index);
        }
        public IEnumerator GetEnumerator() => _Columns.Values.GetEnumerator();

        internal object GetMember(string propName)
        {
            var binder = Binder.GetMember(CSharpBinderFlags.None,
                  propName, this.GetType(),
                  new List<CSharpArgumentInfo>{
                       CSharpArgumentInfo.Create(CSharpArgumentInfoFlags.None, null)});
            var callsite = CallSite<Func<CallSite, object, object>>.Create(binder);
            ISeries s = (ISeries)callsite.Target(callsite, this.Frame);
            return s.GetVal(Index);
        }

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
    }
}
