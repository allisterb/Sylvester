using System;

using System.Collections.Generic;
using System.Dynamic;
using System.Runtime.CompilerServices;
using System.Linq;
using System.Text;

using Microsoft.CSharp.RuntimeBinder;

namespace Sylvester
{
    public class FrameR : DynamicObject
    {
        public FrameR(Frame f, int index, IDictionary<string, dynamic> columns)
        {
            Frame = f;
            Index = index;
            Columns = columns;
        
            foreach (var kv in Columns)
            {
                SetMember(kv.Key, kv.Value);
            }
        }

        public Frame Frame { get; }

        public int Index { get; }

        public IDictionary<string, dynamic> Columns { get; }

        public dynamic this[string column] => Columns[column];

        public dynamic this[int index] => Columns.Values.ElementAt(index);

        protected object GetMember(string propName)
        {
            var binder = Binder.GetMember(CSharpBinderFlags.None,
                  propName, this.GetType(),
                  new List<CSharpArgumentInfo>{
                       CSharpArgumentInfo.Create(CSharpArgumentInfoFlags.None, null)});
            var callsite = CallSite<Func<CallSite, object, object>>.Create(binder);

            return callsite.Target(callsite, this);
        }

        protected void SetMember(string propName, object val)
        {
            var binder = Binder.SetMember(CSharpBinderFlags.None,
                   propName, this.GetType(),
                   new List<CSharpArgumentInfo>{
                       CSharpArgumentInfo.Create(CSharpArgumentInfoFlags.None, null),
                       CSharpArgumentInfo.Create(CSharpArgumentInfoFlags.None, null)});
            var callsite = CallSite<Func<CallSite, object, object, object>>.Create(binder);

            callsite.Target(callsite, this, val);
        }

        public override bool TryGetMember(GetMemberBinder binder, out object result)
        {
            result = null;
            return Columns.TryGetValue(binder.Name, out result);
        }
        public override bool TrySetMember(SetMemberBinder binder, object value)
        {
            return false;
        }
    }
}
