using System;

using System.Collections.Generic;
using System.Dynamic;
using System.Runtime.CompilerServices;
using System.Linq;
using System.Text;

using Microsoft.CSharp.RuntimeBinder;

namespace Sylvester
{
    public class FrameW<T> : DynamicObject where T : IEquatable<T>
    {
        public FrameW(Frame f, Func<T, int> index, params ISeries[] columns)
        {
            Frame = f;
            Index = index;
            Columns = columns;
            for(int i = 0; i < Columns.Length; i++)
            {
                SetMember(Columns[i].Label, Columns[i]);
            }
        }

        public Frame Frame { get; }

        public Func<T, int> Index { get; }

        public ISeries[] Columns { get; }

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
            result = Columns.SingleOrDefault(c => c.Label == binder.Name);
            return result == null; 
        }

        public override bool TrySetMember(SetMemberBinder binder, object value)
        { 
            return false;
        }
    }
}
