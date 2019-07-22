#region Attribution and License
// Contains code from https://github.com/microsoft/referencesource/blob/master/System.Core/Microsoft/Scripting/Actions/FrameObject.cs
// ExpandoObject.cs is licensed under the following terms:

/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Apache License, Version 2.0, please send an email to 
 * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Apache License, Version 2.0.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/

#endregion

using System;
using System.Linq.Expressions;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Dynamic;
using System.Runtime.CompilerServices;

using Sylvester.DataFrame.Dynamic;

namespace Sylvester
{
    public class Frame : IDynamicMetaObjectProvider, IDictionary<string, object>, INotifyPropertyChanged
    {

        #region Constructors
        public Frame()
        {
            _data = FrameData.Empty;
            _lockObject = new object();
            _dynFrame = this;
        }

        public Frame(params ISeries[] series) : this()
        {
            for (int i = 0; i < series.Length; i++)
            {
               TrySetValue(null, -1, series[i], series[i].Label, false, false);
            }            
            Series.AddRange(series);
        }
        #endregion

        #region Properties
        public dynamic _dynFrame;
        public List<dynamic> Series { get; } = new List<dynamic>();

        public int Length { get; protected set; }
        public bool UnrestrictedMembers { get; set; } = false;
        #endregion

        #region IDynamicMetaObjectProvider Members

        DynamicMetaObject IDynamicMetaObjectProvider.GetMetaObject(Expression parameter)
        {
            return new MetaFrame(parameter, this);
        }
        #endregion

        #region MetaFrame
        private class MetaFrame : DynamicMetaObject
        {
            public MetaFrame(Expression expression, Frame value)
                : base(expression, BindingRestrictions.Empty, value)
            {
            }

            private DynamicMetaObject BindGetOrInvokeMember(DynamicMetaObjectBinder binder, string name, bool ignoreCase, DynamicMetaObject fallback, Func<DynamicMetaObject, DynamicMetaObject> fallbackInvoke)
            {
                FrameClass @class = Value.Class;

                //try to find the member, including the deleted members
                int index = @class.GetValueIndex(name, ignoreCase, Value);

                ParameterExpression value = Expression.Parameter(typeof(object), "value");

                Expression tryGetValue = Expression.Call(
                    typeof(RuntimeHelpers).GetMethod("FrameTryGetValue"),
                    GetLimitedSelf(),
                    Expression.Constant(@class, typeof(object)),
                    Expression.Constant(index),
                    Expression.Constant(name),
                    Expression.Constant(ignoreCase),
                    value
                );

                var result = new DynamicMetaObject(value, BindingRestrictions.Empty);
                if (fallbackInvoke != null)
                {
                    result = fallbackInvoke(result);
                }

                result = new DynamicMetaObject(
                    Expression.Block(
                        new[] { value },
                        Expression.Condition(
                            tryGetValue,
                            result.Expression,
                            fallback.Expression,
                            typeof(object)
                        )
                    ),
                    result.Restrictions.Merge(fallback.Restrictions)
                );

                return AddDynamicTestAndDefer(binder, Value.Class, null, result);
            }

            public override DynamicMetaObject BindGetMember(GetMemberBinder binder)
            {
                ContractUtils.RequiresNotNull(binder, "binder");
                return BindGetOrInvokeMember(
                    binder,
                    binder.Name,
                    binder.IgnoreCase,
                    binder.FallbackGetMember(this),
                    null
                );
            }

            public override DynamicMetaObject BindInvokeMember(InvokeMemberBinder binder, DynamicMetaObject[] args)
            {
                ContractUtils.RequiresNotNull(binder, "binder");
                return BindGetOrInvokeMember(
                    binder,
                    binder.Name,
                    binder.IgnoreCase,
                    binder.FallbackInvokeMember(this, args),
                    value => binder.FallbackInvoke(value, args, null)
                );
            }

            public override DynamicMetaObject BindSetMember(SetMemberBinder binder, DynamicMetaObject value)
            {
                ContractUtils.RequiresNotNull(binder, "binder");
                ContractUtils.RequiresNotNull(value, "value");
                if (value.RuntimeType.GetInterface("ISeries") == null)
                {
                    throw new FrameUnrestrictedMembersNotEnabledException();
                }

                FrameClass @class;
                int index;

                FrameClass originalClass = GetClassEnsureIndex(binder.Name, binder.IgnoreCase, Value, out @class, out index);

                return AddDynamicTestAndDefer(
                    binder,
                    @class,
                    originalClass,
                    new DynamicMetaObject(
                        Expression.Call(
                            typeof(RuntimeHelpers).GetMethod("FrameTrySetValue"),
                            GetLimitedSelf(),
                            Expression.Constant(@class, typeof(object)),
                            Expression.Constant(index),
                            Expression.Convert(value.Expression, typeof(object)),
                            Expression.Constant(binder.Name),
                            Expression.Constant(binder.IgnoreCase)
                        ),
                        BindingRestrictions.Empty
                    )
                );
            }

            public override DynamicMetaObject BindDeleteMember(DeleteMemberBinder binder)
            {
                ContractUtils.RequiresNotNull(binder, "binder");

                int index = Value.Class.GetValueIndex(binder.Name, binder.IgnoreCase, Value);

                Expression tryDelete = Expression.Call(
                    typeof(RuntimeHelpers).GetMethod("FrameTryDeleteValue"),
                    GetLimitedSelf(),
                    Expression.Constant(Value.Class, typeof(object)),
                    Expression.Constant(index),
                    Expression.Constant(binder.Name),
                    Expression.Constant(binder.IgnoreCase)
                );
                DynamicMetaObject fallback = binder.FallbackDeleteMember(this);

                DynamicMetaObject target = new DynamicMetaObject(
                    Expression.IfThen(Expression.Not(tryDelete), fallback.Expression),
                    fallback.Restrictions
                );

                return AddDynamicTestAndDefer(binder, Value.Class, null, target);
            }

            public override IEnumerable<string> GetDynamicMemberNames()
            {
                var frameData = Value._data;
                var klass = frameData.Class;
                for (int i = 0; i < klass.Keys.Length; i++)
                {
                    object val = frameData[i];
                    if (val != Frame._uninitialized)
                    {
                        yield return klass.Keys[i];
                    }
                }
            }

            /// <summary>
            /// Adds a dynamic test which checks if the version has changed.  The test is only necessary for
            /// performance as the methods will do the correct thing if called with an incorrect version.
            /// </summary>
            private DynamicMetaObject AddDynamicTestAndDefer(DynamicMetaObjectBinder binder, FrameClass klass, FrameClass originalClass, DynamicMetaObject succeeds)
            {

                Expression ifTestSucceeds = succeeds.Expression;
                if (originalClass != null)
                {
                    // we are accessing a member which has not yet been defined on this class.
                    // We force a class promotion after the type check.  If the class changes the 
                    // promotion will fail and the set/delete will do a full lookup using the new
                    // class to discover the name.
                    Debug.Assert(originalClass != klass);

                    ifTestSucceeds = Expression.Block(
                        Expression.Call(
                            null,
                            typeof(RuntimeHelpers).GetMethod("FramePromoteClass"),
                            GetLimitedSelf(),
                            Expression.Constant(originalClass, typeof(object)),
                            Expression.Constant(klass, typeof(object))
                        ),
                        succeeds.Expression
                    );
                }

                return new DynamicMetaObject(
                    Expression.Condition(
                        Expression.Call(
                            null,
                            typeof(RuntimeHelpers).GetMethod("FrameCheckVersion"),
                            GetLimitedSelf(),
                            Expression.Constant(originalClass ?? klass, typeof(object))
                        ),
                        ifTestSucceeds,
                        binder.GetUpdateExpression(ifTestSucceeds.Type)
                    ),
                    GetRestrictions().Merge(succeeds.Restrictions)
                );
            }

            /// <summary>
            /// Gets the class and the index associated with the given name.  Does not update the frame object.  Instead
            /// this returns both the original and desired new class.  A rule is created which includes the test for the
            /// original class, the promotion to the new class, and the set/delete based on the class post-promotion.
            /// </summary>
            private FrameClass GetClassEnsureIndex(string name, bool caseInsensitive, Frame obj, out FrameClass klass, out int index)
            {
                FrameClass originalClass = Value.Class;

                index = originalClass.GetValueIndex(name, caseInsensitive, obj);
                if (index == Frame.ambiguousMatchFound)
                {
                    klass = originalClass;
                    return null;
                }
                if (index == Frame.noMatch)
                {
                    // go ahead and find a new class now...
                    FrameClass newClass = originalClass.FindNewClass(name);

                    klass = newClass;
                    index = newClass.GetValueIndexCaseSensitive(name);

                    Debug.Assert(index != Frame.noMatch);
                    return originalClass;
                }
                else
                {
                    klass = originalClass;
                    return null;
                }
            }

            /// <summary>
            /// Returns our Expression converted to our known LimitType
            /// </summary>
            private Expression GetLimitedSelf()
            {
                if (TypeUtils.AreEquivalent(Expression.Type, LimitType))
                {
                    return Expression;
                }
                return Expression.Convert(Expression, LimitType);
            }

            /// <summary>
            /// Returns a Restrictions object which includes our current restrictions merged
            /// with a restriction limiting our type
            /// </summary>
            private BindingRestrictions GetRestrictions()
            {
                Debug.Assert(Restrictions == BindingRestrictions.Empty, "We don't merge, restrictions are always empty");

                return RuntimeHelpers.GetTypeRestriction(this);
            }

            public new Frame Value
            {
                get
                {
                    return (Frame)base.Value;
                }
            }
        }

        #endregion

        #region FrameData

        /// <summary>
        /// Stores the class and the data associated with the class as one atomic
        /// pair.  This enables us to do a class check in a thread safe manner w/o
        /// requiring locks.
        /// </summary>
        private class FrameData
        {
            internal static FrameData Empty = new FrameData();

            /// <summary>
            /// the dynamically assigned class associated with the Frame object
            /// </summary>
            internal readonly FrameClass Class;

            /// <summary>
            /// data stored in the frame object, key names are stored in the class.
            /// 
            /// Frame._data must be locked when mutating the value.  Otherwise a copy of it 
            /// could be made and lose values.
            /// </summary>
            private readonly object[] _dataArray;

            /// <summary>
            /// Indexer for getting/setting the data
            /// </summary>
            internal object this[int index]
            {
                get
                {
                    return _dataArray[index];
                }
                set
                {
                    //when the array is updated, version increases, even the new value is the same
                    //as previous. Dictionary type has the same behavior.
                    _version++;
                    _dataArray[index] = value;
                }
            }

            internal int Version
            {
                get { return _version; }
            }

            internal int Length
            {
                get { return _dataArray.Length; }
            }

            /// <summary>
            /// Constructs an empty FrameData object with the empty class and no data.
            /// </summary>
            private FrameData()
            {
                Class = FrameClass.Empty;
                _dataArray = new object[0];
            }

            /// <summary>
            /// the version of the Frame that tracks set and delete operations
            /// </summary>
            private int _version;

            /// <summary>
            /// Constructs a new FrameData object with the specified class and data.
            /// </summary>
            internal FrameData(FrameClass klass, object[] data, int version)
            {
                Class = klass;
                _dataArray = data;
                _version = version;
            }

            /// <summary>
            /// Update the associated class and increases the storage for the data array if needed.
            /// </summary>
            /// <returns></returns>
            internal FrameData UpdateClass(FrameClass newClass)
            {
                if (_dataArray.Length >= newClass.Keys.Length)
                {
                    // we have extra space in our buffer, just initialize it to Uninitialized.
                    this[newClass.Keys.Length - 1] = Frame._uninitialized;
                    return new FrameData(newClass, this._dataArray, this._version);
                }
                else
                {
                    // we've grown too much - we need a new object array
                    int oldLength = _dataArray.Length;
                    object[] arr = new object[GetAlignedSize(newClass.Keys.Length)];
                    Array.Copy(_dataArray, arr, _dataArray.Length);
                    FrameData newData = new FrameData(newClass, arr, this._version);
                    newData[oldLength] = Frame._uninitialized;
                    return newData;
                }
            }

            private static int GetAlignedSize(int len)
            {
                // the alignment of the array for storage of values (must be a power of two)
                const int DataArrayAlignment = 8;

                // round up and then mask off lower bits
                return (len + (DataArrayAlignment - 1)) & (~(DataArrayAlignment - 1));
            }
        }

        #endregion

        #region Get/Set/Delete Members

        /// <summary>
        /// Try to get the data stored for the specified class at the specified index.  If the
        /// class has changed a full lookup for the slot will be performed and the correct
        /// value will be retrieved.
        /// </summary>
        internal bool TryGetValue(object indexClass, int index, string name, bool ignoreCase, out object value)
        {
            // read the data now.  The data is immutable so we get a consistent view.
            // If there's a concurrent writer they will replace data and it just appears
            // that we won the ----
            FrameData data = _data;
            if (data.Class != indexClass || ignoreCase)
            {
                /* Re-search for the index matching the name here if
                 *  1) the class has changed, we need to get the correct index and return
                 *  the value there.
                 *  2) the search is case insensitive:
                 *      a. the member specified by index may be deleted, but there might be other
                 *      members matching the name if the binder is case insensitive.
                 *      b. the member that exactly matches the name didn't exist before and exists now,
                 *      need to find the exact match.
                 */
                index = data.Class.GetValueIndex(name, ignoreCase, this);
                if (index == ambiguousMatchFound)
                {
                    throw new AmbiguousMatchInFrameException(name);
                }
            }

            if (index == noMatch)
            {
                value = null;
                return false;
            }

            // Capture the value into a temp, so it doesn't get mutated after we check
            // for Uninitialized.
            object temp = data[index];
            if (temp == _uninitialized)
            {
                value = null;
                return false;
            }

            // index is now known to be correct
            value = temp;
            return true;
        }

        /// <summary>
        /// Sets the data for the specified class at the specified index.  If the class has
        /// changed then a full look for the slot will be performed.  If the new class does
        /// not have the provided slot then the Frame's class will change. Only case sensitive
        /// setter is supported in Frame.
        /// </summary>
        internal void TrySetValue(object indexClass, int index, object value, string name, bool ignoreCase, bool add)
        {
            FrameData data;
            object oldValue;

            lock (_lockObject)
            {
                data = _data;

                if (data.Class != indexClass || ignoreCase)
                {
                    // The class has changed or we are doing a case-insensitive search, 
                    // we need to get the correct index and set the value there.  If we 
                    // don't have the value then we need to promote the class - that 
                    // should only happen when we have multiple concurrent writers.
                    index = data.Class.GetValueIndex(name, ignoreCase, this);
                    if (index == Frame.ambiguousMatchFound)
                    {
                        throw new AmbiguousMatchInFrameException(name);
                    }
                    if (index == Frame.noMatch)
                    {
                        // Before creating a new class with the new member, need to check 
                        // if there is the exact same member but is deleted. We should reuse
                        // the class if there is such a member.
                        int exactMatch = ignoreCase ?
                            data.Class.GetValueIndexCaseSensitive(name) :
                            index;
                        if (exactMatch != Frame.noMatch)
                        {
                            Debug.Assert(data[exactMatch] == _uninitialized);
                            index = exactMatch;
                        }
                        else
                        {
                            FrameClass newClass = data.Class.FindNewClass(name);
                            data = PromoteClassCore(data.Class, newClass);
                            // After the class promotion, there must be an exact match,
                            // so we can do case-sensitive search here.
                            index = data.Class.GetValueIndexCaseSensitive(name);
                            Debug.Assert(index != Frame.noMatch);
                        }
                    }
                }

                // Setting an uninitialized member increases the count of available members
                oldValue = data[index];
                if (oldValue == _uninitialized)
                {
                    _count++;
                }
                else if (add)
                {
                    throw new SameKeyExistsInFrameException(name);
                }

                data[index] = value;
            }

            // Notify property changed, outside of the lock.
            var propertyChanged = _propertyChanged;
            if (propertyChanged != null && value != oldValue)
            {
                // Use the canonical case for the key.
                propertyChanged(this, new PropertyChangedEventArgs(data.Class.Keys[index]));
            }
        }

        /// <summary>
        /// Deletes the data stored for the specified class at the specified index.
        /// </summary>
        internal bool TryDeleteValue(object indexClass, int index, string name, bool ignoreCase, object deleteValue)
        {
            FrameData data;
            lock (_lockObject)
            {
                data = _data;

                if (data.Class != indexClass || ignoreCase)
                {
                    // the class has changed or we are doing a case-insensitive search,
                    // we need to get the correct index.  If there is no associated index
                    // we simply can't have the value and we return false.
                    index = data.Class.GetValueIndex(name, ignoreCase, this);
                    if (index == Frame.ambiguousMatchFound)
                    {
                        throw new AmbiguousMatchInFrameException(name);
                    }
                }
                if (index == Frame.noMatch)
                {
                    return false;
                }

                object oldValue = data[index];
                if (oldValue == _uninitialized)
                {
                    return false;
                }

                // Make sure the value matches, if requested.
                //
                // It's a shame we have to call Equals with the lock held but
                // there doesn't seem to be a good way around that, and
                // ConcurrentDictionary in mscorlib does the same thing.
                if (deleteValue != _uninitialized && !object.Equals(oldValue, deleteValue))
                {
                    return false;
                }

                data[index] = _uninitialized;

                // Deleting an available member decreases the count of available members
                _count--;
            }

            // Notify property changed, outside of the lock.
            var propertyChanged = _propertyChanged;
            if (propertyChanged != null)
            {
                // Use the canonical case for the key.
                propertyChanged(this, new PropertyChangedEventArgs(data.Class.Keys[index]));
            }

            return true;
        }

        /// <summary>
        /// Returns true if the member at the specified index has been deleted,
        /// otherwise false. Call this function holding the lock.
        /// </summary>
        internal bool IsDeletedMember(int index)
        {
            Debug.Assert(index >= 0 && index <= _data.Length);

            if (index == _data.Length)
            {
                // The member is a newly added by SetMemberBinder and not in data yet
                return false;
            }

            return _data[index] == _uninitialized;
        }

        /// <summary>
        /// Exposes the FrameClass which we've associated with this 
        /// Frame object.  Used for type checks in rules.
        /// </summary>
        internal FrameClass Class
        {
            get
            {
                return _data.Class;
            }
        }

        /// <summary>
        /// Promotes the class from the old type to the new type and returns the new
        /// FrameData object.
        /// </summary>
        private FrameData PromoteClassCore(FrameClass oldClass, FrameClass newClass)
        {
            Debug.Assert(oldClass != newClass);

            lock (_lockObject)
            {
                if (_data.Class == oldClass)
                {
                    _data = _data.UpdateClass(newClass);
                }
                return _data;
            }
        }

        /// <summary>
        /// Internal helper to promote a class.  Called from our RuntimeHelper members.  This
        /// version simply doesn't expose the FrameData object which is a private
        /// data structure.
        /// </summary>
        internal void PromoteClass(object oldClass, object newClass)
        {
            PromoteClassCore((FrameClass)oldClass, (FrameClass)newClass);
        }

        #endregion

        #region IDictionary<string, object> Members
        ICollection<string> IDictionary<string, object>.Keys
        {
            get
            {
                return new KeyCollection(this);
            }
        }

        ICollection<object> IDictionary<string, object>.Values
        {
            get
            {
                return new ValueCollection(this);
            }
        }

        object IDictionary<string, object>.this[string key]
        {
            get
            {
                object value;
                if (!TryGetValueForKey(key, out value))
                {
                    throw new KeyDoesNotExistInFrameException(key);
                }
                return value;
            }
            set
            {
                ContractUtils.RequiresNotNull(key, "key");
                // Pass null to the class, which forces lookup.
                TrySetValue(null, -1, value, key, false, false);
            }
        }

        void IDictionary<string, object>.Add(string key, object value)
        {
            this.TryAddMember(key, value);
        }

        bool IDictionary<string, object>.ContainsKey(string key)
        {
            ContractUtils.RequiresNotNull(key, "key");

            FrameData data = _data;
            int index = data.Class.GetValueIndexCaseSensitive(key);
            return index >= 0 && data[index] != _uninitialized;
        }

        bool IDictionary<string, object>.Remove(string key)
        {
            ContractUtils.RequiresNotNull(key, "key");
            // Pass null to the class, which forces lookup.
            return TryDeleteValue(null, -1, key, false, _uninitialized);
        }

        bool IDictionary<string, object>.TryGetValue(string key, out object value)
        {
            return TryGetValueForKey(key, out value);
        }

        #endregion

        #region ICollection<KeyValuePair<string, object>> Members
        int ICollection<KeyValuePair<string, object>>.Count
        {
            get
            {
                return _count;
            }
        }

        bool ICollection<KeyValuePair<string, object>>.IsReadOnly
        {
            get { return false; }
        }

        void ICollection<KeyValuePair<string, object>>.Add(KeyValuePair<string, object> item)
        {
            TryAddMember(item.Key, item.Value);
        }

        void ICollection<KeyValuePair<string, object>>.Clear()
        {
            // We remove both class and data!
            FrameData data;
            lock (_lockObject)
            {
                data = _data;
                _data = FrameData.Empty;
                _count = 0;
            }

            // Notify property changed for all properties.
            var propertyChanged = _propertyChanged;
            if (propertyChanged != null)
            {
                for (int i = 0, n = data.Class.Keys.Length; i < n; i++)
                {
                    if (data[i] != _uninitialized)
                    {
                        propertyChanged(this, new PropertyChangedEventArgs(data.Class.Keys[i]));
                    }
                }
            }
        }

        bool ICollection<KeyValuePair<string, object>>.Contains(KeyValuePair<string, object> item)
        {
            object value;
            if (!TryGetValueForKey(item.Key, out value))
            {
                return false;
            }

            return object.Equals(value, item.Value);
        }

        void ICollection<KeyValuePair<string, object>>.CopyTo(KeyValuePair<string, object>[] array, int arrayIndex)
        {
            ContractUtils.RequiresNotNull(array, "array");
            ContractUtils.RequiresArrayRange(array, arrayIndex, _count, "arrayIndex", "Count");

            // We want this to be atomic and not throw
            lock (_lockObject)
            {
                foreach (KeyValuePair<string, object> item in this)
                {
                    array[arrayIndex++] = item;
                }
            }
        }

        bool ICollection<KeyValuePair<string, object>>.Remove(KeyValuePair<string, object> item)
        {
            return TryDeleteValue(null, -1, item.Key, false, item.Value);
        }
        #endregion

        #region IEnumerable<KeyValuePair<string, object>> Member

        IEnumerator<KeyValuePair<string, object>> IEnumerable<KeyValuePair<string, object>>.GetEnumerator()
        {
            FrameData data = _data;
            return GetFrameEnumerator(data, data.Version);
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            FrameData data = _data;
            return GetFrameEnumerator(data, data.Version);
        }

        // Note: takes the data and version as parameters so they will be
        // captured before the first call to MoveNext().
        private IEnumerator<KeyValuePair<string, object>> GetFrameEnumerator(FrameData data, int version)
        {
            for (int i = 0; i < data.Class.Keys.Length; i++)
            {
                if (_data.Version != version || data != _data)
                {
                    // The underlying frame object has changed:
                    // 1) the version of the frame data changed
                    // 2) the data object is changed 
                    throw new CollectionModifiedWhileEnumeratingException();
                }
                // Capture the value into a temp so we don't inadvertently
                // return Uninitialized.
                object temp = data[i];
                if (temp != _uninitialized)
                {
                    yield return new KeyValuePair<string, object>(data.Class.Keys[i], temp);
                }
            }
        }
        #endregion

        #region INotifyPropertyChanged Members

        event PropertyChangedEventHandler INotifyPropertyChanged.PropertyChanged
        {
            add { _propertyChanged += value; }
            remove { _propertyChanged -= value; }
        }

        #endregion

        #region Helper methods
        private void TryAddMember(string key, object value)
        {
            ContractUtils.RequiresNotNull(key, "key");
            // Pass null to the class, which forces lookup.
            TrySetValue(null, -1, value, key, false, true);
        }

        private bool TryGetValueForKey(string key, out object value)
        {
            // Pass null to the class, which forces lookup.
            return TryGetValue(null, -1, key, false, out value);
        }

        private bool FrameContainsKey(string key)
        {
            return _data.Class.GetValueIndexCaseSensitive(key) >= 0;
        }

        // We create a non-generic type for the debug view for each different collection type
        // that uses DebuggerTypeProxy, instead of defining a generic debug view type and
        // using different instantiations. The reason for this is that support for generics
        // with using DebuggerTypeProxy is limited. For C#, DebuggerTypeProxy supports only
        // open types (from MSDN http://msdn.microsoft.com/en-us/library/d8eyd8zc.aspx).
        private sealed class KeyCollectionDebugView
        {
            private ICollection<string> collection;
            public KeyCollectionDebugView(ICollection<string> collection)
            {
                Debug.Assert(collection != null);
                this.collection = collection;
            }

            [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
            public string[] Items
            {
                get
                {
                    string[] items = new string[collection.Count];
                    collection.CopyTo(items, 0);
                    return items;
                }
            }
        }

        [DebuggerTypeProxy(typeof(KeyCollectionDebugView))]
        [DebuggerDisplay("Count = {Count}")]
        private class KeyCollection : ICollection<string>
        {
            private readonly Frame _frame;
            private readonly int _frameVersion;
            private readonly int _frameCount;
            private readonly FrameData _frameData;

            internal KeyCollection(Frame frame)
            {
                lock (frame._lockObject)
                {
                    _frame = frame;
                    _frameVersion = frame._data.Version;
                    _frameCount = frame._count;
                    _frameData = frame._data;
                }
            }

            private void CheckVersion()
            {
                if (_frame._data.Version != _frameVersion || _frameData != _frame._data)
                {
                    //the underlying frame object has changed
                    throw new CollectionModifiedWhileEnumeratingException();
                }
            }

            #region ICollection<string> Members

            public void Add(string item)
            {
                throw new CollectionReadOnlyException();
            }

            public void Clear()
            {
                throw new CollectionReadOnlyException();
            }

            public bool Contains(string item)
            {
                lock (_frame._lockObject)
                {
                    CheckVersion();
                    return _frame.FrameContainsKey(item);
                }
            }

            public void CopyTo(string[] array, int arrayIndex)
            {
                ContractUtils.RequiresNotNull(array, "array");
                ContractUtils.RequiresArrayRange(array, arrayIndex, _frameCount, "arrayIndex", "Count");
                lock (_frame._lockObject)
                {
                    CheckVersion();
                    FrameData data = _frame._data;
                    for (int i = 0; i < data.Class.Keys.Length; i++)
                    {
                        if (data[i] != _uninitialized)
                        {
                            array[arrayIndex++] = data.Class.Keys[i];
                        }
                    }
                }
            }

            public int Count
            {
                get
                {
                    CheckVersion();
                    return _frameCount;
                }
            }

            public bool IsReadOnly
            {
                get { return true; }
            }

            public bool Remove(string item)
            {
                throw new CollectionReadOnlyException();
            }

            #endregion

            #region IEnumerable<string> Members

            public IEnumerator<string> GetEnumerator()
            {
                for (int i = 0, n = _frameData.Class.Keys.Length; i < n; i++)
                {
                    CheckVersion();
                    if (_frameData[i] != _uninitialized)
                    {
                        yield return _frameData.Class.Keys[i];
                    }
                }
            }

            #endregion

            #region IEnumerable Members

            System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }

            #endregion
        }

        // We create a non-generic type for the debug view for each different collection type
        // that uses DebuggerTypeProxy, instead of defining a generic debug view type and
        // using different instantiations. The reason for this is that support for generics
        // with using DebuggerTypeProxy is limited. For C#, DebuggerTypeProxy supports only
        // open types (from MSDN http://msdn.microsoft.com/en-us/library/d8eyd8zc.aspx).
        private sealed class ValueCollectionDebugView
        {
            private ICollection<object> collection;
            public ValueCollectionDebugView(ICollection<object> collection)
            {
                Debug.Assert(collection != null);
                this.collection = collection;
            }

            [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
            public object[] Items
            {
                get
                {
                    object[] items = new object[collection.Count];
                    collection.CopyTo(items, 0);
                    return items;
                }
            }
        }

        [DebuggerTypeProxy(typeof(ValueCollectionDebugView))]
        [DebuggerDisplay("Count = {Count}")]
        private class ValueCollection : ICollection<object>
        {
            private readonly Frame _frame;
            private readonly int _frameVersion;
            private readonly int _frameCount;
            private readonly FrameData _frameData;

            internal ValueCollection(Frame frame)
            {
                lock (frame._lockObject)
                {
                    _frame = frame;
                    _frameVersion = frame._data.Version;
                    _frameCount = frame._count;
                    _frameData = frame._data;
                }
            }

            private void CheckVersion()
            {
                if (_frame._data.Version != _frameVersion || _frameData != _frame._data)
                {
                    //the underlying frame object has changed
                    throw new CollectionModifiedWhileEnumeratingException();
                }
            }

            #region ICollection<string> Members

            public void Add(object item)
            {
                throw new CollectionReadOnlyException();
            }

            public void Clear()
            {
                throw new CollectionReadOnlyException();
            }

            public bool Contains(object item)
            {
                lock (_frame._lockObject)
                {
                    CheckVersion();

                    FrameData data = _frame._data;
                    for (int i = 0; i < data.Class.Keys.Length; i++)
                    {

                        // See comment in TryDeleteValue; it's okay to call
                        // object.Equals with the lock held.
                        if (object.Equals(data[i], item))
                        {
                            return true;
                        }
                    }
                    return false;
                }
            }

            public void CopyTo(object[] array, int arrayIndex)
            {
                ContractUtils.RequiresNotNull(array, "array");
                ContractUtils.RequiresArrayRange(array, arrayIndex, _frameCount, "arrayIndex", "Count");
                lock (_frame._lockObject)
                {
                    CheckVersion();
                    FrameData data = _frame._data;
                    for (int i = 0; i < data.Class.Keys.Length; i++)
                    {
                        if (data[i] != _uninitialized)
                        {
                            array[arrayIndex++] = data[i];
                        }
                    }
                }
            }

            public int Count
            {
                get
                {
                    CheckVersion();
                    return _frameCount;
                }
            }

            public bool IsReadOnly
            {
                get { return true; }
            }

            public bool Remove(object item)
            {
                throw new CollectionReadOnlyException();
            }

            #endregion

            #region IEnumerable<string> Members

            public IEnumerator<object> GetEnumerator()
            {
                FrameData data = _frame._data;
                for (int i = 0; i < data.Class.Keys.Length; i++)
                {
                    CheckVersion();
                    // Capture the value into a temp so we don't inadvertently
                    // return Uninitialized.
                    object temp = data[i];
                    if (temp != _uninitialized)
                    {
                        yield return temp;
                    }
                }
            }

            #endregion

            #region IEnumerable Members

            System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }

            #endregion
        }

        #endregion

        #region Fields
        internal readonly object _lockObject;   // the readonly field is used for locking the Frame object
        internal readonly static object _uninitialized = new object(); // A marker object used to identify that a value is uninitialized.
        internal const int ambiguousMatchFound = -2;        // The value is used to indicate there exists ambiguous match in the Frame object
        internal const int noMatch = -1;                    // The value is used to indicate there is no matching member
        private FrameData _data;                // the data currently being held by the Frame object
        private int _count;                     // the count of available members
        private PropertyChangedEventHandler _propertyChanged;
        #endregion

        public int this[int index]  => 0;

    }

    #region RuntimeHelpers
    public static partial class RuntimeHelpers
    {
        public static bool FrameTryGetValue(Frame frame, object indexClass, int index, string name, bool ignoreCase, out object value)
        {
            return frame.TryGetValue(indexClass, index, name, ignoreCase, out value);
        }

        public static object FrameTrySetValue(Frame frame, object indexClass, int index, object value, string name, bool ignoreCase)
        {
            frame.TrySetValue(indexClass, index, value, name, ignoreCase, false);
            return value;
        }

        public static bool FrameTryDeleteValue(Frame frame, object indexClass, int index, string name, bool ignoreCase)
        {
            return frame.TryDeleteValue(indexClass, index, name, ignoreCase, Frame._uninitialized);
        }

        
        public static bool FrameCheckVersion(Frame frame, object version)
        {
            return frame.Class == version;
        }

        
        public static void FramePromoteClass(Frame frame, object oldClass, object newClass)
        {
            frame.PromoteClass(oldClass, newClass);
        }

        public static BindingRestrictions GetTypeRestriction(DynamicMetaObject obj)
        {
            if (obj.Value == null && obj.HasValue)
            {
                return BindingRestrictions.GetInstanceRestriction(obj.Expression, null);
            }
            else
            {
                return BindingRestrictions.GetTypeRestriction(obj.Expression, obj.LimitType);
            }
        }
    }
    #endregion
}




