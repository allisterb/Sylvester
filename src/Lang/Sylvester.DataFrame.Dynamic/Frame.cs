#region Attribution and License
// Contains code from https://github.com/microsoft/referencesource/blob/master/System.Core/Microsoft/Scripting/Actions/ExpandoObject.cs
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
using System.Linq;
using System.Linq.Expressions;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Reflection;
using System.Dynamic;
using System.Runtime.CompilerServices;

using Sylvester.DataFrame.Dynamic;

namespace Sylvester.Data
{
    public class Frame : IDynamicMetaObjectProvider, INotifyPropertyChanged, IEnumerable<FrameR>
    {
        #region Constructors
        public Frame()
        {
            _data = FrameData.Empty;
            _lockObject = new object();
            Backend = new DefaultBackend();
        }

        public Frame(params ISeries[] series) : this()
        {
            Add(series);
            BuildRows();
        }

        public Frame(IEnumerable<ISeries> series) : this(series.ToArray()) { }

        public Frame(Array[] data, object record) : this()
        {
            if (data == null || data.Length == 0)
            {
                throw new ArgumentException("At least one data array must be passed to this Frame constructor.", "data");
            }

            int l = data[0].Length;
            for (int i = 1; i < data.Length; i++)
            {
                if (data[i].Length != l)
                {
                    throw new ArgumentException($"The data arrays much each have the same size: {l}.", "data");
                }
            }

            Type r = record.GetType();
            if (!r.IsAnonymousType())
            {
                throw new TypeNotAnonymousException();
            }

            PropertyInfo[] props = r.GetProperties();
            if (props.Length != data.Length)
            {
                throw new ArgumentException("The number of data arrays must be equal to the number of object properties.", "data");
            }

            for (int i = 0; i < props.Length; i++)
            {
                PropertyInfo p = props[i];
                switch(p.PropertyType.Name)
                {
                    case "String":
                        Add(new Ss((string[])data[i], p.Name, (string) p.GetValue(record)));
                        break;
                    case "DateTime":
                        Add(new Sd((DateTime[])data[i], p.Name, (DateTime)p.GetValue(record)));
                        break;
                    case "Byte":
                        Add(new Sn<byte>((byte[])data[i], p.Name, (byte)p.GetValue(record)));
                        break;
                    case "SByte":
                        Add(new Sn<sbyte>((sbyte[])data[i], p.Name, (sbyte)p.GetValue(record)));
                        break;
                    case "Int16":
                        Add(new Sn<short>((short[])data[i], p.Name, (short)p.GetValue(record)));
                        break;
                    case "UInt16":
                        Add(new Sn<ushort>((ushort[])data[i], p.Name, (ushort)p.GetValue(record)));
                        break;
                    case "Int32":
                        Add(new Sn<int>((int[])data[i], p.Name, (int) p.GetValue(record)));
                        break;
                    case "UInt32":
                        Add(new Sn<uint>((uint[])data[i], p.Name, (uint)p.GetValue(record)));
                        break;
                    case "Int64":
                        Add(new Sn<long>((long[])data[i], p.Name, (long)p.GetValue(record)));
                        break;
                    case "UInt64":
                        Add(new Sn<ulong>((ulong[])data[i], p.Name, (ulong)p.GetValue(record)));
                        break;
                    case "Single":
                        Add(new Sn<float>((float[])data[i], p.Name, (float)p.GetValue(record)));
                        break;
                    case "Double":
                        Add(new Sn<double>((double[])data[i], p.Name, (double)p.GetValue(record)));
                        break;
                    case "Decimal":
                        Add(new Sn<decimal>((decimal[])data[i], p.Name, (decimal)p.GetValue(record)));
                        break;
                    case "Boolean":
                        Add(new Sn<bool>((bool[])data[i], p.Name, (bool)p.GetValue(record)));
                        break;

                    default: throw new NotImplementedException("Series of .NET reference objects can't be added to a Frame using anonymous types.");
                }
            }
            BuildRows();
        }

        public Frame(CsvFile file) : this()
        {
            file.Parse();
            for (int i = 0; i < file.Fields.Count; i++)
            {
                CsvField f = file.Fields[i];
                if (f.Data.Length == 0) continue;
                Add(f.Type, f.Data, f.Label);
            }
            BuildRows();
        }
        #endregion

        #region Properties
        public List<ISeries> Series { get; } = new List<ISeries>();

        public Dictionary<string, ISeries> Columns { get; } = new Dictionary<string, ISeries>();

        public FrameR this[int index] => rows[index];

        public ISeries this[string label] => Series.SingleOrDefault(s => s.Label == label);
        
        public int Length { get; protected set; } = -1;

        public Backend Backend { get; protected set; }

        public bool UnrestrictedMembers { get; set; } = false;

        #endregion

        #region Methods
        IEnumerator IEnumerable.GetEnumerator()
        {
            for (int i = 0; i < rows.Length; i++)
            {
                yield return rows[i];
            }
        }
        public IEnumerator<FrameR> GetEnumerator()
        {
            for (int i = 0; i < rows.Length; i++)
            {
                yield return rows[i];
            }
        }

        public FrameDR[] Ser(params ISeries[] series)
        {
            FrameDR[] rows = new FrameDR[this.Length];
            for (int i = 0; i < rows.Length; i++)
            {
                rows[i] = new FrameDR(this, i, series);
            }
            return rows;
        }

        public FrameDR[] Ser(params string[] series) => Ser(Series.Where(s => series.Contains(s.Label)).ToArray());

        public FrameDR[] Ser(params int[] series) => Ser(series.Select(i => Series[i]).ToArray());

        public FrameDR[] SerEx(params ISeries[] series) => Ser(Series.Where(s => !series.Contains(s)).ToArray());

        public FrameDR[] SerEx(params string[] series) => Ser(Series.Where(s => !series.Contains(s.Label)).ToArray());

        public FrameDR[] SerEx(params int[] series) => Ser(Series.Except(series.Select(i => Series[i])).ToArray());

        public Frame SerF(params ISeries[] series) => new Frame(series);

        public Frame SerF(params string[] labels) => new Frame(Series.Where(s => labels.Contains(s.Label)));

        public Frame SerF(params int[] series) => new Frame(series.Select(i => Series[i]));

        public Frame SerExF(params ISeries[] series) => new Frame(Series.Except(series));

        public Frame SerExF(params int[] series) => new Frame(Series.Except(series.Select(i => Series[i])));

        public Frame SerExF(params string[] labels) => new Frame(Series.Except(Series.Where(s => labels.Contains(s.Label))));

        public FrameW<T> Wnd<T>(Func<T, int> index) where T : struct, IEquatable<T> => new FrameW<T>(this, index);

        public FrameW<string> SWnd(ISeries s) => new FrameW<string>(this, (index) =>
            Array.IndexOf(((Ss)s).Data, index));

        public FrameW<DateTime> DWnd(ISeries s) => new FrameW<DateTime>(this, (index) =>
            Array.IndexOf(((Sd)s).Data, index));

        public FrameW<T> NWnd<T>(ISeries s) where T : struct, IEquatable<T>, IComparable<T>, IConvertible => 
            new FrameW<T>(this, (index) => Array.IndexOf(((Sn<T>)s).Data, index));

        public Frame Add(params ISeries[] series)
        {
            if (series.Length == 0) return this;
            if (Series.Count == 0)
            {
                Length = series[0].Length;
            }
            for (int i = 0; i < series.Length; i++)
            {
                TrySetValue(null, -1, series[i], series[i].Label, false, false);
                series[i].Backend = this.Backend;
            }
            return this;
        }

        public Frame Add(Type type, Array data, string label)
        {
            switch (type.Name)
            {
                case "String":
                    Add(new Ss((string[]) data, label));
                    break;
                case "DateTime":
                    Add(new Sd((DateTime[])data, label));
                    break;
                case "Byte":
                    Add(new Sn<byte>((byte[])data, label));
                    break;
                case "SByte":
                    Add(new Sn<sbyte>((sbyte[])data, label));
                    break;
                case "UInt16":
                    Add(new Sn<ushort>((ushort[])data, label));
                    break;
                case "Int16":
                    Add(new Sn<short>((short[])data, label));
                    break;
                case "UInt32":
                    Add(new Sn<uint>((uint[])data, label));
                    break;
                case "Int32":
                    Add(new Sn<int>((int[])data, label));
                    break;
                case "UInt64":
                    Add(new Sn<ulong>((ulong[])data, label));
                    break;
                case "Int64":
                    Add(new Sn<long>((long[])data, label));
                    break;
                case "Single":
                    Add(new Sn<float>((float[])data, label));
                    break;
                case "Double":
                    Add(new Sn<double>((double[])data, label));
                    break;
                case "Decimal":
                    Add(new Sn<decimal>((decimal[])data, label));
                    break;
                case "Boolean":
                    Add(new Sn<bool>((bool[]) data, label));
                    break;
            }
            return this;
        }

        protected void BuildRows()
        {
            rows = new FrameR[Length];
            for (int i = 0; i < Length; i++)
            {
                rows[i] = new FrameR(this, i);
            }
        }
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
                var @class = frameData.Class;
                for (int i = 0; i < @class.Keys.Length; i++)
                {
                    object val = frameData[i];
                    if (val != Frame._uninitialized)
                    {
                        yield return @class.Keys[i];
                    }
                }
            }

            /// <summary>
            /// Adds a dynamic test which checks if the version has changed.  The test is only necessary for
            /// performance as the methods will do the correct thing if called with an incorrect version.
            /// </summary>
            private DynamicMetaObject AddDynamicTestAndDefer(DynamicMetaObjectBinder binder, FrameClass @class, FrameClass originalClass, DynamicMetaObject succeeds)
            {

                Expression ifTestSucceeds = succeeds.Expression;
                if (originalClass != null)
                {
                    // we are accessing a member which has not yet been defined on this class.
                    // We force a class promotion after the type check.  If the class changes the 
                    // promotion will fail and the set/delete will do a full lookup using the new
                    // class to discover the name.
                    Debug.Assert(originalClass != @class);

                    ifTestSucceeds = Expression.Block(
                        Expression.Call(
                            null,
                            typeof(RuntimeHelpers).GetMethod("FramePromoteClass"),
                            GetLimitedSelf(),
                            Expression.Constant(originalClass, typeof(object)),
                            Expression.Constant(@class, typeof(object))
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
                            Expression.Constant(originalClass ?? @class, typeof(object))
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
            private FrameClass GetClassEnsureIndex(string name, bool caseInsensitive, Frame obj, out FrameClass @class, out int index)
            {
                FrameClass originalClass = Value.Class;

                index = originalClass.GetValueIndex(name, caseInsensitive, obj);
                if (index == Frame.ambiguousMatchFound)
                {
                    @class = originalClass;
                    return null;
                }
                if (index == Frame.noMatch)
                {
                    // go ahead and find a new class now...
                    FrameClass newClass = originalClass.FindNewClass(name);

                    @class = newClass;
                    index = newClass.GetValueIndexCaseSensitive(name);

                    Debug.Assert(index != Frame.noMatch);
                    return originalClass;
                }
                else
                {
                    @class = originalClass;
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
            internal FrameData(FrameClass @class, object[] data, int version)
            {
                Class = @class;
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
                if (!(value is ISeries) && !UnrestrictedMembers)
                {
                    throw new FrameUnrestrictedMembersNotEnabledException();
                }

                if (value is ISeries s && s.Label == "")
                {
                    value = s.Clone(name);
                }

                if (value is ISeries ss)
                {
                    Series.Add(ss);
                    Columns.Add(ss.Label, ss);
                    ss.Backend = this.Backend;
                }

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
        protected FrameR[] rows;
        private FrameData _data;                // the data currently being held by the Frame object
        private int _count;                     // the count of available members
        private PropertyChangedEventHandler _propertyChanged;
        #endregion
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




