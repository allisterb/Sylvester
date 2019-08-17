using System;
using System.Collections;
using System.Collections.Generic;
using System.Dynamic;

namespace Sylvester.Data
{
    public interface IRow : IEnumerable, IDynamicMetaObjectProvider
    {
        IEnumerable<string> ColumnLabels { get; }

        IRow Cols(params string[] columns);

        IRow ColsEx(params string[] columns);

        dynamic this[int i] { get; }

        dynamic this[string i] { get; }
    }
}
