using System;
using System.Collections.Generic;
using System.Dynamic;
using System.Text;

namespace Sylvester
{
    public class FrameWnd<T> : DynamicObject where T : IEquatable<T>
    {
        public FrameWnd(Frame f, IDictionary<T, int> index, params ISeries[] series)
        {
            _wnd = this;
        }

        dynamic _wnd; 
    }
}
