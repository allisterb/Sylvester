using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Math
{
    public interface IRandomNumberGenerator
    {
        int Next();
        int Next(int maxValue);
        int Next(int minValue, int maxValue);
        void NextBytes(byte[] buffer);
        double NextDouble();
    }
}
