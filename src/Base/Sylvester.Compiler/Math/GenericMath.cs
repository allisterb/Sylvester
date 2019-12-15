using System;
using System.Runtime.CompilerServices;

namespace Sylvester.Math
{
    /// <summary>
    /// Methods to working with numeric values in generic classes like <code>Var{T}</code> 
    /// where the concrete type isn't known up-front e.g. INDArray defines a <code>Ones()</code>
    /// method that <code>Var{T}</code> must implement that fills the array buffer with ones.
    /// </summary>
    /// <typeparam name="TData"></typeparam>
    public static class GenericMath<TData> where TData : unmanaged, IEquatable<TData>, IComparable<TData>, IConvertible
    {        
        static GenericMath()
        {
        }

        public static TData Const<TValue>(TValue v) where TValue : unmanaged, IEquatable<TValue>, IComparable<TValue>,
            IConvertible
        {
            return (TData) Convert.ChangeType(v, typeof(TData));
        }

        
        public static double F(Func<double, double> f, TData n)
        {
            switch (n)
            {
                case SByte v:
                    return checked(f(v));
                case Byte v:
                    return checked(f(v));
                case Int32 v:
                    return checked(f(v));
                case UInt32 v:
                    return checked(f(v));
                case Int16 v:
                    return checked(f(v));
                case UInt16 v:
                    return checked(f(v));
                case Int64 v:
                    return checked(f(v));
                case UInt64 v:
                    return checked(f(v));
                case Single v:
                    return checked(f(v));
                case Double v:
                    return checked(f(v));
                case Boolean v:
                    throw new ArgumentException($"Cannot apply math functions to a bool.");
                default:
                    throw new ArithmeticException();
            }
        }

        public static TData Random(IRandomNumberGenerator rng)
        {
            TData e = default;
            switch (e)
            {
                case SByte v:
                    return Const(checked((sbyte) rng.Next(0, SByte.MaxValue)));
                case Byte v:
                    return Const(checked((byte) rng.Next(0, Byte.MaxValue)));
                case Int32 v:
                    return Const(checked((int) rng.Next(0, Int32.MaxValue)));
                case UInt32 v:
                    return Const(checked((uint) rng.Next(0, Int32.MaxValue)));
                case Int16 v:
                    return Const(checked((short) rng.Next(0, Int16.MaxValue)));
                case UInt16 v:
                    return Const(checked((ushort) rng.Next(0, UInt16.MaxValue)));
                case Int64 v: // See https://social.msdn.microsoft.com/Forums/vstudio/en-US/cb9c7f4d-5f1e-4900-87d8-013205f27587/64-bit-strong-random-function?forum=csharpgeneral
                    byte[] buffer = new byte[8];
                    rng.NextBytes(buffer);
                    short hi = (short)rng.Next(4, 0x10000);
                    buffer[7] = (byte)(hi >> 8);
                    buffer[6] = (byte)hi;
                    return Const(BitConverter.ToInt64(buffer, 0));
                case UInt64 v: // See https://social.msdn.microsoft.com/Forums/vstudio/en-US/cb9c7f4d-5f1e-4900-87d8-013205f27587/64-bit-strong-random-function?forum=csharpgeneral
                    buffer = new byte[8];
                    rng.NextBytes(buffer);
                    hi = (short)rng.Next(4, 0x10000);
                    buffer[7] = (byte)(hi >> 8);
                    buffer[6] = (byte)hi;
                    return Const(BitConverter.ToUInt64(buffer, 0));
                case Single v: // TODO: [allisterb] Check for correctness..
                    return Const(checked(((Single) (rng.NextDouble() * Int64.MaxValue))));
                case Double v: // TODO: [allisterb] Check for correctness.
                    return Const(checked((((double) rng.NextDouble() * Int64.MaxValue))));
                case Boolean v:
                    return Const(Convert.ToBoolean(rng.Next(0, 1)));

                default:
                    throw new ArgumentException($"Cannot generate random value for type {typeof(TData).Name}.");
            }
        }

        public static int[] StridesInElements(int[] dim)
        {
            var strides = new int[dim.Length];
            float s = 1;
            for (int i = 0; i < dim.Length; i++)
            {
                if (dim[i] > 0)
                {
                    s *= Convert.ToSingle(dim[i]);
                }
            }

            for (int i = 0; i < dim.Length; i++)
            {
                if (dim[i] > 0)
                {
                    s /= Convert.ToSingle(dim[i]);
                    strides[i] = Convert.ToInt32(s);
                }
            }

            return strides;
        }

        /*
        public static int[] StridesInBytes<T>(int[] dim)
        {
            var strides = StridesInElements(dim);
            for (int i = 0; i < strides.Length; i++)
            {
                strides[i] *= Unsafe.SizeOf<T>();
            }

            return strides;
        }
        */
    }
}