using System;

namespace Sylvester.Compiler
{
	public partial class Kernel<T> : IKernel<T> where T : unmanaged, IEquatable<T>, IComparable<T>, IConvertible
	{
		  
		public Func<Var<T>, Var<T>> Func1 => 
            new Func<Var<T>, Var<T>>((i1) 
			=> Func(new Var<T>[] { i1 }));
		  
		public Func<Var<T>,Var<T>, Var<T>> Func2 => 
            new Func<Var<T>,Var<T>, Var<T>>((i1,i2) 
			=> Func(new Var<T>[] { i1,i2 }));
		  
		public Func<Var<T>,Var<T>,Var<T>, Var<T>> Func3 => 
            new Func<Var<T>,Var<T>,Var<T>, Var<T>>((i1,i2,i3) 
			=> Func(new Var<T>[] { i1,i2,i3 }));
		  
		public Func<Var<T>,Var<T>,Var<T>,Var<T>, Var<T>> Func4 => 
            new Func<Var<T>,Var<T>,Var<T>,Var<T>, Var<T>>((i1,i2,i3,i4) 
			=> Func(new Var<T>[] { i1,i2,i3,i4 }));
		  
		public Func<Var<T>,Var<T>,Var<T>,Var<T>,Var<T>, Var<T>> Func5 => 
            new Func<Var<T>,Var<T>,Var<T>,Var<T>,Var<T>, Var<T>>((i1,i2,i3,i4,i5) 
			=> Func(new Var<T>[] { i1,i2,i3,i4,i5 }));
		  
		public Func<Var<T>,Var<T>,Var<T>,Var<T>,Var<T>,Var<T>, Var<T>> Func6 => 
            new Func<Var<T>,Var<T>,Var<T>,Var<T>,Var<T>,Var<T>, Var<T>>((i1,i2,i3,i4,i5,i6) 
			=> Func(new Var<T>[] { i1,i2,i3,i4,i5,i6 }));
		  
		public Func<Var<T>,Var<T>,Var<T>,Var<T>,Var<T>,Var<T>,Var<T>, Var<T>> Func7 => 
            new Func<Var<T>,Var<T>,Var<T>,Var<T>,Var<T>,Var<T>,Var<T>, Var<T>>((i1,i2,i3,i4,i5,i6,i7) 
			=> Func(new Var<T>[] { i1,i2,i3,i4,i5,i6,i7 }));
		  
		public Func<Var<T>,Var<T>,Var<T>,Var<T>,Var<T>,Var<T>,Var<T>,Var<T>, Var<T>> Func8 => 
            new Func<Var<T>,Var<T>,Var<T>,Var<T>,Var<T>,Var<T>,Var<T>,Var<T>, Var<T>>((i1,i2,i3,i4,i5,i6,i7,i8) 
			=> Func(new Var<T>[] { i1,i2,i3,i4,i5,i6,i7,i8 }));
		}
}