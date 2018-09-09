using System.Linq.Expressions;

namespace Sylvester.Notation
{
	public partial class Tensor
	{
		  
		public TensorIndexExpression this[Index index1, Index index2, 
		(Dimension n1, Dimension n2) N]		
		{
			get
			{
				ThrowIfIndicesExceedRank(2);
				Dimension[] shape = new[] {N.n1, N.n2};
				return new TensorIndexExpression(Expression.ArrayAccess(Expression.Constant(new Tensor[,] {{this}}), 
					new Expression[] {Expression.Parameter(typeof(int), index1.Id), 
						Expression.Parameter(typeof(int), index2.Id)}), 
					new IndexSet(this, index1, index2), shape);
			}
			set
			{
				ThrowIfAlreadyAssiged();
				Dimension[] shape = new[] {N.n1, N.n2};
				IndexSet lhsIndexSet = new IndexSet(this, index1, index2);
				TensorContraction tc = new TensorContraction(value, this, lhsIndexSet, shape);
				ContractionDefinition = (lhsIndexSet, tc);
			}
		}
		  
		public TensorIndexExpression this[Index index1, Index index2, Index index3, 
		(Dimension n1, Dimension n2, Dimension n3) N]		
		{
			get
			{
				ThrowIfIndicesExceedRank(3);
				Dimension[] shape = new[] {N.n1, N.n2, N.n3};
				return new TensorIndexExpression(Expression.ArrayAccess(Expression.Constant(new Tensor[,,] {{{this}}}), 
					new Expression[] {Expression.Parameter(typeof(int), index1.Id), 
						Expression.Parameter(typeof(int), index2.Id), 
						Expression.Parameter(typeof(int), index3.Id)}), 
					new IndexSet(this, index1, index2, index3), shape);
			}
			set
			{
				ThrowIfAlreadyAssiged();
				Dimension[] shape = new[] {N.n1, N.n2, N.n3};
				IndexSet lhsIndexSet = new IndexSet(this, index1, index2, index3);
				TensorContraction tc = new TensorContraction(value, this, lhsIndexSet, shape);
				ContractionDefinition = (lhsIndexSet, tc);
			}
		}
		  
		public TensorIndexExpression this[Index index1, Index index2, Index index3, Index index4, 
		(Dimension n1, Dimension n2, Dimension n3, Dimension n4) N]		
		{
			get
			{
				ThrowIfIndicesExceedRank(4);
				Dimension[] shape = new[] {N.n1, N.n2, N.n3, N.n4};
				return new TensorIndexExpression(Expression.ArrayAccess(Expression.Constant(new Tensor[,,,] {{{{this}}}}), 
					new Expression[] {Expression.Parameter(typeof(int), index1.Id), 
						Expression.Parameter(typeof(int), index2.Id), 
						Expression.Parameter(typeof(int), index3.Id), 
						Expression.Parameter(typeof(int), index4.Id)}), 
					new IndexSet(this, index1, index2, index3, index4), shape);
			}
			set
			{
				ThrowIfAlreadyAssiged();
				Dimension[] shape = new[] {N.n1, N.n2, N.n3, N.n4};
				IndexSet lhsIndexSet = new IndexSet(this, index1, index2, index3, index4);
				TensorContraction tc = new TensorContraction(value, this, lhsIndexSet, shape);
				ContractionDefinition = (lhsIndexSet, tc);
			}
		}
		  
		public TensorIndexExpression this[Index index1, Index index2, Index index3, Index index4, Index index5, 
		(Dimension n1, Dimension n2, Dimension n3, Dimension n4, Dimension n5) N]		
		{
			get
			{
				ThrowIfIndicesExceedRank(5);
				Dimension[] shape = new[] {N.n1, N.n2, N.n3, N.n4, N.n5};
				return new TensorIndexExpression(Expression.ArrayAccess(Expression.Constant(new Tensor[,,,,] {{{{{this}}}}}), 
					new Expression[] {Expression.Parameter(typeof(int), index1.Id), 
						Expression.Parameter(typeof(int), index2.Id), 
						Expression.Parameter(typeof(int), index3.Id), 
						Expression.Parameter(typeof(int), index4.Id), 
						Expression.Parameter(typeof(int), index5.Id)}), 
					new IndexSet(this, index1, index2, index3, index4, index5), shape);
			}
			set
			{
				ThrowIfAlreadyAssiged();
				Dimension[] shape = new[] {N.n1, N.n2, N.n3, N.n4, N.n5};
				IndexSet lhsIndexSet = new IndexSet(this, index1, index2, index3, index4, index5);
				TensorContraction tc = new TensorContraction(value, this, lhsIndexSet, shape);
				ContractionDefinition = (lhsIndexSet, tc);
			}
		}
		  
		public TensorIndexExpression this[Index index1, Index index2, Index index3, Index index4, Index index5, Index index6, 
		(Dimension n1, Dimension n2, Dimension n3, Dimension n4, Dimension n5, Dimension n6) N]		
		{
			get
			{
				ThrowIfIndicesExceedRank(6);
				Dimension[] shape = new[] {N.n1, N.n2, N.n3, N.n4, N.n5, N.n6};
				return new TensorIndexExpression(Expression.ArrayAccess(Expression.Constant(new Tensor[,,,,,] {{{{{{this}}}}}}), 
					new Expression[] {Expression.Parameter(typeof(int), index1.Id), 
						Expression.Parameter(typeof(int), index2.Id), 
						Expression.Parameter(typeof(int), index3.Id), 
						Expression.Parameter(typeof(int), index4.Id), 
						Expression.Parameter(typeof(int), index5.Id), 
						Expression.Parameter(typeof(int), index6.Id)}), 
					new IndexSet(this, index1, index2, index3, index4, index5, index6), shape);
			}
			set
			{
				ThrowIfAlreadyAssiged();
				Dimension[] shape = new[] {N.n1, N.n2, N.n3, N.n4, N.n5, N.n6};
				IndexSet lhsIndexSet = new IndexSet(this, index1, index2, index3, index4, index5, index6);
				TensorContraction tc = new TensorContraction(value, this, lhsIndexSet, shape);
				ContractionDefinition = (lhsIndexSet, tc);
			}
		}
		  
		public TensorIndexExpression this[Index index1, Index index2, Index index3, Index index4, Index index5, Index index6, Index index7, 
		(Dimension n1, Dimension n2, Dimension n3, Dimension n4, Dimension n5, Dimension n6, Dimension n7) N]		
		{
			get
			{
				ThrowIfIndicesExceedRank(7);
				Dimension[] shape = new[] {N.n1, N.n2, N.n3, N.n4, N.n5, N.n6, N.n7};
				return new TensorIndexExpression(Expression.ArrayAccess(Expression.Constant(new Tensor[,,,,,,] {{{{{{{this}}}}}}}), 
					new Expression[] {Expression.Parameter(typeof(int), index1.Id), 
						Expression.Parameter(typeof(int), index2.Id), 
						Expression.Parameter(typeof(int), index3.Id), 
						Expression.Parameter(typeof(int), index4.Id), 
						Expression.Parameter(typeof(int), index5.Id), 
						Expression.Parameter(typeof(int), index6.Id), 
						Expression.Parameter(typeof(int), index7.Id)}), 
					new IndexSet(this, index1, index2, index3, index4, index5, index6, index7), shape);
			}
			set
			{
				ThrowIfAlreadyAssiged();
				Dimension[] shape = new[] {N.n1, N.n2, N.n3, N.n4, N.n5, N.n6, N.n7};
				IndexSet lhsIndexSet = new IndexSet(this, index1, index2, index3, index4, index5, index6, index7);
				TensorContraction tc = new TensorContraction(value, this, lhsIndexSet, shape);
				ContractionDefinition = (lhsIndexSet, tc);
			}
		}
		  
		public TensorIndexExpression this[Index index1, Index index2, Index index3, Index index4, Index index5, Index index6, Index index7, Index index8, 
		(Dimension n1, Dimension n2, Dimension n3, Dimension n4, Dimension n5, Dimension n6, Dimension n7, Dimension n8) N]		
		{
			get
			{
				ThrowIfIndicesExceedRank(8);
				Dimension[] shape = new[] {N.n1, N.n2, N.n3, N.n4, N.n5, N.n6, N.n7, N.n8};
				return new TensorIndexExpression(Expression.ArrayAccess(Expression.Constant(new Tensor[,,,,,,,] {{{{{{{{this}}}}}}}}), 
					new Expression[] {Expression.Parameter(typeof(int), index1.Id), 
						Expression.Parameter(typeof(int), index2.Id), 
						Expression.Parameter(typeof(int), index3.Id), 
						Expression.Parameter(typeof(int), index4.Id), 
						Expression.Parameter(typeof(int), index5.Id), 
						Expression.Parameter(typeof(int), index6.Id), 
						Expression.Parameter(typeof(int), index7.Id), 
						Expression.Parameter(typeof(int), index8.Id)}), 
					new IndexSet(this, index1, index2, index3, index4, index5, index6, index7, index8), shape);
			}
			set
			{
				ThrowIfAlreadyAssiged();
				Dimension[] shape = new[] {N.n1, N.n2, N.n3, N.n4, N.n5, N.n6, N.n7, N.n8};
				IndexSet lhsIndexSet = new IndexSet(this, index1, index2, index3, index4, index5, index6, index7, index8);
				TensorContraction tc = new TensorContraction(value, this, lhsIndexSet, shape);
				ContractionDefinition = (lhsIndexSet, tc);
			}
		}
			}
}