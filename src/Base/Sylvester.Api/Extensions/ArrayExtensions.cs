using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sylvester
{
    public static class ArrayExtensions
    {
        public static long[] ToInt64(this int[] arr)
        {
            long[] output = new long[arr.Length];
            for(int i = 0; i < arr.Length; i++)
            {
                output[i] = Convert.ToInt64(arr[i]);
            }
            return output;
        }

        public static int[] ToInt32(this long[] arr)
        {
            int[] output = new int[arr.Length];
            for (int i = 0; i < arr.Length; i++)
            {
                output[i] = checked(Convert.ToInt32(arr[i]));
            }
            return output;
        }
    }
}
