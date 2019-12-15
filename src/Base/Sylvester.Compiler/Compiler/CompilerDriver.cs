using System;

namespace Sylvester
{
    public class CompilerDriver
    {
        public static ILogger Logger { get; private set; }
        
        public static void SetLogger(Func<ILogger> logger)
        {
            lock(setLogLock)
            {
                if (Logger == null)
                {
                    Logger = logger();
                }
            }
        }
        private static object setLogLock = new object();
    }
}