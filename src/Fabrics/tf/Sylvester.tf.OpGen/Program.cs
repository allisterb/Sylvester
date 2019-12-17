using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;

using Serilog;
namespace Sylvester.tf.OpGen
{
    class Program
    {
        public enum ExitResult
        {
            SUCCESS = 0,
            UNHANDLED_EXCEPTION = 1,
            INVALID_OPTIONS = 2,
            ERROR_DURING_CLEANUP = 3,
            FILE_MISSING
        }

        static DirectoryInfo AssemblyDirectory = new FileInfo(Assembly.GetExecutingAssembly().Location).Directory;
        static System.Version Version = Assembly.GetExecutingAssembly().GetName().Version;
        static LoggerConfiguration LConfig;
        static ILogger L;
        static Dictionary<string, object> ProgramOptions = new Dictionary<string, object>();

        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");
        }
    }
}
