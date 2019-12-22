using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sylvester
{
    public class IO : Api
    {
        public IO(): base()
        {
            Initialized = true;
        }

        public static (int, string[]) GetFiles(string pattern, string srcPath = ".", bool recurse = false)
        {
            if (!Directory.Exists(srcPath))
            {
                Error("The path {0} does not exist.", srcPath);
                return (-1, Array.Empty<string>());
            }
            else
            {
                var files = Directory.GetFiles(srcPath, pattern, recurse ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly);
                return (files.Length, files);
            }
        }

        public static int DeleteFiles(string pattern, string srcPath = ".", bool recurse = false)
        {
            if (!Directory.Exists(srcPath))
            {
                Error("The path {0} does not exist.", srcPath);
                return -1;
            }
            else
            {
                var (length, files) = GetFiles(pattern, srcPath, recurse);
                foreach(var file in files)
                {
                    File.Delete(Path.Combine(srcPath, file));
                }
                return files.Length;
            }
        }
    }
}
