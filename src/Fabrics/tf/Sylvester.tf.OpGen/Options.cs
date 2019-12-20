using System;

using CommandLine;

namespace Sylvester.tf.OpGen
{
    class Options
    {
        [Option('d', "def-dir", Required = false, HelpText = "Set the rectory on the local filesystem for the native library.")]
        public string Root { get; set; }

        [Option('o', "op", Required = false, HelpText = "Only generate code for a specific op.")]
        public string OutputDirName { get; set; }

        [Option('f', "file", Required = false, HelpText = "Set the output filename for the class file for the bindings. If this is not specified then the module name is used.")]
        public string OutputFileName { get; set; }

        [Option('v', "verbose", Required = false, HelpText = "Enable verbose output from CppSharp.", Default = false)]
        public bool Verbose { get; set; }
    }
}