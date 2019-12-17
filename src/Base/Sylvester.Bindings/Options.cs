using System;

using CommandLine;

namespace Sylvester.Bindings
{
    class Options
    {
        [Option('r', "root", Required = false, HelpText = "Set the root directory on the local filesystem for the native library.")]
        public string Root { get; set; }

        [Option('m', "module", Required = false, HelpText = "Specify the name of a module or subset of the library to generate bindings for.")]
        public string ModuleName { get; set; }

        [Option('o', "output", Required = false, HelpText = "Set the output directory for the class file for the bindings.")]
        public string OutputDirName { get; set; }

        [Option('f', "file", Required = false, HelpText = "Set the output filename for the class file for the bindings. If this is not specified then the module name is used.")]
        public string OutputFileName { get; set; }

        [Option('c', "class", Required = false, HelpText = "Specify the class name for the bindings.")]
        public string Class { get; set; }

        [Option('n', "namespace", Required = false, HelpText = "Specify the namespace that the bindings class will belong to.")]
        public string Namespace { get; set; }

        [Option('l', "library-name", Required = false, HelpText = "Specify the shared library name for the DllImport attribute.")]
        public string LibraryName { get; set; }

        [Option("without-common", Required = false, HelpText = "Do not generate bindings for common MKL data structures and functions", Default = false)]
        public bool WithoutCommon { get; set; }

        [Option('v', "verbose", Required = false, HelpText = "Enable verbose output from CppSharp.", Default = false)]
        public bool Verbose { get; set; }
    }

 
    [Verb("plaidml", HelpText = "Generate bindings for the PlaidML library.")]
    class PlaidMLOptions : Options
    {
        [Option("cpp", Required = false, HelpText = "Generate bindings for the PlaidML C++ ")]
        public bool Cpp { get => ModuleName == "plaidml_cpp"; set => ModuleName = "plaidml_cpp"; }

        public PlaidMLOptions()
        {
            ModuleName = "plaidml";
        }
    }

    [Verb("tf", HelpText = "Generate bindings for the TensorFlow 2 framework.")]
    class TensorFlowOptions : Options
    {
        public TensorFlowOptions()
        {
            ModuleName = "tf";
        }
    }
}