using System;

using CommandLine;

namespace Sylvester.tf.OpGen
{
    class Options
    {
        [Option('d', "dirs", Required = false, HelpText = "Specify the directories that contains the TensorFlow 2 op definitions.")]
        public string Dirs { get; set; }

        [Option('o', "op", Required = false, HelpText = "Only generate code for a specific op.")]
        public string Op { get; set; }

        [Option('f', "file", Default = "Ops.g.cs", Required = false, HelpText = "Set the output filename for the generated code.")]
        public string OutputFileName { get; set; }

        [Option('v', "verbose", Required = false, HelpText = "Enable verbose output.", Default = false)]
        public bool Verbose { get; set; }
    }
}