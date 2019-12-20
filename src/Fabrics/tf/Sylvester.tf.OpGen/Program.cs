using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Reflection;

using Serilog;
using CommandLine;
using CommandLine.Text;

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
        static Generator Generator;

        static void Main(string[] args)
        {
            AppDomain.CurrentDomain.UnhandledException += CurrentDomain_UnhandledException;

            LConfig = new LoggerConfiguration()
                .Enrich.FromLogContext()
                .MinimumLevel.Debug()
                .WriteTo.Console(outputTemplate: "{Timestamp:HH:mm:ss}<{ThreadId:d2}> [{Level:u3}] {Message}{NewLine}{Exception}");
            L = Log.Logger = LConfig.CreateLogger();
            Parser p = new Parser();
            ParserResult<Options> result = p.ParseArguments<Options>(args);
            result.WithNotParsed((IEnumerable<Error> errors) =>
            {
                HelpText help = GetAutoBuiltHelpText(result);
                help.MaximumDisplayWidth = Console.WindowWidth;
                help.Copyright = string.Empty;
                help.Heading = new HeadingInfo("Sylvester.tf OpGen CLI", Version.ToString(3));
                help.AddPreOptionsLine(string.Empty);
                if (errors.Any(e => e.Tag == ErrorType.VersionRequestedError))
                {
                    Log.Information(help);
                    Exit(ExitResult.SUCCESS);
                }
                else if (errors.Any(e => e.Tag == ErrorType.HelpVerbRequestedError))
                {
                    HelpVerbRequestedError error = (HelpVerbRequestedError)errors.First(e => e.Tag == ErrorType.HelpVerbRequestedError);
                    if (error.Type != null)
                    {
                        help.AddVerbs(error.Type);
                    }
                    else
                    {
                        help.AddVerbs(typeof(Options));
                    }
                    Log.Information(help);
                    Exit(ExitResult.SUCCESS);
                }
                else if (errors.Any(e => e.Tag == ErrorType.HelpRequestedError))
                {
                    help.AddOptions(result);
                    L.Information(help);
                    Exit(ExitResult.SUCCESS);
                }
                else if (errors.Any(e => e.Tag == ErrorType.MissingRequiredOptionError))
                {
                    MissingRequiredOptionError error = (MissingRequiredOptionError)errors.First(e => e is MissingRequiredOptionError);
                    help.AddOptions(result);
                    help.AddPreOptionsLine($"A required option or value is missing: {error.NameInfo.NameText} The options and values for this benchmark category are: ");
                    L.Information(help);
                    Exit(ExitResult.INVALID_OPTIONS);
                }
                else if (errors.Any(e => e.Tag == ErrorType.MissingValueOptionError))
                {
                    MissingValueOptionError error = (MissingValueOptionError)errors.First(e => e.Tag == ErrorType.MissingValueOptionError);
                    help.AddOptions(result);
                    help.AddPreOptionsLine($"A required option or value is missing. The options and values for this category are: ");
                    L.Information(help);
                    Exit(ExitResult.INVALID_OPTIONS);
                }
                else if (errors.Any(e => e.Tag == ErrorType.UnknownOptionError))
                {
                    UnknownOptionError error = (UnknownOptionError)errors.First(e => e.Tag == ErrorType.UnknownOptionError);
                    help.AddOptions(result);
                    help.AddPreOptionsLine($"Unknown option: {error.Token}.");
                    L.Information(help);
                    Exit(ExitResult.INVALID_OPTIONS);
                }
                else
                {
                    help.AddPreOptionsLine($"An error occurred parsing the program options: {string.Join(" ", errors.Select(e => e.Tag.ToString()).ToArray())}");
                    help.AddVerbs(typeof(Options));
                    L.Information(help);
                    Exit(ExitResult.INVALID_OPTIONS);
                }
            });
            Generator = new Generator(args.FirstOrDefault());
        }

        public static void Exit(ExitResult result)
        {
            Log.CloseAndFlush();

            Environment.Exit((int)result);
        }

        static HelpText GetAutoBuiltHelpText(ParserResult<Options> result)
        {
            return HelpText.AutoBuild(result, h =>
            {
                h.AddOptions(result);
                return h;
            },
            e =>
            {
                return e;
            });
        }

        private static void CurrentDomain_UnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            Exception exception = (Exception)e.ExceptionObject;
            Log.Error(exception, "An unhandled exception occurred. The program will now shutdown.");
            Log.Error(exception.StackTrace);
            Exit(ExitResult.UNHANDLED_EXCEPTION);
        }
    }
}
