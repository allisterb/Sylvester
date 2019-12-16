using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Linq;

using Serilog;
using CommandLine;
using CommandLine.Text;
using CppSharp;

namespace Sylvester.Bindings
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
        static Library ProgramLibrary;

        static void Main(string[] args)
        {
            AppDomain.CurrentDomain.UnhandledException += CurrentDomain_UnhandledException;

            LConfig = new LoggerConfiguration()
                .Enrich.FromLogContext()
                .MinimumLevel.Debug()
                .WriteTo.Console(outputTemplate: "{Timestamp:HH:mm:ss}<{ThreadId:d2}> [{Level:u3}] {Message}{NewLine}{Exception}");
            L = Log.Logger = LConfig.CreateLogger();
            Type[] BindOptionTypes = Assembly.GetExecutingAssembly().GetTypes().Where(t => t == typeof(Options) || t.IsSubclassOf(typeof(Options))).ToArray();
            MethodInfo parseArgumentsMethod = typeof(ParserExtensions).GetMethods().Where(m => m.IsGenericMethod && m.Name == "ParseArguments"
                && m.GetGenericArguments().Count() == BindOptionTypes.Count()).First();
            Parser p = new Parser();
            ParserResult<object> result = (ParserResult<object>)parseArgumentsMethod.MakeGenericMethod(BindOptionTypes).Invoke(p, new object[] { p, args });
            result.WithNotParsed((IEnumerable<Error> errors) =>
            {
                HelpText help = GetAutoBuiltHelpText(result);
                help.MaximumDisplayWidth = Console.WindowWidth;
                help.Copyright = string.Empty;
                help.Heading = new HeadingInfo("Sylvester Bindings CLI", Version.ToString(3));
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
                    Log.Information(help);
                    Exit(ExitResult.SUCCESS);
                }
                else if (errors.Any(e => e.Tag == ErrorType.HelpRequestedError))
                {
                    help.AddVerbs(BindOptionTypes);
                    L.Information(help);
                    Exit(ExitResult.SUCCESS);
                }
                else if (errors.Any(e => e.Tag == ErrorType.NoVerbSelectedError))
                {
                    help.AddVerbs(BindOptionTypes);
                    help.AddPreOptionsLine("No library selected. Select a library or verb from the options below:");
                    L.Information(help);
                    Exit(ExitResult.INVALID_OPTIONS);
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
                    help.AddVerbs(BindOptionTypes);
                    L.Information(help);
                    Exit(ExitResult.INVALID_OPTIONS);
                }
            })
            .WithParsed<Options>(o =>
            {
                if (string.IsNullOrEmpty(o.ModuleName))
                {
                    Log.Error($"You must select a module to create bindings for. Use the --help option to get the list of available modules.");
                    Exit(ExitResult.INVALID_OPTIONS);
                }

                if (!string.IsNullOrEmpty(o.Root) && !Directory.Exists(o.Root))
                {
                    Log.Error($"The library root directory specified {o.Root} does not exist.");
                    Exit(ExitResult.INVALID_OPTIONS);
                }
                else if (!string.IsNullOrEmpty(o.Root))
                {
                    ProgramOptions.Add("RootDirectory", new DirectoryInfo(o.Root));
                }
                foreach (PropertyInfo prop in o.GetType().GetProperties())
                {
                    ProgramOptions.Add(prop.Name, prop.GetValue(o));
                }
            })
            .WithParsed<PlaidMLOptions>(o =>
            {
                if (!File.Exists(Path.Combine(AssemblyDirectory.FullName, "plaidml", "base.h")))
                {
                    L.Error($"The PlaidML header file {Path.Combine(AssemblyDirectory.FullName, "plaidml", "base.h")} was not found.");
                    Exit(ExitResult.FILE_MISSING);
                }
                else if (!File.Exists(Path.Combine(AssemblyDirectory.FullName, "plaidml", "plaidml.h")))
                {
                    L.Error($"The PlaidML header file {Path.Combine(AssemblyDirectory.FullName, "plaidml", "plaidml.h")} was not found.");
                    Exit(ExitResult.FILE_MISSING);
                }
                ProgramLibrary = new PlaidML(ProgramOptions);
                ConsoleDriver.Run(ProgramLibrary);
                if (ProgramLibrary.CleanAndFixup())
                {
                    Exit(ExitResult.SUCCESS);
                }
                else
                {
                    Exit(ExitResult.ERROR_DURING_CLEANUP);
                }

            })
            .WithParsed<TensorFlowOptions>(o =>
            {
                if (!File.Exists(Path.Combine(AssemblyDirectory.FullName, "tf", "c_api.h")))
                {
                    L.Error($"The TensorFlow header file {Path.Combine(AssemblyDirectory.FullName, "tf", "c_api.h")} was not found.");
                    Exit(ExitResult.FILE_MISSING);
                }
                ProgramLibrary = new TensorFlow(ProgramOptions);
                ConsoleDriver.Run(ProgramLibrary);
                if (ProgramLibrary.CleanAndFixup())
                {
                    Exit(ExitResult.SUCCESS);
                }
                else
                {
                    Exit(ExitResult.ERROR_DURING_CLEANUP);
                }
            });
        }

        static void Exit(ExitResult result)
        {
            Log.CloseAndFlush();

            Environment.Exit((int)result);
        }

        static int ExitWithCode(ExitResult result)
        {
            Log.CloseAndFlush();
            return (int)result;
        }

        static HelpText GetAutoBuiltHelpText(ParserResult<object> result)
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
