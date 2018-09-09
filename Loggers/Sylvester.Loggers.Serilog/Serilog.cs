using System;
using Serilog;

namespace Sylvester.Loggers
{
    public class SerilogLogger : ILogger
    {
        public static LoggerConfiguration LoggerConfiguration { get; protected set; }

        public static bool LoggerConfigured { get; protected set; }

        protected Serilog.ILogger L { get; set; }

        internal SerilogLogger()
        {
            if (!LoggerConfigured)
            {
                throw new InvalidOperationException("The Serilog logger is not configured.");
            }
            L = Log.Logger;
        }

        public static SerilogLogger CreateDefaultLogger(string logFilename = "Adrien.log")
        {
            if (!LoggerConfigured)
            {       
                LoggerConfiguration = new LoggerConfiguration()
                    .WriteTo.RollingFile(logFilename,
                        outputTemplate: "{Timestamp:HH:mm:ss} [{Level:u3}] {Message}{NewLine}{Exception}");
                Serilog.Log.Logger = LoggerConfiguration.CreateLogger();
                LoggerConfigured = true;
            }
            return new SerilogLogger();
        }

        public void Info(string messageTemplate, params object[] propertyValues) 
            => L.Information(messageTemplate, propertyValues);

        public void Debug(string messageTemplate, params object[] propertyValues)
            => L.Debug(messageTemplate, propertyValues);

        public void Error(string messageTemplate, params object[] propertyValues)
            => L.Error(messageTemplate, propertyValues);

        public void Error(Exception e, string messageTemplate, params object[] propertyValues)
            => L.Error(e, messageTemplate, propertyValues);

        public void Verbose(string messageTemplate, params object[] propertyValues)
            => L.Verbose(messageTemplate, propertyValues);

        public void Warn(string messageTemplate, params object[] propertyValues)
            => L.Warning(messageTemplate, propertyValues);
    }
}
