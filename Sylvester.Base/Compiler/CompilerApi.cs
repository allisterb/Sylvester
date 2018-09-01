using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;

namespace Sylvester.Compiler
{
    public abstract class CompilerApi<T>
    {
        static DirectoryInfo AssemblyDirectory = new FileInfo(Assembly.GetExecutingAssembly().Location).Directory;

        static Version AssemblyVersion = Assembly.GetExecutingAssembly().GetName().Version;

        protected static ILogger L => CompilerDriver.Logger;

        public CompilerApi()
        {
            if (L == null)
            {
                throw new InvalidOperationException("A logger is not assigned.");
            }
        }

        protected static string GetAssemblyDirectoryFullPath(string path) =>
            Path.Combine(AssemblyDirectory.FullName, path);

        [DebuggerStepThrough]
        protected virtual void Info(string messageTemplate, params object[] propertyValues) =>
            L.Info(messageTemplate, propertyValues);

        [DebuggerStepThrough]
        protected virtual void Debug(string messageTemplate, params object[] propertyValues) =>
            L.Debug(messageTemplate, propertyValues);

        [DebuggerStepThrough]
        protected virtual void Error(string messageTemplate, params object[] propertyValues) =>
            L.Error(messageTemplate, propertyValues);

        [DebuggerStepThrough]
        protected virtual void Error(Exception e, string messageTemplate, params object[] propertyValues) =>
            L.Error(e, messageTemplate, propertyValues);

        [DebuggerStepThrough]
        protected virtual void Verbose(string messageTemplate, params object[] propertyValues) =>
            L.Verbose(messageTemplate, propertyValues);

        [DebuggerStepThrough]
        protected virtual void Warn(string messageTemplate, params object[] propertyValues) =>
            L.Warn(messageTemplate, propertyValues);

        protected static void SetPropFromDict(Type t, object o, Dictionary<string, object> p)
        {
            foreach (var prop in t.GetProperties())
            {
                if (p.ContainsKey(prop.Name) && prop.PropertyType == p[prop.Name].GetType())
                {
                    prop.SetValue(o, p[prop.Name]);
                }
            }
        }

    }
}