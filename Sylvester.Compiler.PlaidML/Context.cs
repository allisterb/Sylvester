using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

using Newtonsoft.Json;

using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class Context : CompilerApi<Context>, ITensorContext
    {
        protected IntPtr ptr;

        protected string logFileName;

        protected FileInfo logFile;
        

        public bool IsAllocated { get; protected set; }

        public VaiStatus LastStatus { get; protected set; }

        public string LastStatusString { get; protected set; }

        public Settings Settings { get; protected set; }

        public List<INDArray> Tensors { get; } = new List<INDArray>();

        public Context(string eventLogFileName, Settings.UseConfigFile configFile)
        {            
            ptr = @base.__Internal.VaiAllocCtx();
            if (ptr.IsZero())
            {
                IsAllocated = false;
                ReportApiCallError("vai_alloc_ctx");
                return;
            }

            Dictionary<string, string> _logConfig = new Dictionary<string, string>
            {
                {"@type", "type.vertex.ai/vertexai.eventing.file.proto.EventLog"},
                { "filename", eventLogFileName }
            };
            string logConfig = JsonConvert.SerializeObject(_logConfig);

            if (@base.__Internal.VaiSetEventlog(ptr, logConfig))
            {
                Info($"PlaidML event log file is {GetAssemblyDirectoryFullPath(eventLogFileName)}.");
            }
            else
            {
                ReportApiCallError("vai_set_event_log");
            }

            Settings = new Settings(configFile);
            IsAllocated = Settings.IsLoaded;
        }

        public Context() : this("PlaidML.log", Settings.UseConfigFile.Default) {}
        
        public Context(Settings.UseConfigFile configFile) : this("PlaidML.log", configFile) {}


        public static implicit operator IntPtr(Context c)
        {
            if (!c.IsAllocated)
            {
                throw new InvalidOperationException("This context is not allocated.");
            }
            else
            {
                return c.ptr;
            }
        }
        

        public void Free()
        {
            ThrowIfNotAllocated();
            @base.__Internal.VaiFreeCtx(ptr);
            ptr = IntPtr.Zero;
            IsAllocated = false;
        }
        
        public void Cancel()
        {
            ThrowIfNotAllocated();
            @base.__Internal.VaiCancelCtx(ptr);
        }

        internal void ThrowIfNotAllocated()
        {
            if (!IsAllocated)
            {
                throw new InvalidOperationException($"This context is not allocated");
            }
        }

        protected void ReportApiCallError(string call) => Error("Call to {0} returned null or false. Status : {1} {2}", call,
            LastStatus = @base.VaiLastStatus(), LastStatusString = @base.VaiLastStatusStr());
    }
}
