using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Threading;
using System.Linq;

namespace Sylvester
{
    public delegate void RecognizedHandler(string sentence);

    public delegate void ListeningHandler();
    public class ConsoleSession : Runtime
    {
        #region Constructors
        public ConsoleSession(CancellationToken ct, string exe, params string[] args) : base(ct)
        {
            Exe = exe ?? throw new ArgumentNullException("exe");
            Args = args; 
            Process = new ConsoleProcess(Exe, Args.ToArray(), onOutput: OnProcessOutput, onError: OnErrorOutput);
            Initialized = Process.Initialized;
        }
        public ConsoleSession(string exe, params string[] args) : this(Runtime.Cts.Token, exe, args)
        {

        }
        #endregion

        #region Properties
        public virtual string Exe { get; protected set; }

        public string[] Args { get; protected set; }

        public bool IsStarted { get; protected set; } = false;

        public bool IsListening { get; protected set; } = false;

        public bool IsStopped => Process != null && Process.IsDisposed;

        public bool IsPass1Recognizing { get; protected set; } = false;

        public bool IsPass1Complete { get; protected set; } = false;

        public string Pass1Text { get; protected set; }

        protected ConsoleProcess Process { get; set; }

        protected Logger.Op Pass1RecognizingOp { get; set; }
        #endregion

        #region Methods
        public void Start()
        {
            if (IsStarted)
            {
                throw new InvalidOperationException("The Julius session is already started.");
            }
            else if (IsStopped)
            {
                Process = new ConsoleProcess(Exe, Args, onOutput: OnProcessOutput, onError: OnErrorOutput);
            }
            Process.Start();
            IsStarted = true;
        }

        public void Stop()
        {
            if (!IsStarted)
            {
                throw new InvalidOperationException("The Julius session is not started.");
            }
            Process.Stop();
            IsListening = false;
            IsStarted = false;
        }

        public void WaitForExit()
        {
            Process.WaitForExit();
        }
        public void OnProcessOutput(string line)
        {
            if (!IsListening && line.Trim().Contains("So, the first input will not be recognized."))
            {
                IsListening = true;
                Listening?.Invoke();
            }
            else if (line.Trim().StartsWith("sentence1: <s> "))
            {
                IsPass1Recognizing = false;
                IsPass1Complete = true;
                Pass1Text = line.Replace("sentence1: <s> ", "").Replace("</s>", "").Trim();
                Debug("Recognized text: {0}", Pass1Text);
                Recognized?.Invoke(Pass1Text);
            }
            /*
            if (line.Trim().StartsWith("pass1_best"))
            {
                IsListening = false;
                IsPass1Recognizing = true;
                Pass1RecognizingOp = Begin("Recognizing");
            }
            else if (line.Trim().StartsWith("sentence1: <s> "))
            {
                Pass1RecognizingOp.Complete();
                IsPass1Recognizing = false;
                IsPass1Complete = true;
                Pass1Text = line.Replace("sentence1: <s> ", "").Replace("</s>", "").Trim();
                Info("Recognized text: {0}", Pass1Text);
                Info("Listening...");
            }*/

        }

        public void OnErrorOutput(string line)
        {
            if (line.ToLower().StartsWith("error:"))
            {
                Error(line);
            }

        }
        #endregion

        #region Events
        public event RecognizedHandler Recognized;

        public event ListeningHandler Listening;
        #endregion
    }
}
