using System;

namespace Sylvester
{
    public abstract class Logger
    {
        public abstract class Op : IDisposable
        {
            public Op(Logger l)
            {
                L = l;
            }

            public Logger L { get; }

            protected bool isCompleted = false;

            protected bool isCancelled = false;

            public abstract void Complete();

            public abstract void Cancel();

            public abstract void Dispose();
        }

        public bool IsConfigured { get; protected set; } = false;

        public abstract void Info(string messageTemplate, params object[] args);

        public abstract void Debug(string messageTemplate, params object[] args);

        public abstract void Error(string messageTemplate, params object[] args);

        public abstract void Error(Exception ex, string messageTemplate, params object[] args);

        public abstract Op Begin(string messageTemplate, params object[] args);
    }

    public class ConsoleOp: Logger.Op
    {
        public ConsoleOp(ConsoleLogger l) : base(l) { }


        public override void Complete()
        {
            L.Info("Complete.");
            isCompleted = true;
        }

        public override void Cancel()
        {
            isCancelled = true;
            L.Error("Cancelled");
        }

        public override void Dispose()
        {
            if(!(isCompleted || isCancelled))
            {
                L.Error("Cancelled.");
            }
        }
    }
        
    public class ConsoleLogger : Logger
    {
        public override void Info(string messageTemplate, params object[] args) => Console.WriteLine(messageTemplate, args);

        public override void Debug(string messageTemplate, params object[] args) => Console.WriteLine(messageTemplate, args);

        public override void Error(string messageTemplate, params object[] args) => Console.WriteLine(messageTemplate, args);

        public override void Error(Exception ex, string messageTemplate, params object[] args) => Console.WriteLine(messageTemplate, args);

        public override Op Begin(string messageTemplate, params object[] args) => new ConsoleOp(this);


    }
}
