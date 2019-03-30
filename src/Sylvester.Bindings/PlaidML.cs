using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using CppSharp;
using CppSharp.AST;
using CppSharp.Generators;
using CppSharp.Passes;

namespace Sylvester.Bindings
{
    public class PlaidML : Library
    {
        public override LibraryKind Kind { get; } = LibraryKind.PlaidML;


        public PlaidML(Dictionary<string, object> options) : base(options)
        {

        }
       
        public override void Setup(Driver driver)
        {
            base.Setup(driver);
            this.Module.Headers.Add(Path.Combine(AssemblyDirectory.FullName, "base.h"));
            this.Module.Headers.Add(Path.Combine(AssemblyDirectory.FullName, "plaidml.h"));
            Info("Creating bindings for PlaidML functions...");
        }

        public override void SetupPasses(Driver driver)
        {

        }
        /// Do transformations that should happen before passes are processed.
        public override void Preprocess(Driver driver, ASTContext ctx)
        {

        }

        /// Do transformations that should happen after passes are processed.
        public override void Postprocess(Driver driver, ASTContext ctx)
        {
            //ctx.SetClassBindName("@base", "VaiBase");
            //ctx.SetClassAsValueType("@base");
            IEnumerable<Class> classes = 
                ctx.FindClass("VaiCtx")
                .Concat(ctx.FindClass("PlaidmlDevconf"))
                .Concat(ctx.FindClass("PlaidmlDevconf"))
                .Concat(ctx.FindClass("PlaidmlDevice"))
                .Concat(ctx.FindClass("PlaidmlDeviceEnumerator"))
                .Concat(ctx.FindClass("PlaidmlBuffer")) 
                .Concat(ctx.FindClass("PlaidmlMapping"))
                .Concat(ctx.FindClass("PlaidmlShape"))
                .Concat(ctx.FindClass("PlaidmlFunction"))
                .Concat(ctx.FindClass("PlaidmlVar"))
                .Concat(ctx.FindClass("PlaidmlApplier")) 
                .Concat(ctx.FindClass("PlaidmlComposer"))
                .Concat(ctx.FindClass("PlaidmlInvoker"))
                .Concat(ctx.FindClass("PlaidmlInvocatio"))
                .Concat(ctx.FindClass("PlaidmlGradient")); ;

            foreach (Class c in classes)
            {
                ctx.SetClassAsValueType(c.Name);
            }   
        }
        
    }
}
