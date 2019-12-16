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
        #region Constructors
        public PlaidML(Dictionary<string, object> options) : base(options)
        {

        }
        #endregion

        #region Overriden members
        public override LibraryKind Kind { get; } = LibraryKind.PlaidML;

        public override void Setup(Driver driver)
        {
            base.Setup(driver);
            this.Module.Headers.Add(Path.Combine(AssemblyDirectory.FullName, "plaidml", "base.h"));
            this.Module.Headers.Add(Path.Combine(AssemblyDirectory.FullName, "plaidml", "plaidml.h"));
            Info("Creating bindings for PlaidML functions...");
        }

        /// Setup your passes here.
        public override void SetupPasses(Driver driver)
        {
            driver.AddTranslationUnitPass(new GetAllClassDeclsPass(this, driver.Generator));
            driver.AddTranslationUnitPass(new ConvertFunctionParameterDeclsPass(this, driver.Generator));
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
        #endregion
    }
}
