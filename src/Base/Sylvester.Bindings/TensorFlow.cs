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
    public class TensorFlow : Library
    {
        #region Constructors
        public TensorFlow(Dictionary<string, object> options) : base(options)
        {

        }
        #endregion

        #region Overriden members
        public override LibraryKind Kind { get; } = LibraryKind.TensorFlow;

        public override void Setup(Driver driver)
        {
            base.Setup(driver);
            this.Module.Headers.Add(Path.Combine(AssemblyDirectory.FullName, "tf", "tf_attrtype.h"));
            this.Module.Headers.Add(Path.Combine(AssemblyDirectory.FullName, "tf", "tf_datatype.h"));
            this.Module.Headers.Add(Path.Combine(AssemblyDirectory.FullName, "tf", "tf_status.h"));
            this.Module.Headers.Add(Path.Combine(AssemblyDirectory.FullName, "tf", "tf_tensor.h"));
            this.Module.Headers.Add(Path.Combine(AssemblyDirectory.FullName, "tf", "c_api.h"));
            this.Module.Headers.Add(Path.Combine(AssemblyDirectory.FullName, "tf", "c_api-EAGER.h"));
            Info("Using {0} C header files for TensorFlow 2.", this.Module.Headers);
            Info("Creating bindings for TensorFlow functions...");
        }

        /// Setup your passes here.
        public override void SetupPasses(Driver driver)
        {
            driver.AddTranslationUnitPass(new GetAllClassDeclsPass(this, driver.Generator));
        }
        
        #endregion
    }
}
