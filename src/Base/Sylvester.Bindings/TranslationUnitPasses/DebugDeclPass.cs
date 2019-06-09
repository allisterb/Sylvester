using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using CppSharp;
using CppSharp.AST;
using CppSharp.Generators;
using CppSharp.Passes;

namespace Sylvester.Bindings
{
    public class DebugDeclsPass : TranslationUnitPass
    {
        protected Generator G;
        protected Library Library;

        public DebugDeclsPass(Library lib, Generator gen) : base()
        {
            G = gen;
            Library = lib;
        }

        public override bool VisitDeclaration(Declaration decl)
        {
            return base.VisitDeclaration(decl);
        }

        
    }
}
