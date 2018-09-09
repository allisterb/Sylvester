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
    public class GetAllClassDeclsPass : TranslationUnitPass
    {
        protected Generator G;
        protected Library Library;


        public GetAllClassDeclsPass(Library lib, Generator gen) : base()
        {
            G = gen;
            Library = lib;
        }


        public override bool VisitClassDecl(Class @class)
        {
            Library.ClassDecls.Add(@class.Name);
            return true;
            
        }

        public override bool VisitClassTemplateDecl(ClassTemplate template)
        {
            Library.ClassTemplateDecls.Add(template.Name);
            return true;
        }

        public override bool VisitClassTemplateSpecializationDecl(ClassTemplateSpecialization specialization)
        {
            Library.ClassTemplateSpecializationDecls.Add(specialization.Name);
            return true;
        }

    }
}
