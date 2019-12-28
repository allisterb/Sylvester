using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester.Generator
{
    public class LanguageGeneratorException<TOp, TWriter> : Exception where TWriter : LanguageWriter<TOp>  
    {
        public LanguageGenerator<TOp, TWriter> LanguageGenerator { get; protected set; }

        public LanguageGeneratorException(LanguageGenerator<TOp, TWriter> languageGenerator, string message) : 
            base(message)
        {
            LanguageGenerator = languageGenerator;
        }
    }
}
