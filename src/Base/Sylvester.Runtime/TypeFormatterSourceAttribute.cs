namespace Sylvester
{
    using System;
    [AttributeUsage(AttributeTargets.Class)]
    public class TypeFormatterSourceAttribute : Attribute
    {
        public TypeFormatterSourceAttribute(Type formatterSourceType)
        {
            FormatterSourceType = formatterSourceType;
        }

        public Type FormatterSourceType { get; }

        public string[] PreferredMimeTypes { get; set; } = { "text/html" };
    }
}
