using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvester
{
    public class AmbiguousMatchInFrameException : Exception
    {
        public AmbiguousMatchInFrameException(string name) : base($"Ambiguous match: {name}.") {}
    }

    public class SameKeyExistsInFrameException : Exception
    {
        public SameKeyExistsInFrameException(string key) : base($"Same key exists: {key}.") { }
    }

    public class KeyDoesNotExistInFrameException : Exception
    {
        public KeyDoesNotExistInFrameException(string key) : base($"Same key exists: {key}.") { }
    }

    public class TypeIsGenericException : Exception
    {
        public TypeIsGenericException(Type type) : base($"Type is generic: {type.Name}.") { }
    }

    public class TypeContainsGenericParametersException : Exception
    {
        public TypeContainsGenericParametersException(Type type) : base($"Type contains generic parameters: {type.Name}.") { }
    }

    public class CollectionReadOnlyException : Exception {}

    public class CollectionModifiedWhileEnumeratingException : Exception {}

    public class FrameUnrestrictedMembersNotEnabledException : Exception
    {
        public FrameUnrestrictedMembersNotEnabledException() : base("Unrestricted members are not allowed for this frame.") {}
    }

    public class TypeNotAnonymousException : Exception
    {
        public TypeNotAnonymousException() : base("The object is not an instance of an anonymous type.") { }
    }

    public class FrameRColumnAddException : Exception
    {
        public FrameRColumnAddException(string col) : base($"Could not dynamically add the column ${col} to the FrameR object.") { }
    }
}
