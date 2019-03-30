using System;
using System.Collections.Generic;
using System.Text;

using Sylvester.Compiler.PlaidML.Bindings;

namespace Sylvester.Compiler.PlaidML
{
    public class PlaidMLApiException<T> : Exception where T : PlaidMLApi<T> 
    {
        public PlaidMLApi<T> ApiObject { get; protected set; }

        public VaiStatus LastApiStatus { get; protected set; }

        public string LastApiStatusMessage { get; protected set; }

        public PlaidMLApiException(PlaidMLApi<T> apiObject, string message) : base(message)
        {
            ApiObject = apiObject;
            LastApiStatus = apiObject.LastStatus;
            LastApiStatusMessage = ApiObject.LastStatusString;
            
        }
    }
}
