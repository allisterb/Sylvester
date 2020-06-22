using System;

using com.valgut.libs.bots.Wit;

namespace Sylvester.NLU.Wit
{
    public class Client : Runtime
    {
        #region Constructors
        public Client(string token) : base()
        {
            _Client = new WitClient(token);
            Initialized = true;
        }
        #endregion

        #region Properties 
        public WitClient _Client { get; protected set; }
        #endregion
    }
}
