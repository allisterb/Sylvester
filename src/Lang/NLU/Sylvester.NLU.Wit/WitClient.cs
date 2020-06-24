using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using RestSharp;

namespace Sylvester.NLU.Wit
{
    public class WitClient : Runtime
    {
        #region Constructors
        /// <summary>
        /// Inits the Wit.ai client
        /// </summary>
        /// <param name="token">Your app token, which can be found under Settings in the Wit console</param>
        public WitClient(string token) : base()
        {
            if (string.IsNullOrEmpty(token))
            {
                Initialized = false;
            }
            else
            {
                authValue = "Bearer " + token;
                Initialized = true;
            }
        }
        
        public WitClient() : this(Environment.GetEnvironmentVariable("WIT")) {}
        #endregion

        #region Methods
        /// <summary>
        /// Returns the extracted meaning from a sentence, based on the app data. Note that you may use JSONP to do cross-domain/cross-origin requests. 
        /// </summary>
        /// <see cref="https://wit.ai/docs/http/20160526#get--message-link"/>
        /// <param name="q">User’s query. Length must be &gt; 0 and &lt; 256</param>
        /// <param name="msg_id">A specific Id you want to assign to the message that will be processed. If not set, Wit.ai will auto generate one for you</param>
        /// <param name="thread_id">A specific Id that will let you group requests per conversation</param>
        public Meaning GetMeaning(string q, string msg_id = null, string thread_id = null)
        {
            var client = new RestClient("https://api.wit.ai");

            var request = new RestRequest("message", Method.GET);
            //request.AddQueryParameter("v", DEFAULT_API_VERSION);
            request.AddQueryParameter("q", q);
            if (msg_id != null)
                request.AddQueryParameter("msg_id", msg_id);
            if (thread_id != null)
                request.AddQueryParameter("thread_id", thread_id);

            request.AddHeader("Authorization", authValue);

            // execute the request
            IRestResponse response = client.Execute(request);
            var content = response.Content;

            return JsonConvert.DeserializeObject<Meaning>(content);
        }

        /// <summary>
        /// Returns what your bot should do next. The next step can be either answering to the user, performing an action, or waiting for further requests.
        /// </summary>
        /// <param name="session_id">Unique ID to group messages from the same user request/conversation.</param>
        /// <param name="q">A message from the user. This should only be set at the first step</param>
        /// <param name="context">The object representing the session state.</param>
        /// <returns></returns>
        public ConverseResponse Converse(string session_id, string q = null, object context = null)
        {
            var client = new RestClient("https://api.wit.ai");

            var request = new RestRequest("converse", Method.POST);
            //request.AddQueryParameter("v", DEFAULT_API_VERSION);
            request.AddQueryParameter("session_id", session_id);
            if (q != null)
                request.AddQueryParameter("q", q);

            request.AddHeader("Accept", "application/json");
            request.AddHeader("Authorization", authValue);

            if (context != null)
            {
                request.AddHeader("Content-Type", "application/json");
                request.AddJsonBody(context);
            }

            // execute the request
            IRestResponse response = client.Execute(request);
            var content = response.Content;

            return JsonConvert.DeserializeObject<ConverseResponse>(content);
        }
        #endregion

        #region Fields
        //const string DEFAULT_API_VERSION = "20160516";
        const int DEFAULT_MAX_STEPS = 5;
        const int CALLBACK_TIMEOUT_MS = 10000;
        private string authValue;
        #endregion
    }
}
