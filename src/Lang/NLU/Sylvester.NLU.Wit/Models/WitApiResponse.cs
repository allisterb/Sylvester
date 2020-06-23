using Newtonsoft.Json;

namespace Sylvester.NLU.Wit
{
    public class WitApiResponse
    {
        [JsonProperty("error")]
        public string Error { get; set; }

        [JsonProperty("code")]
        public string Code { get; set; }
    }
}
