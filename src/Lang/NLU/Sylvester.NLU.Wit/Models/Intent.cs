using Newtonsoft.Json;

namespace Sylvester.NLU.Wit
{
    public class Intent
    {
        [JsonProperty("id")]
        public string Id { get; set; }

        [JsonProperty("name")]
        public string Name { get; set; }

        [JsonProperty("confidence")]
        public float Confidence { get; set; }
    }

}
