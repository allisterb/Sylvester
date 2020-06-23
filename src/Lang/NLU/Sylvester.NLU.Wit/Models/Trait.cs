using Newtonsoft.Json;

namespace Sylvester.NLU.Wit
{
    public class Trait
    {
        [JsonProperty("id")]
        public string Id { get; set; }

        [JsonProperty("value")]
        public string Value { get; set; }

        [JsonProperty("confidence")]
        public float Confidence { get; set; }
    }

}
