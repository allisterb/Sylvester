using Newtonsoft.Json;

namespace Sylvester.NLU.Wit
{
    public class Entity
    {
        [JsonProperty("id")]
        public string Id { get; set; }
        
        [JsonProperty("name")]
        public string Name { get; set; }
        
        [JsonProperty("role")]
        public string Role { get; set; }
        
        [JsonProperty("start")]
        public int Start { get; set; }
        
        [JsonProperty("end")]
        public int End { get; set; }

        [JsonProperty("body")]
        public string Body { get; set; }
        
        [JsonProperty("confidence")]
        public float Confidence { get; set; }

        [JsonProperty("entities")]
        public object[] Entities { get; set; }

        [JsonProperty("value")]
        public string Value { get; set; }

        [JsonProperty("type")]
        public string Type { get; set; }
    }

}
