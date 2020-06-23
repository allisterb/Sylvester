using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Newtonsoft.Json;

namespace Sylvester.NLU.Wit
{
    public class Meaning : WitApiResponse
    {
        [JsonProperty("text")]
        public string Text { get; set; }

        [JsonProperty("intents")]
        public List<Intent> Intents { get; set; }
        
        [JsonProperty("entities")]
        public Dictionary<string, List<Entity>> Entities { get; set; }
        
        [JsonProperty("traits")]
        public Dictionary<string, List<Trait>> Traits { get; set; }
    }
}
