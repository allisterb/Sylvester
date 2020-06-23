using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Newtonsoft.Json;

namespace Sylvester.NLU.Wit
{
    public class ConverseResponse
    {
        /// <summary>
        /// The type of the bot response. Either merge (first bot action after a user message), msg (the bot has something to say), action (the bot has something to do) or stop (the bot is waiting to proceed).
        /// </summary>
        public string type { get; set; }

        /// <summary>
        /// The answer of your bot, when applicable.
        /// </summary>
        public string msg { get; set; }

        /// <summary>
        /// The action to execute, when applicable.
        /// </summary>
        public string action { get; set; }

        /// <summary>
        /// Object of entities, when applicable. Each entity is an array of values (even when there is only one value).
        /// </summary>
        public Dictionary<string, List<Entity>> entities { get; set; }

        /// <summary>
        /// Represents the confidence level of the next step, between 0 (low) and 1 (high).
        /// </summary>
        public double confidence { get; set; }

        /// <summary>
        /// The type field in the response defines what you need to do.
        /// All actions but say might alter the context: make sure to call /converse with the updated context!
        /// </summary>
        public enum Types
        {
            /// <summary>
            /// merge means that you need to execute the merge action
            /// </summary>
            merge,

            /// <summary>
            /// msg means that you need to execute the say action
            /// </summary>
            msg,

            /// <summary>
            /// action means that you need to execute the action whose name is specified in the action field
            /// </summary>
            action,

            /// <summary>
            /// stop means that the bot is done: there is nothing left to do until the next user message
            /// </summary>
            stop
        }

        [JsonIgnore]
        public Types typeCode
        {
            get
            {
                switch (type)
                {
                    case "merge":
                        return Types.merge;
                    case "msg":
                        return Types.msg;
                    case "action":
                        return Types.action;
                    case "stop":
                        return Types.stop;
                    default:
                        return Types.stop;
                }
            }
        }
    }
}
