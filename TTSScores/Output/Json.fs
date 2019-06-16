module Output.Json
open Newtonsoft.Json
let prettyJson obj = JsonConvert.SerializeObject(obj, Formatting.Indented)