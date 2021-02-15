# pathetic

Utility Library for working with [JSON Path](https://goessner.net/articles/JsonPath/).

## Data

Any JSON data is accepted, but given our domain, we will largely be working
with xAPI Statements. 

The following example is taken from: `./resources/pathetic/data/long.json`

``` json
{
    "id": "6690e6c9-3ef0-4ed3-8b37-7f3964730bee",
    "actor": {
        "name": "Team PB",
        "mbox": "mailto:teampb@example.com",
        "member": [
            {
                "name": "Andrew Downes",
                "account": {
                    "homePage": "http://www.example.com",
                    "name": "13936749"
                },
                "objectType": "Agent"
            },
            {
                "name": "Toby Nichols",
                "openid": "http://toby.openid.example.org/",
                "objectType": "Agent"
            },
            {
                "name": "Ena Hills",
                "mbox_sha1sum": "ebd31e95054c018b10727ccffd2ef2ec3a016ee9",
                "objectType": "Agent"
            }
        ],
        "objectType": "Group"
    },
    "verb": {
        "id": "http://adlnet.gov/expapi/verbs/attended",
        "display": {
            "en-GB": "attended",
            "en-US": "attended"
        }
    },
    "object": {
        "id": "http://www.example.com/meetings/occurances/34534",
        "definition": {
            "extensions": {
                "http://example.com/profiles/meetings/activitydefinitionextensions/room": {"name": "Kilby", "id" : "http://example.com/rooms/342"}
            },
            "name": {
                "en-GB": "example meeting",
                "en-US": "example meeting"
            },
            "description": {
                "en-GB": "An example meeting that happened on a specific occasion with certain people present.",
                "en-US": "An example meeting that happened on a specific occasion with certain people present."
            },
            "type": "http://adlnet.gov/expapi/activities/meeting",
            "moreInfo": "http://virtualmeeting.example.com/345256"
        },
        "objectType": "Activity"
    },
    "result": {
        "extensions": {
            "http://example.com/profiles/meetings/resultextensions/minuteslocation": "X:\\meetings\\minutes\\examplemeeting.one"
        },
        "success": true,
        "completion": true,
        "response": "We agreed on some example actions.",
        "duration": "PT1H0M0S"
    },
    "context": {
        "registration": "ec531277-b57b-4c15-8d91-d292c5b2b8f7",
        "contextActivities": {
            "parent": [
                {
                    "id": "http://www.example.com/meetings/series/267",
                    "objectType": "Activity"
                }
            ],
            "category": [
                {
                    "id": "http://www.example.com/meetings/categories/teammeeting",
                    "objectType": "Activity",
                    "definition": {
			            "name": {
			                "en": "team meeting"
			            },
			            "description": {
			                "en": "A category of meeting used for regular team meetings."
			            },
			            "type": "http://example.com/expapi/activities/meetingcategory"
			        }
                }
            ],
            "other": [
                {
                    "id": "http://www.example.com/meetings/occurances/34257",
                    "objectType": "Activity"
                },
                {
                    "id": "http://www.example.com/meetings/occurances/3425567",
                    "objectType": "Activity"
                }
            ]
        },
        "instructor" :
        {
        	"name": "Andrew Downes",
            "account": {
                "homePage": "http://www.example.com",
                "name": "13936749"
            },
            "objectType": "Agent"
        },
        "team":
        {
        	"name": "Team PB",
        	"mbox": "mailto:teampb@example.com",
        	"objectType": "Group"
        },
        "platform" : "Example virtual meeting software",
        "language" : "tlh",
        "statement" : {
        	"objectType":"StatementRef",
        	"id" :"6690e6c9-3ef0-4ed3-8b37-7f3964730bee"
        }

    },
    "timestamp": "2013-05-18T05:32:34.804Z",
    "stored": "2013-05-18T05:32:34.804Z",
    "authority": {
        "account": {
            "homePage": "http://cloud.scorm.com/",
            "name": "anonymous"
        },
        "objectType": "Agent"
    },
    "version": "1.0.0"
}
```

Within this README, the above JSON xAPI Statement will be referred to as `stmt`

## Usage

The following API functions, with the exception of `path-spec/path->spec`, can be found in the `com.yetanalytics.pathetic` namespace.

### parse-path

```
Parse a JSONPath string. Each parsed path is a vector with the
following entries:
    '..     recursive descent operator
    '*      wildcard operator
    [...]   a vector of strings (keys), integers (array indices), or
            maps (array slicing operations).
   
The following optional arguments are supported:
    :first?   Return the first path when multiple paths are joined
              using the \"|\" operator. Default false (in which case
              a vector of paths is returned).
    :strict?  If true, disallows recursive descent, array slicing,
              and negative indices. Conformant to the xAPI Profile
              spec and used by apply-values. Default false.
```

``` clojure
(parse-path stmt "$.context.contextActivities.grouping[*]")
=> [[["context"] ["contextActivities"] ["grouping"] '*]]

(parse-path stmt "$.id | $.timestamp")
=> [[["id"]] [["timestamp"]]]

(parse-path stmt "$.id | $.timestamp" :first? true)
=> [["id"]]
```

### get-paths

```
Given JSON data and a JSONPath string, return a vector of
definite key paths. Each key path is a vector of strings (keys)
or integers (array indices); non-deterministic path entries like
recursive descent and wildcards are removed. If the string
contains multiple JSONPaths, we return the key paths for all
strings.
   
The following optional arguments are supported:
    :return-missing?  Return paths that cannot match any location
                      in the JSON data as nil. Default false.
```

``` clojure

(get-paths stmt "$.context.contextActivities.category[*].id")
=> [["context" "contextActivities" "category" 0 "id"]]

(get-paths stmt "$.context.contextActivities.grouping[*]")
=> []

(get-paths stmt "$.context.contextActivities.grouping[*]" :return-missing? true)
=> [["context" "contextActivities" "grouping"]]
```

### get-values

```
Given JSON data and a JSONPath string, return a vector of
JSON values. If the string contains multiple JSONPaths, we return
the union of all these values.
   
The following optional arguments are supported:
    :return-missing?     Return values that cannot be found in the
                         JSON data as nil. Default false.
    :return-duplicates?  Return duplicate values in the array. Default
                         true.
```

``` clojure
(get-values stmt "$.id")
=> ["6690e6c9-3ef0-4ed3-8b37-7f3964730bee"]

(get-values stmt "$.result.score")
=> []

(get-values stmt "$.result.score" :return-missing? true)
=> [nil]

(get-values stmt "$.result['success','completion']")
=> [true true]

(get-values stmt "$.result['success','completion']" :return-duplicates? false)
=> [true]
```

### get-path-value-map

```
Given JSON data nd a JSONPath string, return a map associating
JSON paths to JSON values. Does not return duplicates.

The following optional arguments are supported:
    :return-missing?  Return path-value pairs where the path cannot
                      match any location in the JSON data. The object
                      is returned as nil. Default false.
```

```clojure
(get-path-value-map stmt "$.context.contextActivities.category[*].id")
=> {["context" "contextActivities" "category" 0 "id"]
    "http://www.example.com/meetings/categories/teammeeting"}
```

### select-keys-at

```
Given JSON data and a JSONPath string, return a vector of maps
that represent the key path into the JSON value. If the string
contains multiple JSONPaths, we return the maps for all strings.
If no value exists at the selection, return a truncated map with
"{}" as the innermost possible value.
   
The following optional arguments are supported:
    :first?  Returns the maps corresponding to the first path (if
             paths are separated by \"|\"). Default false.
```

``` clojure
(select-keys-at stmt "$.id")
=> {"id" "6690e6c9-3ef0-4ed3-8b37-7f3964730bee"}

(select-keys-at stmt "$.context.contextActivities.category[*].id")
=> {"context"
    {"contextActivities"
     {"category"
      [{"id" "http://www.example.com/meetings/categories/teammeeting"}]}}}
```


### excise

```
Given JSON data and a JSONPath string, return the JSON value with
the elements at the location removed.
   
The following optional arguments are supported:
    :prune-empty?  Removes empty maps and vectors, as well as
                   key-value pairs where values are empty, after the
                   elements are excised. Default false.
```

``` clojure
(= (dissoc stmt "id")
   (excise stmt "$.id"))
```


### apply-values

```
Given JSON data, a JSONPath string, and a JSON value, apply the
value to the location given by the path. If the location exists,
update the pre-existing value. Otherwise, create the necessary
data structures needed to contain the JSON value.

The following caveats apply:
- If an array index skips over any vector entries, those skipped
    entries will be assigned nil.
- If a path contains a wildcard and the location up to that
    point does not exist, create a new vector.
- If a path contains a wildcard and the location is a collection,
    append it to the coll. In the case of maps, the key is its
    current size, e.g. {\"2\" : \"foo\"}.
- Recursive descent, array slicing, and negative array indices
    are disallowed (as per strict mode).
```

``` clojure
(= (update-in
    stmt
    ["context" "contextActivities" "category"]
    (fn [old] (conj old
                    {"id" "http://www.example.com/meetings/categories/brainstorm_sesh"})))
   (apply-value stmt
                "$.context.contextActivities.category[*].id"
                "http://www.example.com/meetings/categories/brainstorm_sesh"))
```

### path-spec/path->spec

```
Given a root spec and a parsed path into it, return the spec for
that path, or nil if it is not possible. Accepts optional hint
data (i.e. an xAPI Statement) to dispatch multi-specs
```

Unlike the previous API functions, this function is designed specifically for xAPI Statements.

```clojure
(path->spec ::xapi-schema/activity
            ["definition" "name" "en-US"])
=> ::xapi-schema/language-map-text


(path->spec ::xapi-schema/statement
            ["object" "definition" "correctResponsesPattern" 0])
=> string?
````

## License

Copyright © 2019-2020 Yet Analytics, Inc.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
