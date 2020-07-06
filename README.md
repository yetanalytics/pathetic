# pathetic

Utility Library for working with [JSON Path](https://goessner.net/articles/JsonPath/)

## Data

Any JSON but given our domain...the JSON is expected to be an xAPI Statement; see `./resources/pathetic/data/long.json`

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

> Within this README, the above JSON xAPI Statement will be refered to as `stmt`


## Usage

moc "API" docs for this util lib


### com.yetanalytics.pathetic

Provides Idomatic Clojure core-esq fns which expect JSON-path instead of key seqs

- Examples are provided but more can be found in test namespaces


### enumerate

`ns` = `com.yetanalytics.pathetic.json-path`

> "Given a JSON-path passed through `parse`, enumerate all possible branching pathways when ambigious within JSON-path"

``` clojure

(= (vec
     (com.yetanalytics.pathetic.json-path/enumerate
      (com.yetanalytics.pathetic.json-path/parse "$.context.contextActivities.grouping[*]")
      :limit 3))
   [["context" "contextActivities" "grouping" 0]
    ["context" "contextActivities" "grouping" 1]
    ["context" "contextActivities" "grouping" 2]])
```


#### parse

`ns` = `com.yetanalytics.pathetic.json-path`

> "Given a JSON-path, parse it into data"

``` clojure

(= (com.yetanalytics.pathetic.json-path/parse
    "$.context.contextActivities.grouping[*]")
   [#{"context"} #{"contextActivities"} #{"grouping"} '*])

(= (com.yetanalytics.pathetic.json-path/parse
    "$.context.extensions['https://w3id.org/xapi/cmi5/context/extensions/sessionid']")
   [#{"context"} #{"extensions"} #{"https://w3id.org/xapi/cmi5/context/extensions/sessionid"}])
```


#### select-keys-at

`ns` = `com.yetanalytics.pathetic`

> "Given json data and a path, return the selection. Note that this does not
  return the json-path selection, just the pruned datastructure as with
  clojure.core/select-keys"

``` clojure

(= (select-keys-at stmt "$.id")
   {"id" "6690e6c9-3ef0-4ed3-8b37-7f3964730bee"})

(= (select-keys-at stmt "$.context.contextActivities.category[*].id")
   {"context"
     {"contextActivities"
      {"category"
       [{"id" "http://www.example.com/meetings/categories/teammeeting"}]}}})
```


#### get-at

`ns` = `com.yetanalytics.pathetic`

> "Given json data and a parsed path, return a selection vector."

``` clojure

(= (get-at stmt (json-path/parse "$.id"))
   ["6690e6c9-3ef0-4ed3-8b37-7f3964730bee"])

```


#### excise

`ns` = `com.yetanalytics.pathetic`

> "Given json data and a parsed path, return the data without the selection, and
  any empty container.
  If :prune-empty? is true, will remove empty arrays and maps"

``` clojure

(= (dissoc stmt "id")
   (excise stmt (json-path/parse "$.id")))

```


#### apply-values

`ns` = `com.yetanalytics.pathetic`

> "Given json data, path and values, apply them to the structure.
  If there is no place to put a value, enumerate further possible paths and use
  those."

``` clojure

(= (update-in long-statement ["context" "contextActivities" "category"]
                    (fn [old] (conj old
                                    {"id" "http://www.example.com/meetings/categories/brainstorm_sesh"}
                                    {"id" "http://www.example.com/meetings/categories/whiteboard_sesh"})))
         (apply-values long-statement
                       (json-path/parse "$.context.contextActivities.category[*].id")
                       ["http://www.example.com/meetings/categories/brainstorm_sesh"
                        "http://www.example.com/meetings/categories/whiteboard_sesh"]))

```


## License

Copyright © 2020 Yet Analytics, Inc.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
