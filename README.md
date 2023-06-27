# pathetic

<img src="logo/logo.svg" alt="Pathetic Logo" />

[![CI](https://github.com/yetanalytics/pathetic/actions/workflows/main.yml/badge.svg)](https://github.com/yetanalytics/pathetic/actions/workflows/main.yml) [![Clojars Project](https://img.shields.io/clojars/v/com.yetanalytics/pathetic.svg)](https://clojars.org/com.yetanalytics/pathetic) [![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-5e0b73.svg)](CODE_OF_CONDUCT.md)

Utility Library for working with [JSON Path](https://goessner.net/articles/JsonPath/).

## Installation

Add the following to your `:deps` map in your deps.edn file:

```clojure
com.yetanalytics/pathetic {:mvn/version "0.5.0"
                           :exclusions [org.clojure/clojure
                                        org.clojure/clojurescript]}
```

## Data

Any JSON data is accepted, but at Yet Analytics we will largely be working with [xAPI Statements](https://xapi.com/statements-101/), which can be represented as JSON or EDN maps.

The following JSON example was excised from the xAPI Statement
 at `dev-resources/pathetic/data/long.json`

``` json
{
    "id": "6690e6c9-3ef0-4ed3-8b37-7f3964730bee",
    "result": {
        "success": true,
        "completion": true
    },
    "context": {
        "contextActivities": {
            "category": [
                {
                    "id": "http://www.example.com/meetings/categories/teammeeting"
                }
            ]
        }
    }
}
```

Within this README, the example will be referred to as `stmt`

## Core usage

The core functions of the Pathetic API are found in the namespace `com.yetanalytics.pathetic`. Most functions take an optional `opts-map`
argument; common fields in `opts-map` include:

- `:first?` - Parse or apply only the first path, if the JSONPath string contains multiple paths separated by `|`. Default false.
- `:strict?` - Disallows recursive descent, array slicing, and negative indices. This makes JSONPath strings conform to [xAPI Profile](https://adlnet.github.io/xapi-profiles/xapi-profiles-about.html) spec requirements (listed at the bottom of [this section](https://github.com/adlnet/xapi-profiles/blob/master/xapi-profiles-structure.md#statement-template-rules)). Default false.
- `:return-missing?` - Return paths and/or values at locations not found in the JSONPath object. Missing values are returned as `nil`. Default false.
- `:return-duplicates?` - Return duplicate values from a JSONPath object. Default true.
- `:prune-empty?` - Remove empty collections and values from a JSONPath object after having values excised. Default false.

Each function has two versions: a regular and a starred version. The regular versions accept JSONPath strings, while the starred versions accept parsed paths (which is useful in performance-critical situations). The starred versions do not accept `:first?` or `:strict?` as `opts-map` fields.

### parse-paths

```
Parse a JSONPath string. Each parsed path is a vector with the
following entries:
    '..     recursive descent operator
    '*      wildcard operator
    [...]   a vector of strings (keys), integers (array indices), or
            maps (array slicing operations).
   
Supports :first? and :strict? in `opts-map`.
```

``` clojure
(parse-paths stmt "$.context.contextActivities.grouping[*]")
=> [[["context"] ["contextActivities"] ["grouping"] '*]]

(parse-paths stmt "$.id | $.timestamp")
=> [[["id"]] [["timestamp"]]]

(parse-paths stmt "$.id | $.timestamp" {:first? true})
=> [[["id"]]]
```

### get-paths

```
Given `json` and a JSONPath string `paths`, return a vector of
definite key paths. Each key path is a vector of strings (keys)
or integers (array indices); non-deterministic path entries like
recursive descent and wildcards are removed. If the string
contains multiple JSONPaths, we return the key paths for all
strings.

Supports :first?, :strict?, and :return-missing? in `opts-map`.   
```

``` clojure

(get-paths stmt "$.context.contextActivities.category[*].id")
=> [["context" "contextActivities" "category" 0 "id"]]

(get-paths stmt "$.context.contextActivities.grouping[*]")
=> []

(get-paths stmt "$.context.contextActivities.grouping[*]" {:return-missing? true})
=> [["context" "contextActivities" "grouping"]]
```

### get-values

```
Given `json` and a JSONPath string `paths`, return a vector of
JSON values. If the string contains multiple JSONPaths, we return
the union of all these values.

Supports :first?, :strict?, :return-missing?, and :return-duplicates?
in `opts-map`.   
```

``` clojure
(get-values stmt "$.id")
=> ["6690e6c9-3ef0-4ed3-8b37-7f3964730bee"]

(get-values stmt "$.result.score")
=> []

(get-values stmt "$.result.score" {:return-missing? true})
=> [nil]

(get-values stmt "$.result['success','completion']")
=> [true true]

(get-values stmt "$.result['success','completion']" {:return-duplicates? false})
=> [true]
```

### get-path-value-map

```
Given `json` and a JSONPath string `paths`, return a map associating
JSON paths to JSON values. Does not return duplicates.

Supports :first?, :strict?, and :return-missing? in `opts-map`.   
```

```clojure
(get-path-value-map stmt "$.context.contextActivities.category[*].id")
=> {["context" "contextActivities" "category" 0 "id"]
    "http://www.example.com/meetings/categories/teammeeting"}
```

### select-keys-at

```
Given `json` and a JSONPath string `paths`, return a vector of maps
that represent the key path into the JSON value. If the string
contains multiple JSONPaths, we return the maps for all strings.
If no value exists at the selection, return a truncated map with
"{}" as the innermost possible value.

Supports :first? and :strict? in `opts-map`.   
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
Given `json` and a JSONPath string `paths`, return the JSON value with
the elements at the location removed.
   
Supports :first?, :strict?, and :prune-empty? in `opts-map`.
```

``` clojure
(= (dissoc stmt "id")
   (excise stmt "$.id"))
```

### speculate-paths

```
Given `json` and a JSONPath string `paths`, return a vector of
definite key paths, just like `get-paths`. However, unlike `get-paths`,
paths will be enumerated even if the corresponding value does not exist
in `json` on that path; in other words, it speculates what paths would
exist if they are applied. If the string contains multiple JSONPaths, we
return the key paths for all strings.

Supports :first? and :multi-value? in `opts-map`. :strict? is always overridden to `true`. Also accepts the following args:

:wildcard-append? Dictates if wildcard values should be appended to
                  the end of existing seqs instead of overwriting existing
                  values. Default `false`.
:wildcard-limit   Dictates how many wildcard paths should be generated.
                  In overwrite mode, defaults to the length of each coll.
                  In append mode, defaults to 1.
```

```clojure
(speculate-paths stmt "$.context.contextActivities.grouping[*]")
=> [["context" "contextActivities" "grouping" 0]]

(speculate-paths stmt "$.context.contextActivities.category[*].id")
=> [["context" "contextActivities" "category" 1 "id"]]

(speculate-paths stmt
                 "$.context.contextActivities.category[*].id" 
                 {:wildcard-limit 2})
=> [["context" "contextActivities" "category" 1 "id"]
    ["context" "contextActivities" "category" 2 "id"]]

(speculate-paths stmt
                 "$.context.contextActivities.category[*].id" 
                 {:wildcard-append? false})
=> [["context" "contextActivities" "category" 0 "id"]]
```

### apply-value

```
Given `json`, a JSONPath string `paths`, and the JSON data
`value`, apply `value` to the location given by `paths` If
the location exists, update the pre-existing value. Otherwise,
create the necessary data structures needed to contain `value`.

The following caveats apply:
- If only the root \"$\" is provided, `json` is overwritten in
  its entirety.
- If an array index skips over any vector entries, those skipped
  entries will be assigned nil.
- If a path contains a wildcard and the location up to that
  point does not exist, create a new vector.
- If a path contains a wildcard and the location is a collection,
  append it to the coll. In the case of maps, the key is its
  current size, e.g. {\"2\" : \"foo\"}.
- Recursive descent, array slicing, and negative array indices
  are disallowed (as per strict mode).

Supports :first? and :multi-value? in `opts-map`. :strict? is always overridden to `true`. Also accepts the following args:

:multi-value?     If provided, then `value` must be a collection of
                  values that will be applied in order, e.g. for an
                  array specified by `[0,1]` in the path, then the first
                  and second elements of `value` will be applied. Returns
                  the modified `json` once `value` or the available path
                  seqs runs out.
:wildcard-append? Dictates if wildcard values should be appended to
                  the end of existing seqs instead of overwriting existing
                  values. Default `false`.
:wildcard-limit   Dictates how many wildcard paths should be generated.
                  If `multi-value?`, defaults to the number of values.
                  In overwrite mode, defaults to the length of each coll.
                  In append mode, defaults to 1.
```

``` clojure
(= (assoc-in
    stmt
    ["context" "contextActivities" "category" 0 "id"]
    "http://www.example.com/meetings/categories/brainstorm_sesh")
   (apply-value stmt
                "$.context.contextActivities.category[*].id"
                "http://www.example.com/meetings/categories/brainstorm_sesh"))

(= (update-in
    stmt
    ["context" "contextActivities" "category"]
    (conj old
          {"id" "http://www.example.com/meetings/categories/brainstorm_sesh"}))
   (apply-value stmt
                "$.context.contextActivities.category[*].id"
                "http://www.example.com/meetings/categories/brainstorm_sesh"
                {:wildcard-append? true}))
```

### apply-multi-value

```
"Given `json`, a JSONPath string `paths`, and a collection of JSON data
`value`, apply `value` to the location given by `paths` in the order
they are given. If the location exists, update the pre-existing value.
Otherwise, create the necessary data structures needed to contain `value`.

For example, an array specified by `[0,1]` in the path, then the first
and second elements of `value` will be applied. Returns the modified
`json` once `value` or the available path seqs runs out.

The same caveats as `apply-value` apply here as well.
   
The following `opts-map` fields are supported:
:first?           Apply only the first \"|\"-separated path. Default false.
:strict?          Always overrides to true regardless of value provided.
:wildcard-append? Dictates if wildcard values should be appended to
                  the end of existing seqs instead of overwriting existing
                  values. Default `false`.
:wildcard-limit   Dictates how many wildcard paths should be generated.
                  Defaults to the number of values.
```

```clojure
(= (-> stmt
       (assoc-in ["context" "contextActivities" "category" 0 "id"]
                 "http://www.example.com/meetings/categories/brainstorm_sesh")
       (assoc-in ["context" "contextActivities" "category" 1 "id"]
                 "http://www.example.com/meetings/categories/whiteboard_sesh"))
   (apply-multi-value stmt
                      "$.context.contextActivities.category[*].id"
                      ["http://www.example.com/meetings/categories/brainstorm_sesh"
                       "http://www.example.com/meetings/categories/whiteboard_sesh"]))

(= (update-in
    stmt
    ["context" "contextActivities" "category"]
    (conj old
          {"id" "http://www.example.com/meetings/categories/brainstorm_sesh"})
          {"id" "http://www.example.com/meetings/categories/whiteboard_sesh"})
   (apply-multi-value stmt
                      "$.context.contextActivities.category[*].id"
                      ["http://www.example.com/meetings/categories/brainstorm_sesh"
                       "http://www.example.com/meetings/categories/whiteboard_sesh"]
                      {:wildcard-append? true}))
```

## Other usage

Useful functions can be found in other namespaces.

### json-path/parse

```
"Given a JSON-path, parse it into data. Returns a vector of parsed
JSON-paths, or the first error map if one or more paths are
invalid."
```

```clojure
(parse "$.foo | $.*.bar")
=> [[["foo"]] [* ["bar"]]]
```

### json-path/parse-first

```
Same as `parse`, but returns the first parsed JSON-path, or `nil`
if the paths are invalid.
```

```clojure
(parse "$.foo | $.*.bar")
=> [["foo"]]
```

### json-path/path->string

```
Stringify a parsed path back into a JSONPath string.
```

```clojure
(path->string [* ["books"]])
=> "$[*]['books']"
```

## License

Copyright © 2019-2023 Yet Analytics, Inc.

Distributed under the Apache License version 2.0. 
