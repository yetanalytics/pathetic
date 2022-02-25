# Change Log

## 0.4.0 - 2022-02-25
- Remove `path-spec` namespace and associated code.

## 0.3.2 - 2021-12-20
- Deployed Pathetic to Clojars.

## 0.3.1 - 2021-08-13
- Pathetic is now OSS!

## 0.3.0 - 2021-03-04
- Refactored API:
  - Renamed `parse-path` to `parse-paths`.
  - Add starred versions of API functions, which take already-parsed paths as args.
  - Replace keyword args with option maps.
  - Add additional options to API functions, e.g. `:first?` and `:strict?`.
  - Changed auxillary functions in `json-path.cljc` and `json.cljc`.
- Applied optimizations to path sequencing algorithm.
- Add specs and generative tests.
- Fixes to edge cases (e.g. `"$"`).

## 0.2.0 - 2021-02-17
- Migrated from `.clj` to `.cljc`.
- Replaced the kern parsing lib with Instaparse (latter is `.cljc`-compatible).
- Added support for recursive descent and fixed other features (e.g. array union and slicing).
- Removed `zip.clj` due to overhauling path enumeration algorithm.
- Changed API functions:
  - Take in JSONPath strings instead of already-parsed strings.
  - Move API functions in json-path namespace to pathetic namespace.
  - Renamed functions for consistency purposes.
  - Add optional arguments like `:first?` and `:return-missing?`.
- Increased number of tests:
  - Added tests from Christoph Burgmer's test suite.

## 0.1.2 - 2020-07-08
- Add JSON spec and other DATASIM-related changes.

## 0.1.1 - 2019-12-27
- Added `path-to-spec` function.

## 0.1.0 - 2019-12-13
- Initial (private) release.
