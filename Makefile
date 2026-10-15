.PHONY: test-clj test-cljs

test-clj:
	clojure -M:test:runner-clj

test-cljs:
	clojure -M:test:runner-cljs

.PHONY: pom.xml

pom.xml:
	clojure -X:deps mvn-pom :user nil

