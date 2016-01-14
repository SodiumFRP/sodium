# sodium

A small set of macros to enable use of the Java Sodium FRP library from Clojure.

Must be built using Java 8.0

BUILD WITH Leiningen

lein deps
lein compile

## Usage

	lein test
	lein run -m sodium.memory-test-1
	lein run -m sodium.memory-test-3
	lein run -m sodium.memory-test-4
	lein run -m sodium.memory-test-5
	lein run -m sodium.s-widgets 
	for all s-widger or any combination of the following parameters for individual widgets
	lein run -m soduium.s-widgets form-validation spinme spinner translate redgreen gamechat frp-reverse label

### Any Other Sections
### That You Think
### Might be Useful

Command to add sodium jar to local mvn repository

mvn deploy:deploy-file -DgroupId=local -DartifactId=sodium     -Dversion=1.0.0 -Dpackaging=jar -Dfile=sodium.jar -Durl=file:repo

## License

Copyright © 2016 Francis Hitchens

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
