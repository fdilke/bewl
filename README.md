Bewl is my long-running topos theory DSL, written in Java, then Clojure, then Scala.

the README for Bewl 1 is [here](https://github.com/fdilke/bewl/blob/master/bewl1/README.md).
note to run Bewl1, because it uses Scala 2.x, you'll need to use Java 17, using such commands as:

```
nix-shell -p jdk17 sbt
nix shell nixpkgs#jdk17 nixpkgs#sbt
export JAVA_HOME=$(dirname $(dirname $(which java)))
sbt -java-home $JAVA_HOME test
```

Proper documentation for Bewl 2 has yet to be added, even as Bewl 3 is under construction

