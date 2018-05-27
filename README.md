# Eff in Anger

Modularity and extensibility are keys when writing software systems. There exist few options when one wants to write modular, extensible, effectful code in Haskell: basically mtl-style typeclasses and free monad derivatives. Extensible effects, aka the Eff monad, is a solution loosely based on the free monad technique using the freer package for fun and profit.

This session is a live-coding, pair-programming introduction to the use of `Eff` to structure an application decoupled in distinct components with strict interfaces. We will interactively develop a simple yet realistic Eff-based Pet Store REST service, demonstrating how to code and test the various effects introduced, how to compose them to produce the desired service, how to leverage the existing standard effects provided by the freer package, and the various ways of writing interpreters and how to handle the sometimes daunting type-checker errors.

## PetStore

This assumes the following software are installed:

* [stack](https://docs.haskellstack.org/en/stable/README/)
* [java 8](http://java/com)
* [maven](http://maven.apache.org/)
* [docker](https://www.docker.com/)

### Build

Builds all the needed software packages, it might take a while initially to download dependencies:

```
./Build.hs build
```

### Run

Run the petstore server and the petstore payment service on ports 9090 and 8080 respectively:

```
./Build.hs run
```
