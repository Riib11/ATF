# Abstract Transpilation Framework

ATF implements an abstractly-defined transpilation flow. ATF takes as input a source text written in ATF-src syntax and a transpilation definition written in ATF-lang syntax, and writes as output the transpiled source.

![ATF logo](ATF-logo.png)



## Wiki

[The ATF Wiki](https://github.com/Riib11/ATF/wiki)



## Prerequisites

| Description              | Link                          |
|--------------------------|-------------------------------|
| Glasgow Haskell Compiler | https://www.haskell.org/ghc/  |
| Git Command Line Tools   | https://git-scm.com/downloads |



## Installation

### MacOS

To install ATF, change to the directory where you want to install the ATF repository and then execute the following:

```bash
git clone https://github.com/Riib11/ATF.git # clone repository to this directory
./ATF/install                               # run install script
```

*Updating*. To update a previous installation of ATF, change to inside your local clone of the ATF repository and then execute the following:

```bash
git fetch
git pull
```

### Windows

_Unimplemented_

## Documentation

ATF uses the literate haskell style for documentation. Each haskell module (`.lhs` file) has an associated pdf file for documentation, in a pleasantly readable format.



## In Progress

#### Translator to implement:
* markdown -> html
