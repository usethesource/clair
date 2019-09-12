# ClaiR - C Language Analysis in Rascal

ClaiR provides a mapping from the Eclipse CDT open C and C++ front-end to a Rascal M3 model for further processing.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.891122.svg)](https://doi.org/10.5281/zenodo.891122)

## Required software
* Eclipse 2019-06 with CDT (C/C++ Development Tools) (see [here](https://www.eclipse.org/downloads/packages/release/2019-06/r/eclipse-ide-cc-developers)).
* Rascal (see [here](https://www.rascal-mpl.org/start/)).

## How to install ClaiR into Eclipse
* In the `Help` menu, select `Install New Software`.
* Use <https://update.rascal-mpl.org/libs> as an update site. Hit `Add` to save it for future updates.
* Select `clair_feature` in the list of libraries and follow the instructions.

## How to run
* In Eclipse's Project Explorer, right click a project's folder and select *Rascal Console*. The console should open inside Eclipse's terminal window with the title "Rascal [**project: <Project Name>**, mode: debug]". If, instead, it shows "project: none", make sure you clicked the project and the project is actually open.
* Import ClaiR into the console by running `import lang::cpp::AST;`.
* To parse a string containing source code, call the `parseString` function, e.g., `parseString("int foo() { return 0; }")`.
* To parse source files on disk, call the `parseCpp` function with a source location, e.g., `parseCpp(|file:///tmp/example.cpp|)`, `parseCpp(|project://clair/src/test/test.cpp|)`, `parseCpp(|home://myFile.cpp|)`, or `parseCpp(|file://C:/My%20Dir/main.cpp|)`.
