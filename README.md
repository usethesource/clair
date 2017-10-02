# CLAIR - C Language Analysis in Rascal

Clair provides a mapping from the Eclipse CDT open C and C++ front-end to a Rascal M3 model for further processing.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.891122.svg)](https://doi.org/10.5281/zenodo.891122)

How to Run
----------

1. If you haven't installed Rascal yet, follow the instructions [here](http://www.rascal-mpl.org/start/) to install Rascal.
2. Add <http://update.rascal-mpl.org/libs> as an update site and `Help->Install New Software` select the `clair` feature
3. Create a new Rascal Project, like `myProject`
4. Right-click the project's folder and select *Rascal Console*, or from the Rascal menu select *Start Console*. The console should open inside Eclipse's terminal window with the title "Rascal [**project: myProject**, mode: debug]". If, instead, it shows "project: none", make sure the project is open and try again.
5. Type in the Racal console the following:  
`import lang::cpp::AST;`
6. To parse the test file included in the project, type:  
`parseCpp(|project://myProject/src/test/test.cpp|)`
7. To parse a file anywhere in your system, make sure you provide the path in the following format:  
`parseCpp(|file:///c:/Path/to/Folder%20Containing%20Spaces/parseMe.cpp|)`
