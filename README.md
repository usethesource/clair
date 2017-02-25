# CLAIR - C Language Analysis in Rascal

Clair provides a mapping from the Eclipse CDT open C and C++ front-end to a Rascal M3 model for further processing.

How to Run
----------
If you haven't installed Rascal yet, follow the instructions [here](http://www.rascal-mpl.org/start/) to install Rascal.  
After having imported this project into Eclipse, right-click the project's folder and select *Rascal Console*, or from the Rascal menu select *Start Console*. The console should open inside Eclipse's terminal window with the title "Rascal [**project: clair**, mode: debug]". If, instead, it shows "project: none", make sure the project is open and try again.

Type in the Racal console the following:  
`import lang::cpp::AST;`

To parse the test file included in the project, type:  
`parseCpp(|project://clair/src/test/test.cpp|)`

To parse a file anywhere in your system, make sure you provide the path in the following format:  
`parseCpp(|file:///c:/Path/to/Folder%20Containing%20Spaces/parseMe.cpp|)`
