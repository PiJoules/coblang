## 1. Unexpected tree tag: 0 on copy

We explicitly copy the members in some places instead of
using the copy because gnucobol crashes with the following error
when compiling this file and using the copy:

 ```
 cobc: unexpected tree tag: 0
 cobc: codegen.c: 1217: internal compiler error
 
 cobc: aborting codegen for /home/brocklee/projects/cobol2/codegen.cbl (PROGRAM-ID: codegen)
 cobc: Please report this!
 ```

Until it is fixed, we will need to manually copy the fields here.
