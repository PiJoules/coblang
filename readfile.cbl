       identification division.
         program-id. readfile.
       data division.
         linkage section.
           01 size    pic(100).
           01 output  pic(size).
       procedure division using output, input.
