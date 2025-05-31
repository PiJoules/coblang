       IDENTIFICATION DIVISION.
         PROGRAM-ID. cobl-ctype.
       environment division.
         configuration section.
           special-names.
             class whitespace is space, x"09" through x"0D".
       DATA DIVISION.
         LINKAGE SECTION.
           01 bool-return pic x.
           01 char-arg pic x.

       PROCEDURE DIVISION.
         stop run.

       entry "cobl-isspace" using bool-return char-arg.
         if char-arg is whitespace
           move 'Y' to bool-return
         else
           move 'N' to bool-return
         end-if.
         goback.

       entry "cobl-isdigit" using bool-return char-arg.
         if char-arg is numeric
           move 'Y' to bool-return
         else
           move 'N' to bool-return
         end-if.
         goback.
