      * gnucobol manual: https://gnucobol.sourceforge.io/faq/index.htm 
       IDENTIFICATION DIVISION.
         PROGRAM-ID. COBLANG-MAIN.
       environment division.
         input-output section.
           file-control.
      * to read a file normally, this would need to be:
      *       select cobol-init-file assign to ".cobolinit"
             select cobol-init-file
             assign to cobol-init-filename
             organization is sequential
             file status is file-status.
       DATA DIVISION.
         file section.
      * This actually fd needs to match the name in file-control and the record
      * needs to start with the fd.
           fd cobol-init-file.
      * Update `line-size` when changing this such that it can always hold this
      * value.
           01 max-filename-size constant as 64.
           01 cobol-init-file-line PIC X.

         local-STORAGE SECTION.
           01 cobol-init-filename PIC X(max-filename-size)
              value ".cobolinit".
           01 PARAM  PIC 9(2) VALUE 8.
           01 RES    PIC 9(2).

           01 EOF    PIC A(1) VALUE 'N'.
           01 line-actual   PIC X.
           01 line2         PIC X occurs 1 to 2 times
                            depending on line-size.
      * Keep this less than `max-line-size`
           01 line-size     PIC 99.
           01 file-status.
              05 fs1 pic x value "0".
              05 fs2 pic x.

           01 print-ordinal-values-i pic 9(38).

      * See "CBL_GC_HOSTED".
           01 argc usage binary-long.
      * argv is the char** and we have storage to it.
           01 argv usage pointer.
      * arg will represent a pointer to an element from argv, but we don't
      * allocate any storage to it so we need to point it somewhere into argv
      *
      *   set address of arg to argv
      *
      * This will be needed for incrementing through the characters in an
      * argv[i].
           01 arg usage pointer based.
      * arg-buff will effectively be a char pointer for printing. This is
      * needed to convert the char* to a picture we can display.
           01 arg-buff pic x based.

           01 arg1-string.
             copy "cobl-string.cpy".
           01 coblang-lexer.
             copy "lexer.cpy".
           01 token-string.
             copy "cobl-string.cpy".
           01 coblang-parser.
             copy "parser.cpy".

           01 llvm-result usage binary-int.

      * LLVMVerifierFailureAction
           78 LLVMAbortProcessAction value 0.
           78 LLVMPrintMessageAction value 1.
           78 LLVMReturnStatusAction value 2.

       PROCEDURE DIVISION.
         call "CBL_GC_HOSTED" using argc "argc"

         if argc < 2
           display "Expected at least one argument."
           stop run.

         call "CBL_GC_HOSTED" using argv "argv"

      * arg now points to argv[1].
         set argv up by function byte-length(arg)
         set address of arg to argv
         display "arg: " arg
         display "arg length: " function content-length(arg)

      * arg-buff now points to the character at *arg.
         set address of arg-buff to arg

         call "string-construct-from-c-str" using arg1-string arg.
         display "arg1: " no advancing.
         call "string-display" using arg1-string 'Y'.

         call "cobl-memcpy" using address of cobol-init-filename
                                  cobl-string-ptr in arg1-string
                                  cobl-string-length in arg1-string.

         display "reading: " with no advancing
         perform until arg-buff = low-value
           display arg-buff with no advancing
           set arg up by function byte-length(arg-buff)
           set address of arg-buff to arg
         end-perform
         display " ".

         display "arg1-string: " no advancing.
         call "string-display" using arg1-string 'Y'.

         call "lexer-construct" using coblang-lexer
                                      cobl-string-ptr in arg1-string.

         call "parser-construct" using
           coblang-parser
           address of coblang-lexer.
        
         call "parser-parse" using coblang-parser.
         
         call "LLVMVerifyModule"
              using
                by value llvm-module in coblang-parser
                by value LLVMPrintMessageAction
                by value zeros
              returning llvm-result.

         if llvm-result not = zero
           display "Verify module failed."
           call "LLVMDumpModule"
                using by value llvm-module in coblang-parser
           stop run
         end-if.

         call "LLVMDumpModule"
              using by value llvm-module in coblang-parser.

         call "write-obj-file" using coblang-parser.

         call "parser-destroy" using coblang-parser.
         call "lexer-destroy" using coblang-lexer.
         
         call "string-destroy" using token-string.
         call "string-destroy" using arg1-string.

         STOP RUN.

      * From https://www.microfocus.com/documentation/object-cobol/ocu4120/books/fhstat.htm
       check-status.
           evaluate fs1
            when "0"
               perform check-maybe-success
            when "1" display "end of file reached"
               perform check-eof-status
            when "2" display "invalid key"
               perform check-inv-key-status
            when "3" display "permanent error"
               perform check-perm-err-status
            when "4" display "logic error"
            when "9" display file-status ": run-time-system error; "
                     "may be system-dependent"
           end-evaluate.

       check-maybe-success.
           evaluate fs2
             when "9"
               display "File does not exist"
           end-evaluate.
      
       check-eof-status.
           if fs2 = "0"
               display "no next logical record"
           end-if.
       
       check-inv-key-status.
           evaluate fs2
            when "2" display "attempt to write dup key"
            when "3" display "no record found"
           end-evaluate.
       
       check-perm-err-status.
           if fs2 = "5"
               display "file not found"
           end-if.
