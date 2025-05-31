       IDENTIFICATION DIVISION.
         PROGRAM-ID. lexer.
       environment division.
         input-output section.
           file-control.
             select lexer-file
             assign to filename-buffer
             organization is sequential
             file status is file-status.
       DATA DIVISION.
         file section.
           fd lexer-file.
             01 lexer-file-char pic x.

         working-storage section.

         local-storage section.
           01 file-status.
              05 fs1 pic x value "0".
              05 fs2 pic x.

           01 read-char pic x.

           01 filename-buffer pic x(1000).
      * NOTE: This must match the type of pic-length in cobl-string.
           01 filename-buffer-size usage index.

           01 isspace pic x.
           01 peek-char pic x.
           01 get-char pic x.
           01 starting-quote-char pic x.

           01 tmp-ptr usage pointer.

         LINKAGE SECTION.
           01 local-lexer.
              copy "lexer.cpy".
           01 token-string.
              copy "cobl-string.cpy".
           01 local-string.
              copy "cobl-string.cpy".
           01 line-ret usage binary-c-long unsigned.
           01 col-ret usage binary-c-long unsigned.

       PROCEDURE DIVISION.
         stop run.

       entry "lexer-construct" using local-lexer
                                     local-string.
         set filename-buffer-size to length of filename-buffer.
         set tmp-ptr to address of filename-buffer.
         call "string-copy-to-pic" using
            local-string
            tmp-ptr
            filename-buffer-size.

         move 1 to lexer-line in local-lexer.
         move 0 to lexer-col in local-lexer.

         open input lexer-file.
         perform check-status.
         goback.

       entry "lexer-destroy" using local-lexer.
         close lexer-file.
         goback.

      *
      * Sets peek-char.
      *
       do-peek-char.
         if lexer-has-lookahead in local-lexer = 'N'
           read lexer-file into lexer-lookahead in local-lexer
             at end
               set lexer-at-eof in local-lexer to true
               exit paragraph
           end-read

           if lexer-lookahead in local-lexer = x"0A"
             set lexer-line in local-lexer up by 1
             move 0 to lexer-col in local-lexer
           else
             set lexer-col in local-lexer up by 1
           end-if
           set lexer-does-have-lookahead in local-lexer to true
         end-if.

         move lexer-lookahead in local-lexer to peek-char.
       end-do-peek-char.

      *
      * Sets get-char
      *
       do-get-char.
         if lexer-has-lookahead in local-lexer = 'Y'
           move lexer-lookahead in local-lexer to get-char
           set lexer-does-not-have-lookahead in local-lexer to true
           exit paragraph
         end-if.

         read lexer-file into get-char
           at end
             set lexer-at-eof in local-lexer to true
         end-read.

         if get-char = x"0A"
           set lexer-line in local-lexer up by 1
           move 0 to lexer-col in local-lexer
         else
           set lexer-col in local-lexer up by 1
         end-if.
       end-do-get-char.

      *
      * Skip whitespace.
      *
       skip-whitespace-and-comments.
         perform forever
           perform do-peek-char
           if lexer-eof in local-lexer = 'Y'
             exit paragraph
           end-if

           if peek-char = "*"
      * Read until newline (0x0A).
             perform do-get-char
             perform until get-char = x"0A"
               perform do-get-char
             end-perform
             exit perform cycle
           end-if

           call "cobl-isspace" using isspace peek-char
           if isspace = 'N'
             exit paragraph
           end-if

           perform do-get-char
         end-perform.
       end-skip-whitespace-and-comments.

      * Lex one string into `token-string`.
       entry "lexer-lex" using local-lexer token-string line-ret
             col-ret.
         perform check-status.

         if lexer-eof in local-lexer = 'Y'
           goback.

         perform forever
           perform skip-whitespace-and-comments
           if lexer-eof in local-lexer = 'Y'
             goback
           end-if

           perform do-get-char
           move get-char to read-char
        
           call "cobl-isspace" using isspace read-char
           if isspace = 'Y' or lexer-eof in local-lexer = 'Y'
             display "ERROR: read-char should not be whitespace or EOF"
             stop run
           end-if

           call "string-push-back" using token-string read-char

           exit perform
         end-perform.

         move lexer-line in local-lexer to line-ret.
         move lexer-col in local-lexer to col-ret.

      * Parse a period.
         if read-char = "." or
            read-char = "(" or
            read-char = ")"
           goback.

      * Parse a string literal.
         if read-char = '"'  or read-char = "'"
           perform handle-string-literal
           goback
         end-if.

      * See if this is a hexadecimal string literal.
         if read-char = 'x'
           perform do-peek-char
           if peek-char = '"' or peek-char = "'"
             perform do-get-char
             call "string-push-back" using token-string get-char
             move get-char to read-char
             perform handle-string-literal
             goback
           end-if
         end-if.

      * Parse an identifier.
         perform forever
           perform do-peek-char
           evaluate peek-char
             when "A" through "Z"
             when "a" through "z"
             when is numeric
             when =  "-"
               perform do-get-char
               call "string-push-back" using token-string get-char
             when other
               exit perform
           end-evaluate
         end-perform.

         goback.

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
           if fs1 not = 0
             display file-status
             stop run
           end-if.

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
           evaluate fs2
             when "1"
               display "INCONSISTANT_FILENAME"
             when "4"
               display "BOUNDARY_VIOLATION"
             when "5"
               display "file not found"
             when "7"
               display "PERMISSION_DENIES"
             when "8"
               display "CLOSED_WITH_LOCK"
             when "9"
               display "CONFLICT_ATTRIBUTE"
           end-evaluate.

       handle-string-literal.
         move read-char to starting-quote-char.
         perform forever
           perform do-get-char
           call "string-push-back" using token-string get-char
           if get-char = starting-quote-char
             exit perform
           end-if
         end-perform.
