       IDENTIFICATION DIVISION.
         PROGRAM-ID. codegen.
       DATA DIVISION.
         working-storage section.
      * FIXME: This acts as static storage but should probably exist once
      * per codegen.
           01 LLVMContext usage pointer value null.
           01 LLVMInt8Type usage pointer value null.
           01 LLVMInt32Type usage pointer value null.
           01 LLVMPtrType usage pointer value null.
           01 LLVMVoidType usage pointer value null.
           01 LLVMInt32ZeroValue usage pointer value null.
           01 LLVMNewlineGlobalString usage pointer value null.
           01 LLVMStringFormatSpecifier usage pointer value null.

         local-storage section.
           01 token-string.
             copy "cobl-string.cpy".
           01 parsed-identifier.
             copy "cobl-string.cpy".
           01 parsed-integer.
             copy "cobl-string.cpy".

      * A parsed pic type is a pair of vectors. The first vector contains
      * the actual symbols. The second vector contains the number of each
      * symbol.
           01 parsed-pic-type.
              02 symbols.
                 copy "cobl-vector.cpy".
              02 sizes.
                 copy "cobl-vector.cpy".
           01 parsed-pic-value.
              copy "cobl-string.cpy".

           01 llvm-error usage pointer.
           01 llvm-triple usage pointer.
           01 llvm-target usage pointer.
           01 llvm-result usage binary-int.
           01 llvm-cpu usage pointer.
           01 llvm-features usage pointer.
           01 llvm-data-layout usage pointer.
           01 llvm-type-res usage pointer.
           01 llvm-value-res usage pointer.
           01 llvm-value-res2 usage pointer.

      * LLVMCodeGenFileType
           78 LLVMAssemblyFile value 0.
           78 LLVMObjectFile value 1.

      * LLVMCodeGenOptLevel
           78 LLVMCodeGenLevelNone value 0.
           78 LLVMCodeGenLevelLess value 1.
           78 LLVMCodeGenLevelDefault value 2.
           78 LLVMCodeGenLevelAggressive value 3.

      * LLVMRelocMode
           78 LLVMRelocDefault value 0.
           78 LLVMRelocStatic value 1.
           78 LLVMRelocPIC value 2.
           78 LLVMRelocDynamicNoPic value 3.
           78 LLVMRelocROPI value 4.
           78 LLVMRelocRWPI value 5.
           78 LLVMRelocROPI_RWPI value 6.

      * LLVMCodeModel
           78 LLVMCodeModelDefault value 0.
           78 LLVMCodeModelJITDefault value 1.
           78 LLVMCodeModelTiny value 2.
           78 LLVMCodeModelSmall value 3.
           78 LLVMCodeModelKernel value 4.
           78 LLVMCodeModelMedium value 5.
           78 LLVMCodeModelLarge value 6.

      * LLVMVerifierFailureAction
           78 LLVMAbortProcessAction value 0.
           78 LLVMPrintMessageAction value 1.
           78 LLVMReturnStatusAction value 2.

           01 func-type-params.
              copy "cobl-vector.cpy".

           01 tmp-vector.
              copy "cobl-vector.cpy".

           01 pointer-size usage binary-c-long unsigned
              value length of pointer.
      * FIXME: This isn't quite right, but AFAICT there's no concept
      * of alignment with cobol data types.
           01 pointer-align usage binary-c-long unsigned
              value length of pointer.
           01 char-size usage binary-c-long unsigned
              value length of binary-char.
           01 char-align usage binary-c-long unsigned
              value length of binary-char.
           01 unsigned-long-size usage binary-c-long unsigned
              value length of binary-c-long.
           01 unsigned-long-align usage binary-c-long unsigned
              value length of binary-c-long.

           01 tmp-ptr usage pointer.
      * This can be used as a buffer we can move pointers into. For example,
      * we might store a pointer in `tmp-ptr` from a call to LLVM and now we
      * want to store something at the address specified by `tmp-ptr`. We
      * can do that by setting the address of `tmp-ptr-storage` to the value
      * in `tmp-ptr` then MOVEing to `tmp-ptr-storage`.
           01 tmp-ptr-storage usage pointer based.
           01 tmp-char pic x.
           01 tmp-unsigned-long usage binary-c-long unsigned.
           01 iter usage binary-c-long unsigned.
           01 tmp-char-storage pic x based.
           01 tmp-unsigned-long-storage usage binary-c-long unsigned
              based.
           01 last-parsed-pic-symbol pic x.

           01 bb-entry-ptr usage pointer.
           01 builder-ptr usage pointer.
           01 func-ptr usage pointer.
           01 main-func-ptr usage pointer.
           01 printf-func-ptr usage pointer.
           01 printf-func-type-ptr usage pointer.
           01 exit-func-ptr usage pointer.
           01 exit-func-type-ptr usage pointer.
           01 compare-result usage binary-int.
           01 pic-buffer pic x(1024).
           78 pic-buffer-size value length of pic-buffer.

           01 this-codegen-lexer based.
              copy "lexer.cpy".

         LINKAGE SECTION.
           01 this-codegen.
             copy "codegen.cpy".

           01 lexer-ptr-arg usage pointer.
           01 output-filename usage pointer.

       PROCEDURE DIVISION.
         stop run.

       entry "codegen-construct" using this-codegen lexer-ptr-arg.
         move lexer-ptr-arg to lexer-ptr in this-codegen.

         call "LLVMModuleCreateWithName"
              using "this-module"
              returning llvm-module in this-codegen.

         call "LLVMCreateDIBuilder"
              using by value llvm-module in this-codegen
              returning llvm-dibuilder in this-codegen.

         call "LLVMInitializeX86TargetInfo".
         call "LLVMInitializeX86Target".
         call "LLVMInitializeX86TargetMC".
         call "LLVMInitializeX86AsmParser".
         call "LLVMInitializeX86AsmPrinter".

         call "LLVMGetDefaultTargetTriple"
              returning llvm-triple.

         call "LLVMGetTargetFromTriple"
              using
                by value llvm-triple
                by reference llvm-target
                by reference llvm-error
              returning llvm-result.

         if llvm-result not = ZERO then
           display "llvm error:"
           call "print-c-string" using llvm-error
           call "LLVMDisposeMessage" using llvm-error
           call "LLVMDisposeMessage" using llvm-triple
         end-if.

         call "LLVMGetHostCPUName" returning llvm-cpu.
         call "LLVMGetHostCPUFeatures" returning llvm-features.

         call "LLVMCreateTargetMachine"
              using
                by value llvm-target
                by value llvm-triple
                by value llvm-cpu
                by value llvm-features
                by value LLVMCodeGenLevelNone
                by value LLVMRelocPIC
                by value LLVMCodeModelDefault
              returning llvm-target-machine in this-codegen.

         call "LLVMDisposeMessage" using by value llvm-triple.
         call "LLVMDisposeMessage" using by value llvm-cpu.
         call "LLVMDisposeMessage" using by value llvm-features.

         call "LLVMCreateTargetDataLayout"
              using by value llvm-target-machine in this-codegen
              returning llvm-data-layout.
         call "LLVMSetModuleDataLayout"
              using
                by value llvm-module in this-codegen
                by value llvm-data-layout.

         call "LLVMGetModuleContext"
              using by value llvm-module in this-codegen
              returning LLVMContext.
         call "LLVMIntType" using by value 32 returning LLVMInt32Type.
         call "LLVMIntType" using by value 8 returning LLVMInt8Type.
         call "LLVMPointerTypeInContext"
              using
                by value LLVMContext
                by value 0
              returning LLVMPtrType.
         call "LLVMVoidType" returning LLVMVoidType.
         call "LLVMConstNull" using
              by value LLVMInt32Type
              returning LLVMInt32ZeroValue.

         goback.

       entry "codegen-destroy" using this-codegen.
         call "LLVMDisposeDIBuilder"
              using by value llvm-dibuilder in this-codegen.

         call "LLVMDisposeModule"
              using by value llvm-module in this-codegen.
         goback.

      *
      * This does the actual codegen.
      *
       entry "codegen-run" using this-codegen.
         perform insert-main-func.

         call "LLVMAppendBasicBlock" using
              by value main-func-ptr
              by content function concatenate("entry", x"00")
              returning bb-entry-ptr.

         call "LLVMCreateBuilder" returning builder-ptr.
         call "LLVMPositionBuilderAtEnd" using
              by value builder-ptr
              by value bb-entry-ptr.

         set address of this-codegen-lexer to lexer-ptr in this-codegen.

         call "string-construct" using token-string.
         call "string-construct" using parsed-identifier.
         call "string-construct" using parsed-integer.
         call "string-construct" using parsed-pic-value.
         call "vector-construct" using
              symbols in parsed-pic-type
              char-size char-align.
         call "vector-construct" using
              sizes in parsed-pic-type
              unsigned-long-size unsigned-long-align.

         perform until lexer-eof in this-codegen-lexer = 'Y'
           perform get-token-string-and-buffer

           if lexer-eof in this-codegen-lexer = 'Y'
             exit perform
           end-if

      *     display "'" no advancing
      *     call "string-display" using token-string 'N'
      *     display "'"

           evaluate pic-buffer
             when "DISPLAY"
               perform handle-display
             when "EXIT"
               perform handle-exit
             when "DATA"
               perform handle-data-division
           end-evaluate
         end-perform.

         call "vector-destroy" using sizes in parsed-pic-type.
         call "vector-destroy" using symbols in parsed-pic-type.
         call "string-destroy" using token-string.
         call "string-destroy" using parsed-identifier.
         call "string-destroy" using parsed-integer.
         call "string-destroy" using parsed-pic-value.

      * End of the function.
         call "LLVMBuildRet" using
              by value builder-ptr
              by value LLVMInt32ZeroValue.

         call "LLVMDisposeBuilder" using by value builder-ptr.

         call "LLVMVerifyFunction" using
              by value main-func-ptr
              by value LLVMPrintMessageAction
              returning llvm-result.

         if llvm-result not = zero
           display "Verify function failed"
           call "LLVMDumpValue" using by value main-func-ptr
           stop run
         end-if.

         goback.

      *
      * Write the module to an object file.
      *
       entry "write-obj-file" using this-codegen output-filename.
         call "LLVMVerifyModule"
              using
                by value llvm-module in this-codegen
                by value LLVMPrintMessageAction
                by value zeros
              returning llvm-result.

         if llvm-result not = zero
           display "Verify module failed."
           call "LLVMDumpModule"
                using by value llvm-module in this-codegen
           stop run
         end-if.

         call "LLVMDumpModule"
              using by value llvm-module in this-codegen.

         call "LLVMTargetMachineEmitToFile" using
              by value llvm-target-machine in this-codegen
              by value llvm-module in this-codegen
              by value output-filename
              by value LLVMObjectFile
              by reference llvm-error
              returning llvm-result.

         if llvm-result not = zero
           display "llvm error:"
           call "print-c-string" using llvm-error
           call "LLVMDisposeMessage" using llvm-error
           stop run
         end-if.
         goback.

      *
      * These both set `token-string` and `pic-buffer`.
      *
       get-token-string-and-buffer.
         if has-lookahead in this-codegen = 'Y'
           move 'N' to has-lookahead in this-codegen
         else
           call "string-clear" using token-string
           call "lexer-lex" using this-codegen-lexer token-string
           set tmp-ptr to address of pic-buffer
           call "string-copy-to-pic" using
                token-string
                tmp-ptr
                pic-buffer-size
         end-if.
       end-get-token-string-and-buffer.

       peek-token-string-and-buffer.
         if has-lookahead in this-codegen = 'N'
           call "string-clear" using token-string
           call "lexer-lex" using this-codegen-lexer token-string
           set tmp-ptr to address of pic-buffer
           call "string-copy-to-pic" using
                token-string
                tmp-ptr
                pic-buffer-size
           move 'Y' to has-lookahead in this-codegen
         end-if.
       end-peek-token-string-and-buffer.

       handle-data-division.
         perform get-token-string-and-buffer.
         if pic-buffer not = "DIVISION"
           display "error: Expected DATA DIVISION"
           stop run.

         perform pop-period.
         perform peek-token-string-and-buffer.

         if pic-buffer = "WORKING-STORAGE"
           perform get-token-string-and-buffer
           perform pop-section
           perform pop-period
           perform parse-variables
         end-if.
       end-handle-data-division.

       parse-variables.
         perform parse-level.
         perform parse-identifier.
         perform pop-pic.
         perform parse-pic-type.
         perform pop-period.

         perform get-parsed-pic-size.
      * Increment by 1 because we always append the null terminator.
         set tmp-unsigned-long up by 1.
         call "LLVMArrayType" using
              by value LLVMInt8Type
              by value tmp-unsigned-long
              returning tmp-ptr.
         
         call "LLVMAddGlobal" using
              by value llvm-module in this-codegen
              by value tmp-ptr
              by value cobl-string-ptr in parsed-identifier
              returning llvm-value-res.

         perform get-parsed-pic-size.
         call "string-resize" using parsed-pic-value
              tmp-unsigned-long ' '.

         call "LLVMConstStringInContext" using
              by value LLVMContext
              by value cobl-string-ptr in parsed-pic-value
              by value cobl-string-length in parsed-pic-value
              by value 0
              returning llvm-value-res2.

         call "LLVMSetInitializer" using
              by value llvm-value-res
              by value llvm-value-res2.
       end-parse-variables.

      * Get the total number of characters for the last parsed-pic-type
      * and store it in `tmp-unsigned-long`.
       get-parsed-pic-size.
         move 0 to tmp-unsigned-long.
         move 0 to iter.
         perform until iter >= vector-size in sizes in parsed-pic-type
           call "vector-at" using sizes in parsed-pic-type iter tmp-ptr
           set address of tmp-unsigned-long-storage to tmp-ptr
           set tmp-unsigned-long up by tmp-unsigned-long-storage
           set iter up by 1
         end-perform.
       end-get-parsed-pic-size.

       parse-pic-type.
         perform get-token-string-and-buffer.

         call "vector-clear" using symbols in parsed-pic-type.
         call "vector-clear" using sizes in parsed-pic-type.

         if pic-buffer = "X"
           call "vector-append-storage" using
                symbols in parsed-pic-type tmp-ptr
           set address of tmp-char-storage to tmp-ptr
           move "X" to tmp-char-storage
         else
           display "error: Unhandled pic symbol '" no advancing
           call "string-display" using token-string 'N'
           display "'"
           stop run
         end-if.

         move pic-buffer(1:1) to last-parsed-pic-symbol.

         call "vector-append-storage" using
              sizes in parsed-pic-type tmp-ptr.
         set address of tmp-unsigned-long-storage to tmp-ptr.
         move 1 to tmp-unsigned-long-storage.

         perform forever
           perform peek-token-string-and-buffer
           evaluate pic-buffer
             when "("
               perform pop-lpar
               perform parse-integer

               call "vector-back" using
                    sizes in parsed-pic-type tmp-ptr
               set address of tmp-unsigned-long-storage to tmp-ptr
               move function numval(pic-buffer) to
                    tmp-unsigned-long-storage

               perform pop-rpar
             when "X"
               if last-parsed-pic-symbol = "X"
                 call "vector-back" using
                      sizes in parsed-pic-type tmp-ptr
                 set address of tmp-unsigned-long-storage to tmp-ptr
                 set tmp-unsigned-long-storage up by 1
               else
                 call "vector-append-storage" using
                      symbols in parsed-pic-type tmp-ptr
                 set address of tmp-char-storage to tmp-ptr
                 move "X" to tmp-char-storage

                 call "vector-append-storage" using
                      sizes in parsed-pic-type tmp-ptr
                 set address of tmp-unsigned-long-storage to tmp-ptr
                 move 1 to tmp-unsigned-long-storage
               end-if

               move "X" to last-parsed-pic-symbol
               perform get-token-string-and-buffer
             when other
               exit perform
           end-evaluate
         end-perform.

      * Parse an optional `value` clause.
         perform peek-token-string-and-buffer.
         if pic-buffer = "VALUE"
           perform get-token-string-and-buffer
           perform get-token-string-and-buffer
      * Strip the quotes off a string.
           if pic-buffer(1:1) = "'" or pic-buffer(1:1) = '"'
             move 0 to tmp-unsigned-long
             call "string-erase" using token-string tmp-unsigned-long
             compute tmp-unsigned-long =
               cobl-string-length in token-string - 1
             call "string-erase" using token-string tmp-unsigned-long
           end-if
           call "string-copy" using parsed-pic-value token-string
         end-if.
         
       end-parse-pic-type.

      * Gets a token and copies it into `parsed-integer`.
       parse-integer.
         perform get-token-string-and-buffer.
         if function trim(pic-buffer TRAILING) is not numeric
           display "error: Expected integer but found '" no advancing
           call "string-display" using token-string 'N'
           display "'"
           stop run.
         call "string-copy" using parsed-integer token-string.
       end-parse-integer.

      * Gets a token and copies it into `parsed-identifier`.
       parse-identifier.
         perform get-token-string-and-buffer.
         call "string-copy" using parsed-identifier token-string.
       end-parse-identifier.

       parse-level.
         perform get-token-string-and-buffer.
         if function trim(pic-buffer TRAILING) is not numeric
           display "error: Level number is not numeric '" no advancing
           call "string-display" using token-string 'N'
           display "'"
           stop run.
       end-parse-level.

       pop-pic.
         perform get-token-string-and-buffer.
         if pic-buffer not = "PIC"
           display "error: Expected PIC"
           stop run.
       end-pop-pic.

       pop-lpar.
         perform get-token-string-and-buffer.
         if pic-buffer not = "("
           display "error: Expected ("
           stop run.
       end-pop-lpar.

       pop-rpar.
         perform get-token-string-and-buffer.
         if pic-buffer not = ")"
           display "error: Expected )"
           stop run.
       end-pop-rpar.

       pop-period.
         perform get-token-string-and-buffer.
         if pic-buffer not = "."
           display "error: Expected period"
           stop run.
       end-pop-period.

       pop-section.
         perform get-token-string-and-buffer.
         if pic-buffer not = "SECTION"
           display "error: Expected SECTION but found '" no advancing
           call "string-display" using token-string 'N'
           display "'"
           stop run.
       end-pop-section.

      * We parsed an EXIT token.
       handle-exit.
         perform get-token-string-and-buffer.
         if pic-buffer not = "PROGRAM"
           display "error: Expected STOP PROGRAM"
           stop run
         end-if.

      * exit(0)
         perform get-exit-func.

         call "LLVMBuildCall2" using
              by value builder-ptr
              by value exit-func-type-ptr
              by value exit-func-ptr
              by value address of LLVMInt32ZeroValue
              by value 1
              by content x"00".
       end-handle-exit.

       get-exit-func.
         call "LLVMGetNamedFunction" using
              by value llvm-module in this-codegen
              by content function concatenate("exit", x"00")
              returning exit-func-ptr.

         if exit-func-ptr not = null
           exit paragraph.

         call "vector-construct" using
              func-type-params pointer-size pointer-align.

      * int exit_code
         call "vector-append-storage" using
              func-type-params tmp-ptr.
         set address of tmp-ptr-storage to tmp-ptr.
         move LLVMInt32Type to tmp-ptr-storage.

         call "LLVMFunctionType"
              using
                by value LLVMVoidType
                by value vector-data in func-type-params
                by value 1
                by value 0
              returning exit-func-type-ptr.

         call "vector-destroy" using func-type-params.

         call "LLVMAddFunction" using
              by value llvm-module in this-codegen
              by content function concatenate("exit", x"00")
              by value exit-func-type-ptr
              returning exit-func-ptr.
       end-get-exit-func.

      * We parsed and popped a DISPLAY token. Spin up a printf.
       handle-display.
         perform get-token-string-and-buffer.

         move 0 to tmp-unsigned-long.
         call "string-at" using token-string tmp-unsigned-long tmp-char.
         if tmp-char = '"' or tmp-char = "'"
           call "string-erase" using token-string tmp-unsigned-long
           compute tmp-unsigned-long =
             cobl-string-length in token-string - 1
           call "string-erase" using token-string tmp-unsigned-long

      * Build the global string.
           call "LLVMBuildGlobalStringPtr" using
                by value builder-ptr
                by value cobl-string-ptr in token-string
                by content x"00"
                returning llvm-value-res
         
           perform get-printf-func
           call "LLVMBuildCall2" using
                by value builder-ptr
                by value printf-func-type-ptr
                by value printf-func-ptr
                by value address of llvm-value-res
                by value 1
                by content x"00"
         else
      * This must be an identifier.
      * TODO: The format specifier should change depending on the type.
           call "LLVMGetNamedGlobal" using
                by value llvm-module in this-codegen
                by value cobl-string-ptr in token-string
                returning llvm-value-res

           if llvm-value-res = null
             display "error: Could not find global '" no advancing
             call "string-display" using token-string 'N'
             display "'"
             stop run
           end-if

           perform get-string-format-specifier
           call "vector-construct" using
                tmp-vector pointer-size pointer-align

           call "vector-append-storage" using tmp-vector tmp-ptr
           set address of tmp-ptr-storage to tmp-ptr
           move LLVMStringFormatSpecifier to tmp-ptr-storage

           call "vector-append-storage" using tmp-vector tmp-ptr
           set address of tmp-ptr-storage to tmp-ptr
           move llvm-value-res to tmp-ptr-storage
           
           perform get-printf-func
           call "LLVMBuildCall2" using
                by value builder-ptr
                by value printf-func-type-ptr
                by value printf-func-ptr
                by value vector-data in tmp-vector
                by value 2
                by content x"00"

           call "vector-destroy" using tmp-vector
         end-if.

         perform emit-print-newline.
       end-handle-display.

       get-newline-global-string.
         call "LLVMBuildGlobalStringPtr" using
              by value builder-ptr
              by content function concatenate(x"0A", x"00")
              by content x"00"
              returning LLVMNewlineGlobalString.
       end-get-newline-global-string.

       get-string-format-specifier.
         call "LLVMBuildGlobalStringPtr" using
              by value builder-ptr
              by content function concatenate("%s", x"00")
              by content x"00"
              returning LLVMStringFormatSpecifier.
       end-string-format-specifier.

       emit-print-newline.
         perform get-printf-func.
         perform get-newline-global-string.
         call "LLVMBuildCall2" using
              by value builder-ptr
              by value printf-func-type-ptr
              by value printf-func-ptr
              by value address of LLVMNewlineGlobalString
              by value 1
              by content x"00".
       end-emit-print-newline.

       get-printf-func.
         call "LLVMGetNamedFunction" using
              by value llvm-module in this-codegen
              by content function concatenate("printf", x"00")
              returning printf-func-ptr.

         if printf-func-ptr not = null
           exit paragraph.

         call "vector-construct" using
              func-type-params pointer-size pointer-align.

      * const char *format
         call "vector-append-storage" using
              func-type-params tmp-ptr.
         set address of tmp-ptr-storage to tmp-ptr.
         move LLVMPtrType to tmp-ptr-storage.

         call "LLVMFunctionType"
              using
                by value LLVMInt32Type
                by value vector-data in func-type-params
                by value 1
                by value 1
              returning printf-func-type-ptr.

         call "vector-destroy" using func-type-params.

         call "LLVMAddFunction" using
              by value llvm-module in this-codegen
              by content function concatenate("printf", x"00")
              by value printf-func-type-ptr
              returning printf-func-ptr.
       end-get-printf-func.

       insert-main-func.
         call "LLVMGetNamedFunction" using
              by value llvm-module in this-codegen
              by content function concatenate("main", x"00")
              returning main-func-ptr.

         if main-func-ptr not = null
           exit paragraph.

         call "vector-construct" using
              func-type-params pointer-size pointer-align.

      * int argc
         call "vector-append-storage" using
              func-type-params tmp-ptr.
         set address of tmp-ptr-storage to tmp-ptr.
         move LLVMInt32Type to tmp-ptr-storage.
      * char **argv
         call "vector-append-storage" using
              func-type-params tmp-ptr.
         set address of tmp-ptr-storage to tmp-ptr.
         move LLVMPtrType to tmp-ptr-storage.

         call "LLVMFunctionType"
              using
                by value LLVMInt32Type
                by value vector-data in func-type-params
                by value 2
                by value 0
              returning llvm-type-res.

         call "vector-destroy" using func-type-params.

      * `main()`
         call "LLVMAddFunction" using
              by value llvm-module in this-codegen
      * This is needed to create a null-terminated string.
              by content function concatenate("main", x"00")
              by value llvm-type-res
              returning main-func-ptr.
       end-insert-main-func.
