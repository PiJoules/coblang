       IDENTIFICATION DIVISION.
         PROGRAM-ID. codegen.
       environment division.
       DATA DIVISION.
         working-storage section.
      * FIXME: This acts as static storage but should probably exist once
      * per codegen.
           01 LLVMContext usage pointer.
           01 LLVMInt32Type usage pointer.
           01 LLVMPtrType usage pointer.
           01 LLVMVoidType usage pointer.
           01 LLVMInt32ZeroValue usage pointer.

         local-storage section.
           01 token-string.
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

           01 pointer-size usage binary-c-long unsigned
              value length of pointer.
      * FIXME: This isn't quite right, but AFAICT there's no concept
      * of alignment with cobol data types.
           01 pointer-align usage binary-c-long unsigned
              value length of pointer.

           01 tmp-ptr usage pointer.
      * This can be used as a buffer we can move pointers into. For example,
      * we might store a pointer in `tmp-ptr` from a call to LLVM and now we
      * want to store something at the address specified by `tmp-ptr`. We
      * can do that by setting the address of `tmp-ptr-storage` to the value
      * in `tmp-ptr` then MOVEing to `tmp-ptr-storage`.
           01 tmp-ptr-storage usage pointer based.
           01 tmp-char pic x.
           01 tmp-unsigned-long usage binary-c-long unsigned.

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

         perform until lexer-eof in this-codegen-lexer = 'Y'
           call "string-construct" using token-string
           call "lexer-lex" using this-codegen-lexer token-string

           if lexer-eof in this-codegen-lexer = 'Y'
             exit perform
           end-if

      *     display "'" no advancing
      *     call "string-display" using token-string 'N'
      *     display "'"

           call "string-copy-to-pic" using
                token-string
                address of pic-buffer
                pic-buffer-size

           evaluate pic-buffer
             when "DISPLAY"
               perform handle-display
             when "EXIT"
               perform handle-exit
           end-evaluate

           call "string-destroy" using token-string
         end-perform.

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
       entry "write-obj-file" using this-codegen.
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
              by content function concatenate("out.obj", x"00")
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

      * We parsed an EXIT token.
       handle-exit.
         call "string-clear" using token-string.
         call "lexer-lex" using this-codegen-lexer token-string.

         call "string-copy-to-pic" using
              token-string
              address of pic-buffer
              pic-buffer-size.

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
         call "string-clear" using token-string.
         call "lexer-lex" using this-codegen-lexer token-string.

         move 0 to tmp-unsigned-long.
         call "string-at" using token-string tmp-unsigned-long tmp-char.
         if tmp-char not = '"' and tmp-char not = "'"
           display "error: Expected string literal"
           stop run
         end-if.

         move 0 to tmp-unsigned-long.
         call "string-erase" using token-string tmp-unsigned-long.
         compute tmp-unsigned-long =
           cobl-string-length in token-string - 1.
         call "string-erase" using token-string tmp-unsigned-long.
      * Append a newline.
         call "string-push-back" using token-string x"0A".

      * Build the global string.
         call "LLVMBuildGlobalStringPtr" using
              by value builder-ptr
              by value cobl-string-ptr in token-string
              by content x"00"
              returning llvm-value-res.

         perform get-printf-func.
         
         call "LLVMBuildCall2" using
              by value builder-ptr
              by value printf-func-type-ptr
              by value printf-func-ptr
              by value address of llvm-value-res
              by value 1
              by content x"00".
       end-handle-display.

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
