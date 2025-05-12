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
           01 LLVMInt32OneValue usage pointer value null.
           01 LLVMInt32NegOneValue usage pointer value null.
           01 LLVMNullPtrValue usage pointer value null.
           01 LLVMNewlineGlobalString usage pointer value null.
           01 LLVMStringFormatSpecifier usage pointer value null.
           01 LLVMEntryPointFuncType usage pointer value null.
           01 LLVMInternalFuncType usage pointer value null.

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
           01 llvm-type-res usage pointer.
           01 llvm-value-res usage pointer.
           01 llvm-value-res2 usage pointer.
           01 llvm-target-data-res usage pointer.
           01 llvm-bb-res usage pointer.

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

      * LLVMLinkage
           78 LLVMExternalLinkage value 0.
           78 LLVMAvailableExternallyLinkage value 1.
           78 LLVMLinkOnceAnyLinkage value 2.
           78 LLVMLinkOnceODRLinkage value 3.
           78 LLVMLinkOnceODRAutoHideLinkage value 4.
           78 LLVMWeakAnyLinkage value 5.
           78 LLVMWeakODRLinkage value 6.
           78 LLVMAppendingLinkage value 7.
           78 LLVMInternalLinkage value 8.
           78 LLVMPrivateLinkage value 9.
           78 LLVMDLLImportLinkage value 10.
           78 LLVMDLLExportLinkage value 11.
           78 LLVMExternalWeakLinkage value 12.
           78 LLVMGhostLinkage value 13.
           78 LLVMCommonLinkage value 14.
           78 LLVMLinkerPrivateLinkage value 15.
           78 LLVMLinkerPrivateWeakLinkage value 16.
           
      * LLVMIntPredicate
           78 LLVMIntEQ value 32.
           78 LLVMIntNE value 33.
           78 LLVMIntUGT value 34.
           78 LLVMIntUGE value 35.
           78 LLVMIntULT value 36.
           78 LLVMIntULE value 36.
           78 LLVMIntSGT value 38.
           78 LLVMIntSGE value 39.
           78 LLVMIntSLT value 40.
           78 LLVMIntSLE value 41.

      * LLVMVisibility
           78 LLVMDefaultVisibility value 0.
           78 LLVMHiddenVisibility value 1.
           78 LLVMProtectedVisibility value 2.

           01 func-type-params.
              copy "cobl-vector.cpy".

           01 all-indirect-brs.
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

           01 tmp-bool pic x.
           01 tmp-ptr usage pointer.
           01 tmp-ptr2 usage pointer.
      * This can be used as a buffer we can move pointers into. For example,
      * we might store a pointer in `tmp-ptr` from a call to LLVM and now we
      * want to store something at the address specified by `tmp-ptr`. We
      * can do that by setting the address of `tmp-ptr-storage` to the value
      * in `tmp-ptr` then MOVEing to `tmp-ptr-storage`.
           01 tmp-ptr-storage usage pointer based.
           01 tmp-char pic x.
           01 tmp-unsigned-long usage binary-c-long unsigned.
           01 tmp-unsigned-long2 usage binary-c-long unsigned.
           01 tmp-unsigned-long3 usage binary-c-long unsigned.
           01 tmp-unsigned-long-long usage binary-long-long unsigned.
           01 iter usage binary-c-long unsigned.
           01 tmp-char-storage pic x based.
           01 tmp-unsigned-long-storage usage binary-c-long unsigned
              based.
           01 last-parsed-pic-symbol pic x.
           01 is-linkage-section-global pic x.
           01 tmp-int usage binary-int.
           01 paragraph-bb usage pointer.
           01 paragraph-block-addr usage pointer.
           01 frame-ptr-alloca usage pointer.
           01 frame-ptr-value usage pointer.
           01 frame-stack-global usage pointer.
           01 perform-bb usage pointer.
           01 paragraph-block-addr-ret usage pointer.
           01 current-indirect-br usage pointer.
           01 num-entries usage binary-c-long unsigned.

           01 tmp-string.
              copy "cobl-string.cpy".
           01 entry-name-string.
              copy "cobl-string.cpy".
           01 move-from-string.
              copy "cobl-string.cpy".

           01 callee-ptr usage pointer.
      * This is a vector of LLVMValueRefs that will be stored in
      * the args alloca.
           01 parsed-callee-args.
              copy "cobl-vector.cpy".
           01 callee-args-type usage pointer.
           01 callee-gep-offsets occurs 2 times usage pointer.
      * These are the actual arguments passed to the call, which
      * are the arguments for LLVMEntryPointFuncType.
           01 callee-args.
              02 callee-args-alloca usage pointer.
              02 callee-args-num-alloca-args usage pointer.
              02 callee-args-ret-ptr usage pointer.
           01 callee-args-gep usage pointer.

      * This is a map of C strings to global pointers for linkage-section
      * variables.
           01 linkage-section-vars.
              copy "cobl-tree-map-node.cpy".
           01 linkage-section-var-types.
              copy "cobl-tree-map-node.cpy".
      * This is a map of C strings to basic blocks for paragraphs.
           01 paragraph-bbs.
              copy "cobl-tree-map-node.cpy".
      * This is a map of C strings to internal functions for paragraphs.
           01 paragraph-funcs.
              copy "cobl-tree-map-node.cpy".

           01 bb-entry-ptr usage pointer.
           01 builder-ptr usage pointer.
           01 entry-builder-ptr usage pointer.
           01 last-builder-ptr usage pointer.
           01 func-ptr usage pointer.
           01 program-func-ptr usage pointer.
           01 entry-func-ptr usage pointer.
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

           01 move-from-llvm-value-ptr usage pointer.
           01 move-from-size usage binary-c-long unsigned.
           01 move-to-llvm-value-ptr usage pointer.
           01 move-to-size usage binary-c-long unsigned.

         LINKAGE SECTION.
           01 this-codegen.
             copy "codegen.cpy".

           01 lexer-ptr-arg usage pointer.
           01 output-filename usage pointer.
           01 source-filename usage pointer.
           01 insert-main pic x.

       PROCEDURE DIVISION.
         stop run.

       entry "codegen-construct" using this-codegen lexer-ptr-arg
             source-filename.
         move lexer-ptr-arg to lexer-ptr in this-codegen.

         call "LLVMModuleCreateWithName"
              using by value source-filename
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
              returning llvm-data-layout in this-codegen.
         call "LLVMSetModuleDataLayout"
              using
                by value llvm-module in this-codegen
                by value llvm-data-layout in this-codegen.

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
         call "LLVMConstInt" using
              by value LLVMInt32Type
              by value 1
              by value 0
              returning LLVMInt32OneValue.
         call "LLVMConstInt" using
              by value LLVMInt32Type
              by value -1
              by value 0
              returning LLVMInt32NegOneValue.
         call "LLVMConstNull" using
              by value LLVMPtrType
              returning LLVMNullPtrValue.

      * All functions will have the signature
      *
      *   void func(void **args, unsigned numargs, void **ret)
      *
         call "vector-construct" using tmp-vector
              pointer-size pointer-align.

         call "vector-append-storage" using tmp-vector tmp-ptr.
         set address of tmp-ptr-storage to tmp-ptr.
         move LLVMPtrType to tmp-ptr-storage.

         call "vector-append-storage" using tmp-vector tmp-ptr.
         set address of tmp-ptr-storage to tmp-ptr.
         move LLVMInt32Type to tmp-ptr-storage.

         call "vector-append-storage" using tmp-vector tmp-ptr.
         set address of tmp-ptr-storage to tmp-ptr.
         move LLVMPtrType to tmp-ptr-storage.

         call "LLVMFunctionType" using
              by value LLVMVoidType
              by value vector-data in tmp-vector
              by value vector-size in tmp-vector
              by value 0
              returning LLVMEntryPointFuncType.

      * The internal function takes a single argument: a unique identifier
      * corresponding to the BB it should immediately jump to. This BB
      * corresponds to the entry point we would call into.
         call "LLVMFunctionType" using
              by value LLVMVoidType
              by value address of LLVMInt32Type
              by value 1
              by value 0
              returning LLVMInternalFuncType.

         call "vector-destroy" using tmp-vector.

         goback.

       entry "codegen-destroy" using this-codegen.
         call "string-destroy" using module-identifier in this-codegen.

         call "LLVMDisposeDIBuilder"
              using by value llvm-dibuilder in this-codegen.

         call "LLVMDisposeTargetMachine" using
              by value llvm-target-machine in this-codegen.

         call "LLVMDisposeTargetData" using
              by value llvm-data-layout in this-codegen.

         call "LLVMDisposeModule"
              using by value llvm-module in this-codegen.
         goback.

      *
      * This does the actual codegen.
      *
       entry "codegen-run" using this-codegen insert-main.
         call "string-tree-map-construct" using linkage-section-vars.
         call "string-tree-map-construct" using
              linkage-section-var-types.
         call "string-tree-map-construct" using
              paragraph-bbs.
         call "string-tree-map-construct" using
              paragraph-funcs.

         perform insert-program-func.

         call "LLVMAppendBasicBlock" using
              by value program-func-ptr
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
         call "vector-construct" using
              all-indirect-brs
              pointer-size pointer-align.

      * Add the frame stack. This is an array of pointers to return to
      * from performs to paragraphs.
         call "LLVMArrayType" using
              by value LLVMPtrType
              by value 256
              returning llvm-type-res.
         call "LLVMAddGlobal" using
              by value llvm-module in this-codegen
              by value llvm-type-res
              by content function concatenate("frame-stack", x"00")
              returning frame-stack-global.
         call "LLVMSetLinkage" using
              by value frame-stack-global
              by value LLVMInternalLinkage.
         call "LLVMConstNull" using
              by value llvm-type-res
              returning llvm-value-res.
         call "LLVMSetInitializer" using
              by value frame-stack-global
              by value llvm-value-res.
      * The pointer stored in frame-ptr-alloca is the current position
      * in the frame-stack-global (which is zero-initialized). When we are
      * at the end of a paragraph, we always check the value this pointer
      * points to which is always somewhere in the frame-stack-global. If
      * the value is NULL, which only the first element should be, then
      * we just jump to the next paragraph. Otherwise, we pop the current
      * ptr from this stack and jump to that.
         call "LLVMBuildAlloca" using
              by value builder-ptr
              by value LLVMPtrType
              by content function concatenate("frame-ptr-alloca", x"00")
              returning frame-ptr-alloca.
         call "LLVMBuildStore" using
              by value builder-ptr
              by value frame-stack-global
              by value frame-ptr-alloca.

         move 0 to num-entries.

         perform until lexer-eof in this-codegen-lexer = 'Y'
           perform get-token-string-and-buffer

           if lexer-eof in this-codegen-lexer = 'Y'
             exit perform
           end-if

      *     display "'" no advancing
      *     call "string-display" using token-string 'N'
      *     display "'"

           evaluate pic-buffer
             when "IDENTIFICATION"
               perform handle-identification-division
             when "PROCEDURE"
               perform handle-procedure-division
             when "PROGRAM-ID"
               perform handle-program-id
             when "DATA"
               perform handle-data-division
             when "DISPLAY"
               perform handle-display
             when "PERFORM"
               perform handle-perform
             when "ENTRY"
               perform handle-entry
             when "GOBACK"
               perform handle-goback
             when "EXIT"
               perform handle-exit
             when "MOVE"
               perform handle-move
             when "CALL"
               perform handle-call
      * Just skip period for now.
             when "."
               continue
             when other
      * This might be a paragraph declaration.
               call "string-construct" using tmp-string
               call "string-copy" using tmp-string token-string
               perform peek-token-string-and-buffer
               if pic-buffer = "."
                 call "string-copy" using token-string tmp-string
                 perform handle-paragraph
               else
                 display "error: Unknown token '" no advancing
                 call "string-display" using tmp-string 'N'
                 display "'"
                 stop run
               end-if
               call "string-destroy" using tmp-string
           end-evaluate
         end-perform.

      * End of the function.
         call "LLVMBuildRetVoid" using
              by value builder-ptr.

      * TODO: For each indirectbr, add all known BBs in the current function
      * as a destination. This prevents UB but definitely is not efficient
      * since we'd be adding some BBs to some indirectbrs that we can't
      * possibly branch to.
         move 0 to iter.
         perform until iter >= vector-size in all-indirect-brs
           call "vector-at" using all-indirect-brs
                iter tmp-ptr
           set address of tmp-ptr-storage to tmp-ptr
           move tmp-ptr-storage to current-indirect-br

           call "LLVMGetFirstBasicBlock" using
                by value program-func-ptr
                returning llvm-bb-res

      * Ensure we do not add the entry bb as a destination since this is
      * illegal in LLVM.
           call "LLVMGetEntryBasicBlock" using
                by value program-func-ptr
                returning bb-entry-ptr

           perform until llvm-bb-res = null
             if llvm-bb-res not = bb-entry-ptr
               call "LLVMAddDestination" using
                    by value current-indirect-br
                    by value llvm-bb-res
             end-if

             call "LLVMGetNextBasicBlock" using
                  by value llvm-bb-res
                  returning llvm-bb-res
           end-perform

           set iter up by 1
         end-perform.

         call "vector-destroy" using all-indirect-brs.
         call "vector-destroy" using sizes in parsed-pic-type.
         call "vector-destroy" using symbols in parsed-pic-type.
         call "string-destroy" using token-string.
         call "string-destroy" using parsed-identifier.
         call "string-destroy" using parsed-integer.
         call "string-destroy" using parsed-pic-value.

         call "LLVMDisposeBuilder" using by value builder-ptr.

         move program-func-ptr to tmp-ptr.
         perform verify-function.

         if insert-main = 'Y'
           perform insert-main-func

           call "LLVMAppendBasicBlock" using
                by value main-func-ptr
                by content function concatenate("entry", x"00")
                returning bb-entry-ptr

           call "LLVMCreateBuilder" returning builder-ptr
           call "LLVMPositionBuilderAtEnd" using
                by value builder-ptr
                by value bb-entry-ptr

           call "LLVMBuildCall2" using
                by value builder-ptr
                by value LLVMInternalFuncType
                by value program-func-ptr
                by value address of LLVMInt32ZeroValue
                by value 1
                by content x"00"

           call "LLVMBuildRet" using
                by value builder-ptr
                by value LLVMInt32ZeroValue

           move main-func-ptr to tmp-ptr
           perform verify-function

           call "LLVMDisposeBuilder" using by value builder-ptr
         end-if.

         call "tree-map-destroy" using linkage-section-vars.
         call "tree-map-destroy" using linkage-section-var-types.
         call "tree-map-destroy" using paragraph-bbs.
         call "tree-map-destroy" using paragraph-funcs.
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

       handle-paragraph.
      * 0) Create the BB for the new paragraph if it has not yet been created.
      *    Do not insert it yet.
         call "tree-map-get" using paragraph-bbs
              cobl-string-ptr in token-string
              paragraph-bb tmp-bool.

         if tmp-bool = 'N'
           call "LLVMCreateBasicBlockInContext" using
                by value LLVMContext
                by value cobl-string-ptr in token-string
                returning paragraph-bb
           call "tree-map-set" using paragraph-bbs
                cobl-string-ptr in token-string
                paragraph-bb
         end-if.

         call "LLVMCreateBasicBlockInContext" using
              by value LLVMContext
              by content function
                         concatenate("ret-back-to-perform", x"00")
              returning llvm-bb-res.

      * 1) Load the frame-ptr.
         call "LLVMBuildLoad2" using
              by value builder-ptr
              by value LLVMPtrType
              by value frame-ptr-alloca
              by content x"00"
              returning paragraph-block-addr-ret.
         call "LLVMBuildLoad2" using
              by value builder-ptr
              by value LLVMPtrType
              by value paragraph-block-addr-ret
              by content x"00"
              returning llvm-value-res.
      * 2) Test if the frame ptr is set.
         call "LLVMBuildICmp" using
              by value builder-ptr
              by value LLVMIntEQ
              by value llvm-value-res
              by value LLVMNullPtrValue
              by content x"00"
              returning llvm-value-res.
      * 3) If the frame ptr is not set, then jump to the next paragraph BB.
      *    Otherwise, jump to the frame-ptr then decrement where it points
      *    to.
         call "LLVMBuildCondBr" using
              by value builder-ptr
              by value llvm-value-res
              by value paragraph-bb
              by value llvm-bb-res.

      * 4) This is the BB we jump to if the frame-ptr is set. Before jumping
      *    to the frame-ptr we loaded, save a decremented version of it in
      *    the frame-ptr-alloca.
         call "LLVMAppendExistingBasicBlock" using
              by value program-func-ptr
              by value llvm-bb-res.
         call "LLVMPositionBuilderAtEnd" using
              by value builder-ptr
              by value llvm-bb-res.
      * Note we need to load from the alloca again in the new BB. We cannot
      * use the value from the previous BB since this would not dominate
      * all uses.
         call "LLVMBuildLoad2" using
              by value builder-ptr
              by value LLVMPtrType
              by value frame-ptr-alloca
              by content x"00"
              returning paragraph-block-addr-ret.
         call "LLVMBuildGEP2" using
              by value builder-ptr
              by value LLVMPtrType
              by value paragraph-block-addr-ret
              by value address of LLVMInt32NegOneValue
              by value 1
              by content x"00"
              returning llvm-value-res.
         call "LLVMBuildStore" using
              by value builder-ptr
              by value llvm-value-res
              by value frame-ptr-alloca.
         call "LLVMBuildLoad2" using
              by value builder-ptr
              by value LLVMPtrType
              by value paragraph-block-addr-ret
              by content x"00"
              returning llvm-value-res.
         call "LLVMBuildIndirectBr" using
              by value builder-ptr
              by value llvm-value-res
              by value 0
              returning llvm-value-res.
         call "vector-append-storage" using
              all-indirect-brs tmp-ptr.
         set address of tmp-ptr-storage to tmp-ptr.
         move llvm-value-res to tmp-ptr-storage.

      * 5) Now emit the next paragraph BB.
         call "LLVMAppendExistingBasicBlock" using
              by value program-func-ptr
              by value paragraph-bb.
         call "LLVMPositionBuilderAtEnd" using
              by value builder-ptr
              by value paragraph-bb.

         perform pop-period.
       end-handle-paragraph.

       handle-data-division.
         perform get-token-string-and-buffer.
         if pic-buffer not = "DIVISION"
           display "error: Expected DATA DIVISION"
           stop run.

         perform pop-period.

         perform forever
           perform peek-token-string-and-buffer

           if pic-buffer = "WORKING-STORAGE"
             perform get-token-string-and-buffer
             perform pop-section
             perform pop-period
             perform parse-working-storage-variables
           else if pic-buffer = "LINKAGE"
             perform get-token-string-and-buffer
             perform pop-section
             perform pop-period
             perform parse-linkage-variables
           else
             exit perform
           end-if
         end-perform.
       end-handle-data-division.

       parse-linkage-variables.
         perform forever
           perform peek-token-string-and-buffer
           if function trim(pic-buffer TRAILING) is not numeric
             exit perform
           end-if

           perform parse-level
           perform parse-identifier
           perform pop-pic
           perform parse-pic-type
           perform pop-period

           perform get-parsed-pic-size

      * Increment by 1 because we always append the null terminator.
           set tmp-unsigned-long up by 1
           call "LLVMArrayType" using
                by value LLVMInt8Type
                by value tmp-unsigned-long
                returning llvm-type-res

           call "LLVMAddGlobal" using
                by value llvm-module in this-codegen
                by value LLVMPtrType
                by value cobl-string-ptr in parsed-identifier
                returning llvm-value-res
           call "LLVMSetInitializer" using
                by value llvm-value-res
                by value LLVMNullPtrValue
           call "LLVMSetVisibility" using
                by value llvm-value-res
                by value LLVMHiddenVisibility

           call "tree-map-set" using
                linkage-section-vars
                cobl-string-ptr in parsed-identifier
                llvm-value-res
           call "tree-map-set" using
                linkage-section-var-types
                cobl-string-ptr in parsed-identifier
                llvm-type-res
         end-perform.
       end-parse-linkage-variables.

       parse-working-storage-variables.
         perform forever
           perform peek-token-string-and-buffer
           if function trim(pic-buffer TRAILING) is not numeric
             exit perform
           end-if

           perform parse-level
           perform parse-identifier
           perform pop-pic
           perform parse-pic-type
           perform pop-period

           perform get-parsed-pic-size
      * Increment by 1 because we always append the null terminator.
           set tmp-unsigned-long up by 1
           call "LLVMArrayType" using
                by value LLVMInt8Type
                by value tmp-unsigned-long
                returning tmp-ptr
           
      * TODO: This global should be hidden unless it's an EXTERNAL data item.
           call "LLVMAddGlobal" using
                by value llvm-module in this-codegen
                by value tmp-ptr
                by value cobl-string-ptr in parsed-identifier
                returning llvm-value-res

           perform get-parsed-pic-size
           call "string-resize" using parsed-pic-value
                tmp-unsigned-long ' '

           call "LLVMConstStringInContext" using
                by value LLVMContext
                by value cobl-string-ptr in parsed-pic-value
                by value cobl-string-length in parsed-pic-value
                by value 0
                returning llvm-value-res2

           call "LLVMSetInitializer" using
                by value llvm-value-res
                by value llvm-value-res2
         end-perform.
       end-parse-working-storage-variables.

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
             move token-string to tmp-string
             perform strip-quotes-from-string-token
             move tmp-string to token-string
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

       pop-to.
         perform get-token-string-and-buffer.
         if pic-buffer not = "TO"
           display "error: Expected `TO`"
           stop run.
       end-pop-to.

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

      * We parsed and popped a PROGRAM-ID token. Get the name.
       handle-program-id.
         perform pop-period.
         perform get-token-string-and-buffer.

         call "LLVMSetModuleIdentifier" using
              by value llvm-module in this-codegen
              by value cobl-string-ptr in token-string
              by value cobl-string-length in token-string.

         call "string-construct" using
              module-identifier in this-codegen.
         call "string-copy" using
              module-identifier in this-codegen
              token-string.

         perform pop-period.
       end-handle-program-id.

      * Given a function name in `tmp-string`, get the llvm function with
      * the same name, or create it. It will have type LLVMEntryPointFuncType.
      * The return value will be in `llvm-value-res`.
       get-or-create-external-function.
         call "LLVMGetNamedFunction" using
              by value llvm-module in this-codegen
              by value cobl-string-ptr in tmp-string
              returning llvm-value-res.

         if llvm-value-res = null
           call "LLVMAddFunction" using
                by value llvm-module in this-codegen
                by value cobl-string-ptr in tmp-string
                by value LLVMEntryPointFuncType
                returning llvm-value-res
         end-if.
       end-get-or-create-external-function.

      * Return an alloca in `llvm-value-res` that holds enough pointers
      * for a call. The number of elements is specified by `tmp-unsigned-long`.
       get-call-args-alloca.
         call "LLVMArrayType" using
              by value LLVMPtrType
              by value tmp-unsigned-long
              returning llvm-type-res.
         call "LLVMBuildAlloca" using
              by value builder-ptr
              by value llvm-type-res
              by content x"00"
              returning llvm-value-res.
       end-get-call-args-alloca.

       handle-call.
         perform get-token-string-and-buffer.

         move 0 to tmp-unsigned-long.
         call "string-at" using token-string tmp-unsigned-long tmp-char.
         if tmp-char not = '"' and tmp-char not = "'"
           display "error: Expected string for call but found '"
                   no advancing
           call "string-display" using token-string 'N'
           display "'"
           stop run
         end-if.

         move token-string to tmp-string.
         perform strip-quotes-from-string-token.
         perform get-or-create-external-function.
         move llvm-value-res to callee-ptr.

         if callee-ptr = null
           display "error: Unknown function '" no advancing
           call "string-display" using tmp-string 'N'
           display "'"
           stop run
         end-if.

         call "vector-construct" using
              parsed-callee-args pointer-size pointer-align.

         perform get-token-string-and-buffer.

         evaluate pic-buffer
           when "USING"
             perform forever
               perform get-expression
               call "vector-append-storage" using
                    parsed-callee-args tmp-ptr
               set address of tmp-ptr-storage to tmp-ptr
               move llvm-value-res to tmp-ptr-storage

               perform peek-token-string-and-buffer
               perform is-token-verb
               if tmp-bool = 'Y' or pic-buffer = "."
                 exit perform
               end-if
             end-perform
           when "."
             continue
           when other
             display "error: Unknown keyword followed by CALL '"
                     no advancing
             call "string-display" using token-string 'N'
             display "'"
             stop run
         end-evaluate.

         move vector-size in parsed-callee-args to tmp-unsigned-long.
         perform get-call-args-alloca.
         move llvm-value-res to callee-args-alloca.

         call "LLVMGetAllocatedType" using
              by value callee-args-alloca
              returning callee-args-type.

         move 0 to iter.
         perform until iter >= vector-size in parsed-callee-args
           move LLVMInt32ZeroValue to callee-gep-offsets(1)

           call "LLVMConstInt" using
                by value LLVMInt32Type
                by value iter
                by value 0
                returning callee-gep-offsets(2)

           set tmp-ptr to address of callee-gep-offsets(1)
           call "LLVMBuildGEP2" using
                by value builder-ptr
                by value callee-args-type
                by value callee-args-alloca
                by value tmp-ptr
                by value 2
                by content x"00"
                returning callee-args-gep

           call "vector-at" using parsed-callee-args iter
                tmp-ptr
           set address of tmp-ptr-storage to tmp-ptr

           call "LLVMBuildStore" using
                by value builder-ptr
                by value tmp-ptr-storage
                by value callee-args-gep

           set iter up by 1
         end-perform.

         call "LLVMConstInt" using
              by value LLVMInt32Type
              by value vector-size in parsed-callee-args
              returning callee-args-num-alloca-args.

         move LLVMNullPtrValue to callee-args-ret-ptr.

         set tmp-ptr to address of callee-args.
         call "LLVMBuildCall2" using
              by value builder-ptr
              by value LLVMEntryPointFuncType
              by value callee-ptr
              by value tmp-ptr
              by value 3
              by content x"00".

         call "vector-destroy" using parsed-callee-args.
       end-handle-call.

       handle-move.
         perform get-expression.
         move llvm-value-res to move-from-llvm-value-ptr.
         if is-linkage-section-global = 'Y'
           call "LLVMBuildLoad2" using
                by value builder-ptr
                by value LLVMPtrType
                by value move-from-llvm-value-ptr
                by content x"00"
                returning move-from-llvm-value-ptr
           call "tree-map-get" using
                linkage-section-var-types
                cobl-string-ptr in token-string
                tmp-ptr
                tmp-bool
           call "LLVMStoreSizeOfType" using
                by value llvm-data-layout in this-codegen
                by value tmp-ptr
                returning move-from-size
         else
           move move-from-llvm-value-ptr to tmp-ptr
           perform get-size-of-global
           move tmp-unsigned-long to move-from-size
         end-if.

         perform pop-to.

         perform get-expression.
         move llvm-value-res to move-to-llvm-value-ptr.
         if is-linkage-section-global = 'Y'
           display "linkage"
           call "LLVMBuildLoad2" using
                by value builder-ptr
                by value LLVMPtrType
                by value move-to-llvm-value-ptr
                by content x"00"
                returning move-to-llvm-value-ptr
           call "tree-map-get" using
                linkage-section-var-types
                cobl-string-ptr in token-string
                tmp-ptr
                tmp-bool
           call "LLVMStoreSizeOfType" using
                by value llvm-data-layout in this-codegen
                by value tmp-ptr
                returning move-to-size
         else
           move move-to-llvm-value-ptr to tmp-ptr
           perform get-size-of-global
           move tmp-unsigned-long to move-to-size
         end-if.

         move function min(move-from-size, move-to-size) to
              tmp-unsigned-long.
      * Each of these strings is null-terminated, but we don't want to copy the
      * null-terminator.
         set tmp-unsigned-long down by 1.

         call "LLVMConstInt" using
              by value LLVMInt32Type
              by value tmp-unsigned-long
              by value 0
              returning llvm-value-res.

         call "LLVMBuildMemCpy" using
              by value builder-ptr
              by value move-to-llvm-value-ptr
              by value 0
              by value move-from-llvm-value-ptr
              by value 0
              by value llvm-value-res.
       end-handle-move.

      * Given an llvm global in `tmp-ptr`, return the size (in bytes) of
      * that global's value type in `tmp-unsigned-long`.
       get-size-of-global.
         call "LLVMGlobalGetValueType" using
              by value tmp-ptr
              returning llvm-type-res.

         call "LLVMGetModuleDataLayout" using
              by value llvm-module in this-codegen
              returning llvm-target-data-res.

         call "LLVMStoreSizeOfType" using
              by value llvm-target-data-res
              by value llvm-type-res
              returning tmp-unsigned-long-long.

         move tmp-unsigned-long-long to tmp-unsigned-long.
       end-get-size-of-global.

      * Parse an expression and store it in llvm-value-res.
      *
      * If the expression is an identifier to a linkage section global,
      * this sets `is-linkage-section-global` to `Y` and the `token-string`
      * will contain that global's identifier. Otherwise, `N`.
       get-expression.
         perform get-token-string-and-buffer.
         move 'N' to is-linkage-section-global.

         call "string-front" using token-string tmp-char.
         if tmp-char = '"' or tmp-char = "'"
           move token-string to tmp-string
           perform get-llvm-string-from-string-literal
           exit paragraph
         end-if.

         call "LLVMGetNamedGlobal" using
              by value llvm-module in this-codegen
              by value cobl-string-ptr in token-string
              returning llvm-value-res.

         call "tree-map-get" using linkage-section-vars
              cobl-string-ptr in token-string
              tmp-ptr is-linkage-section-global.
       end-get-expression.

      * Given a cobl-string in `tmp-string`, strip the leading and trailing
      * quotes from the string.
       strip-quotes-from-string-token.
         move 0 to tmp-unsigned-long.
         call "string-erase" using tmp-string tmp-unsigned-long.
         compute tmp-unsigned-long =
           cobl-string-length in tmp-string - 1.
         call "string-erase" using tmp-string tmp-unsigned-long.
       end-strip-quotes-from-string-token.

      * Given a string literal in `tmp-string`, return an llvm-string in
      * `llvm-value-res`.
       get-llvm-string-from-string-literal.
         perform strip-quotes-from-string-token.

         call "LLVMBuildGlobalStringPtr" using
              by value builder-ptr
              by value cobl-string-ptr in tmp-string
              by content x"00"
              returning llvm-value-res.
       end-get-llvm-string-from-string-literal.

      * Given an identifier in `tmp-string`, return a global with the name
      * in `llvm-value-res`.
       get-named-global.
         call "LLVMGetNamedGlobal" using
              by value llvm-module in this-codegen
              by value cobl-string-ptr in tmp-string
              returning llvm-value-res.

         if llvm-value-res = null
           display "error: Could not find global '" no advancing
           call "string-display" using tmp-string 'N'
           display "'"
           stop run
         end-if.
       end-get-named-global.

       handle-goback.
         call "LLVMBuildRetVoid" using by value builder-ptr.

         call "LLVMAppendBasicBlockInContext" using
              by value LLVMContext
              by value program-func-ptr
              by content x"00"
              returning llvm-bb-res.
         call "LLVMPositionBuilderAtEnd" using
              by value builder-ptr
              by value llvm-bb-res.
       end-handle-goback.

       handle-entry.
         perform get-token-string-and-buffer.

         if not (pic-buffer(1:1) = "'" or pic-buffer(1:1) = '"')
           display "error: Expected string for ENTRY but found '"
                   no advancing
           call "string-display" using token-string 'N'
           display "'"
           stop run
         end-if.

         call "string-construct" using tmp-string.
         call "string-construct" using entry-name-string.
         call "string-copy" using tmp-string token-string.
         perform strip-quotes-from-string-token.
         call "string-copy" using entry-name-string tmp-string.
         call "string-destroy" using tmp-string.

         call "LLVMAddFunction" using
              by value llvm-module in this-codegen
              by value cobl-string-ptr in entry-name-string
              by value LLVMEntryPointFuncType
              returning entry-func-ptr.
         call "LLVMAppendBasicBlock" using
              by value entry-func-ptr
              by content function concatenate("entry", x"00")
              returning bb-entry-ptr.
         call "LLVMCreateBuilder" returning entry-builder-ptr.
         call "LLVMPositionBuilderAtEnd" using
              by value entry-builder-ptr
              by value bb-entry-ptr.

         perform peek-token-string-and-buffer.
         if pic-buffer = "USING"
           perform get-token-string-and-buffer
           move 0 to iter

           perform forever
             perform get-token-string-and-buffer
             if pic-buffer = "."
               exit perform
             end-if

             call "tree-map-get" using
                  linkage-section-vars
                  cobl-string-ptr in token-string
                  llvm-value-res2
                  tmp-bool

             if tmp-bool = 'N'
               display "error: Unexpected linkage-section variable '"
                       no advancing
               call "string-display" using token-string 'N'
               display "'"
               stop run
             end-if

      * Set each USING argument to one of the function arguments.
             call "LLVMGetParam" using
                  by value entry-func-ptr
                  by value 0
                  returning llvm-value-res

             call "LLVMConstInt" using
                  by value LLVMInt32Type
                  by value iter
                  by value 0
                  returning callee-gep-offsets(1)

             set tmp-ptr to address of callee-gep-offsets(1)
             call "LLVMBuildGEP2" using
                  by value entry-builder-ptr
                  by value LLVMPtrType
                  by value llvm-value-res
                  by value tmp-ptr
                  by value 1
                  by content x"00"
                  returning llvm-value-res

             call "LLVMBuildLoad2" using
                  by value entry-builder-ptr
                  by value LLVMPtrType
                  by value llvm-value-res
                  by value cobl-string-ptr in token-string
                  returning llvm-value-res

             call "LLVMBuildStore" using
                  by value entry-builder-ptr
                  by value llvm-value-res
                  by value llvm-value-res2

             set iter up by 1
           end-perform
         end-if.

         call "LLVMConstInt" using
              by value LLVMInt32Type
              by value num-entries
              by value 0
              returning llvm-value-res.

         call "LLVMBuildCall2" using
              by value entry-builder-ptr
              by value LLVMInternalFuncType
              by value program-func-ptr
              by value address of llvm-value-res
              by value 1
              by content x"00".

         call "LLVMBuildRetVoid" using by value entry-builder-ptr.
         call "LLVMDisposeBuilder" using by value entry-builder-ptr.

      * Emit a new BB for this entry point that the switch at the very start
      * of the program function dispatches to.
         call "LLVMCreateBasicBlockInContext" using
              by value LLVMContext
              by value cobl-string-ptr in entry-name-string
              returning llvm-bb-res.
         call "LLVMBuildBr" using
              by value builder-ptr
              by value llvm-bb-res.
         call "LLVMAppendExistingBasicBlock" using
              by value program-func-ptr
              by value llvm-bb-res.
         call "LLVMPositionBuilderAtEnd" using
              by value builder-ptr
              by value llvm-bb-res.

      * Add the case to the switch at the very start of the program function.
         call "LLVMAddCase" using
              by value switch-ptr in this-codegen
              by value llvm-value-res
              by value llvm-bb-res.

         set num-entries up by 1.
         call "string-destroy" using entry-name-string.
       end-handle-entry.

       is-last-instruction-terminator.
         call "LLVMGetInsertBlock" using
              by value builder-ptr
              returning llvm-bb-res.
         call "LLVMGetLastInstruction" using
              by value llvm-bb-res
              returning llvm-value-res.
         if llvm-value-res = null
           move 'N' to tmp-bool
           exit paragraph
         end-if.

         call "LLVMIsATerminatorInst" using
              by value llvm-value-res
              returning tmp-int.
         if tmp-int = 0
           move 'N' to tmp-bool
         else
           move 'Y' to tmp-bool
         end-if.
       end-is-last-instruction-terminator.

       handle-perform.
      * ALl paragraphs are implemented as static functions to make returning
      * back from performs simpler.
         perform get-token-string-and-buffer.

         call "tree-map-get" using paragraph-bbs
              cobl-string-ptr in token-string
              perform-bb tmp-bool.

         if tmp-bool = 'N'
           call "LLVMCreateBasicBlockInContext" using
                by value LLVMContext
                by value cobl-string-ptr in token-string
                returning perform-bb
           call "tree-map-set" using paragraph-bbs
                cobl-string-ptr in token-string
                perform-bb
         end-if.

      * Store the return address in the frame stack.
      * This is the BB coming immediately after this perform.
         call "LLVMCreateBasicBlockInContext" using
              by value LLVMContext
              by content x"00"
              returning llvm-bb-res.
         call "LLVMBlockAddress" using
              by value program-func-ptr
              by value llvm-bb-res
              returning llvm-value-res.

      * 1) Increment the frame-ptr.
         call "LLVMBuildLoad2" using
              by value builder-ptr
              by value LLVMPtrType
              by value frame-ptr-alloca
              by content x"00"
              returning frame-ptr-value.
         call "LLVMBuildGEP2" using
              by value builder-ptr
              by value LLVMPtrType
              by value frame-ptr-value
              by value address of LLVMInt32OneValue
              by value 1
              by content x"00"
              returning frame-ptr-value.

      * 2) Store the return address at this index in the frame-stack
      *    which is pointed to by the incremented frame-ptr.
         call "LLVMBuildStore" using
              by value builder-ptr
              by value llvm-value-res
              by value frame-ptr-value.

      * 3) Now store the incremented frame-ptr back into the alloca.
         call "LLVMBuildStore" using
              by value builder-ptr
              by value frame-ptr-value
              by value frame-ptr-alloca.

      * 2) Do a direct jump to the perform target.
         call "LLVMBuildBr" using
              by value builder-ptr
              by value perform-bb.

      * 3) Emit the returning BB.
         call "LLVMAppendExistingBasicBlock" using
              by value program-func-ptr
              by value llvm-bb-res.
         call "LLVMPositionBuilderAtEnd" using
              by value builder-ptr
              by value llvm-bb-res.
       end-handle-perform.

      * We parsed and popped a DISPLAY token. Spin up a printf.
       handle-display.
         perform get-token-string-and-buffer.

         move 0 to tmp-unsigned-long.
         call "string-at" using token-string tmp-unsigned-long tmp-char.
         if tmp-char = '"' or tmp-char = "'"
           move token-string to tmp-string
           perform get-llvm-string-from-string-literal
         
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
           move token-string to tmp-string
           perform get-named-global

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

       handle-identification-division.
         perform get-token-string-and-buffer

         if pic-buffer not = "DIVISION"
           display "error: Expected 'IDENTIFICATION DIVISION'."
           stop run
         end-if.

         perform pop-period.
       end-handle-identification-division.

       handle-procedure-division.
         perform get-token-string-and-buffer

         if pic-buffer not = "DIVISION"
           display "error: Expected 'PROCEDURE DIVISION'."
           stop run
         end-if.

         perform pop-period.

      * Add the initial procedure entry point.
         call "LLVMAddFunction" using
              by value llvm-module in this-codegen
              by value cobl-string-ptr in
                       module-identifier in this-codegen
              by value LLVMEntryPointFuncType
              returning entry-func-ptr.
         call "LLVMAppendBasicBlock" using
              by value entry-func-ptr
              by content function concatenate("entry", x"00")
              returning bb-entry-ptr.
         call "LLVMCreateBuilder" returning entry-builder-ptr.
         call "LLVMPositionBuilderAtEnd" using
              by value entry-builder-ptr
              by value bb-entry-ptr.

         call "LLVMBuildCall2" using
              by value entry-builder-ptr
              by value LLVMInternalFuncType
              by value program-func-ptr
              by value address of LLVMInt32ZeroValue
              by value 1
              by content x"00".

         call "LLVMBuildRetVoid" using by value entry-builder-ptr.
         call "LLVMDisposeBuilder" using by value entry-builder-ptr.

      * In the procedure division, emit the switch for dispatching
      * between entries.
         call "LLVMCreateBasicBlockInContext" using
              by value LLVMContext
              by content function concatenate("procedure-entry", x"00")
              returning llvm-bb-res.
         call "LLVMGetFirstParam" using
              by value program-func-ptr
              returning llvm-value-res.
         call "LLVMBuildSwitch" using
              by value builder-ptr
              by value llvm-value-res
              by value llvm-bb-res
              by value 0
              returning switch-ptr in this-codegen.
         call "LLVMAppendExistingBasicBlock" using
              by value program-func-ptr
              by value llvm-bb-res.
         call "LLVMPositionBuilderAtEnd" using
              by value builder-ptr
              by value llvm-bb-res.

         set num-entries up by 1.
       end-handle-procedure-division.

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

       insert-program-func.
         call "LLVMGetNamedFunction" using
              by value llvm-module in this-codegen
              by content function concatenate("module-program", x"00")
              returning program-func-ptr.

         if program-func-ptr not = null
           exit paragraph.

         call "LLVMAddFunction" using
              by value llvm-module in this-codegen
              by content function concatenate("module-program", x"00")
              by value LLVMInternalFuncType
              returning program-func-ptr.

         call "LLVMSetLinkage" using
              by value program-func-ptr
              by value LLVMInternalLinkage.
       end-insert-program-func.

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

      * Verify the llvm function at tmp-ptr.
       verify-function.
         call "LLVMVerifyFunction" using
              by value tmp-ptr
              by value LLVMPrintMessageAction
              returning llvm-result.

         if llvm-result not = zero
           display "Verify function failed"
           call "LLVMDumpValue" using by value tmp-ptr
           stop run
         end-if.
       end-verify-function.

      * Check if `token-string` is a verb. If it is, set `tmp-bool` to 'Y'.
      * Otherwise, 'N'.
       is-token-verb.
         if pic-buffer = "MOVE" or
            pic-buffer = "CALL" or
            pic-buffer = "DISPLAY" or
            pic-buffer = "PERFORM" or
            pic-buffer = "GOBACK" or
            pic-buffer = "EXIT"
           move 'Y' to tmp-bool
         else
           move 'Y' to tmp-bool
         end-if.
       end-is-token-verb.
