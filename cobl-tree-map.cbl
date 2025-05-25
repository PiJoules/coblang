       IDENTIFICATION DIVISION.
      * Mark as recursive since the tree-map-dtor can call the default
      * key-dtor function also defined here.
         PROGRAM-ID. tree-map recursive.
       DATA DIVISION.
         working-storage section.
      * TODO: Can these be in local-storage? This leads to a heap-use-after
      * free when placed in local-storage but only when invoked from the
      * test-coblang.cpp tests. No errors occur when invoked from a normal
      * cobol program.
           01 tree-map-storage based.
              copy "cobl-tree-map-node.cpy".
           01 tree-map-storage2 based.
              copy "cobl-tree-map-node.cpy".
           78 tree-map-size value length of tree-map-storage.

           01 left-tree-cpy.
              copy "cobl-tree-map-node.cpy".
           01 right-tree-cpy.
              copy "cobl-tree-map-node.cpy".
           01 current-node based.
              copy "cobl-tree-map-node.cpy".
           01 parents-ptr-to-current-node usage pointer based.
           01 node-storage based.
              copy "cobl-tree-map-node.cpy".

         local-storage section.
           01 tmp-ptr usage pointer.
           01 src-char-buffer pic x based.
           01 strlen usage binary-c-long unsigned.

           01 local-tree-map-cmp-arg usage program-pointer.
           01 local-tree-map-ctor-arg usage program-pointer.
           01 local-tree-map-dtor-arg usage program-pointer.
           01 cmp-return usage binary-int.

         linkage section.
           01 local-tree-map.
              copy "cobl-tree-map-node.cpy".
           01 dst-tree-map.
              copy "cobl-tree-map-node.cpy".

           01 tree-map-cmp-arg usage program-pointer.
           01 tree-map-ctor-arg usage program-pointer.
           01 tree-map-dtor-arg usage program-pointer.

           01 key-arg usage pointer.
           01 val-arg usage pointer.
           01 cmp-return-arg usage binary-int.
           01 ptr-return-arg usage pointer.

           01 lhs-key-arg usage pointer.
           01 rhs-key-arg usage pointer.
           01 bool-return-arg pic x.
       procedure division.
         stop run.

       entry "tree-map-construct" using
             local-tree-map tree-map-cmp-arg
             tree-map-ctor-arg tree-map-dtor-arg.
         move null to tree-map-left-node in local-tree-map.
         move null to tree-map-right-node in local-tree-map.
         move null to tree-map-key in local-tree-map.
         move null to tree-map-value in local-tree-map.
         move tree-map-cmp-arg to
              tree-map-key-cmp in local-tree-map.
         if tree-map-ctor-arg = null
           set tree-map-key-ctor in local-tree-map to
               entry "key-ctor-default"
         else
           move tree-map-ctor-arg to
                tree-map-key-ctor in local-tree-map
         end-if.
         if tree-map-dtor-arg = null
           set tree-map-key-dtor in local-tree-map to
               entry "key-dtor-default"
         else
           move tree-map-dtor-arg to
                tree-map-key-dtor in local-tree-map
         end-if.
         goback.

      * Create a tree map where the keys are C-style strings.
       entry "string-tree-map-construct" using local-tree-map.
         set local-tree-map-cmp-arg to entry "string-tree-map-cmp".
         set local-tree-map-ctor-arg to
             entry "string-tree-map-key-ctor".
         set local-tree-map-dtor-arg to
             entry "string-tree-map-key-dtor".
         call "tree-map-construct" using local-tree-map
              local-tree-map-cmp-arg local-tree-map-ctor-arg
              local-tree-map-dtor-arg.
         goback.

       entry "key-ctor-default" using key-arg ptr-return-arg.
         move key-arg to ptr-return-arg.
         goback.

       entry "key-dtor-default" using key-arg.
         goback.

       entry "string-tree-map-cmp" using lhs-key-arg rhs-key-arg
             cmp-return-arg.
         call "strcmp" using
              by value lhs-key-arg
              by value rhs-key-arg
              returning cmp-return-arg.
         goback.

       entry "string-tree-map-key-ctor" using key-arg ptr-return-arg.
         call "cobl-strlen" using key-arg strlen.
         set strlen up by 1.

         allocate strlen characters returning ptr-return-arg.

         set strlen down by 1.
         call "cobl-memcpy" using ptr-return-arg key-arg strlen.

         move ptr-return-arg to tmp-ptr.
         set tmp-ptr up by strlen.
         set address of src-char-buffer to tmp-ptr.
         move x"00" to src-char-buffer.
         goback.

       entry "string-tree-map-key-dtor" using key-arg.
         if key-arg not = null
           free key-arg
         end-if.

         goback.

       entry "tree-map-destroy" using local-tree-map.
         if tree-map-left-node in local-tree-map not = null
           set address of tree-map-storage to
               tree-map-left-node in local-tree-map
           call "tree-map-destroy" using tree-map-storage
           free tree-map-left-node in local-tree-map
           move null to tree-map-left-node in local-tree-map
         end-if.

         if tree-map-right-node in local-tree-map not = null
           set address of tree-map-storage to
               tree-map-right-node in local-tree-map
           call "tree-map-destroy" using tree-map-storage
           free tree-map-right-node in local-tree-map
           move null to tree-map-right-node in local-tree-map
         end-if.

         call tree-map-key-dtor in local-tree-map using
              tree-map-key in local-tree-map.
         move null to tree-map-key in local-tree-map.
         goback.

       entry "tree-map-left" using local-tree-map ptr-return-arg.
         move tree-map-left-node in local-tree-map
              to ptr-return-arg.
         goback.

       entry "tree-map-right" using local-tree-map ptr-return-arg.
         move tree-map-right-node in local-tree-map
              to ptr-return-arg.
         goback.

       entry "tree-map-key" using local-tree-map ptr-return-arg.
         move tree-map-key in local-tree-map to ptr-return-arg.
         goback.

       entry "tree-map-value" using local-tree-map ptr-return-arg.
         move tree-map-value in local-tree-map to ptr-return-arg.
         goback.

      * Store a pointer value in the tree map given a key.
       entry "tree-map-set" using local-tree-map key-arg val-arg.
         if tree-map-key in local-tree-map = null
           call tree-map-key-ctor in local-tree-map using key-arg
                tree-map-key in local-tree-map
           move val-arg to tree-map-value in local-tree-map
           goback
         end-if.

         call tree-map-key-cmp in local-tree-map using
              key-arg tree-map-key in local-tree-map cmp-return.
         if cmp-return < 0
           if tree-map-left-node in local-tree-map = null
               allocate tree-map-size characters returning
                        tree-map-left-node in local-tree-map
               set address of tree-map-storage to
                   tree-map-left-node in local-tree-map
               move tree-map-key-cmp in local-tree-map to
                    local-tree-map-cmp-arg
               move tree-map-key-ctor in local-tree-map to
                    local-tree-map-ctor-arg
               move tree-map-key-dtor in local-tree-map to
                    local-tree-map-dtor-arg
               call "tree-map-construct" using
                    tree-map-storage local-tree-map-cmp-arg
                    local-tree-map-ctor-arg local-tree-map-dtor-arg
           end-if

           set address of tree-map-storage to
               tree-map-left-node in local-tree-map
           call "tree-map-set" using tree-map-storage key-arg val-arg
         else if cmp-return > 0
           if tree-map-right-node in local-tree-map = null
               allocate tree-map-size characters returning
                        tree-map-right-node in local-tree-map
               set address of tree-map-storage to
                   tree-map-right-node in local-tree-map
               move tree-map-key-cmp in local-tree-map to
                    local-tree-map-cmp-arg
               move tree-map-key-ctor in local-tree-map to
                    local-tree-map-ctor-arg
               move tree-map-key-dtor in local-tree-map to
                    local-tree-map-dtor-arg
               call "tree-map-construct" using
                    tree-map-storage local-tree-map-cmp-arg
                    local-tree-map-ctor-arg local-tree-map-dtor-arg
           end-if

           set address of tree-map-storage to
               tree-map-right-node in local-tree-map
           call "tree-map-set" using tree-map-storage key-arg val-arg
         else
           move val-arg to tree-map-value in local-tree-map
         end-if.
         goback.

       entry "tree-map-has" using local-tree-map key-arg
             bool-return-arg.
         call "tree-map-get" using local-tree-map key-arg tmp-ptr
              bool-return-arg.
         goback.

      * Get a value in the tree map given a key. If the key exists, `val-arg`
      * is set to the stored pointer value in the tree and `bool-return-arg`
      * is 'Y'. Otherwise, `bool-return-arg` is 'N'.
       entry "tree-map-get" using local-tree-map key-arg val-arg
             bool-return-arg.
         if tree-map-key in local-tree-map = null
           move 'N' to bool-return-arg
           goback
         end-if.

         call tree-map-key-cmp in local-tree-map using
              key-arg tree-map-key in local-tree-map cmp-return.
         if cmp-return < 0
           if tree-map-left-node in local-tree-map = null
             move 'N' to bool-return-arg
             goback
           end-if

           set address of tree-map-storage to
               tree-map-left-node in local-tree-map
           call "tree-map-get" using tree-map-storage key-arg val-arg
                bool-return-arg
         else if cmp-return > 0
           if tree-map-right-node in local-tree-map = null
             move 'N' to bool-return-arg
             goback
           end-if

           set address of tree-map-storage to
               tree-map-right-node in local-tree-map
           call "tree-map-get" using tree-map-storage key-arg val-arg
                bool-return-arg
         else
           move tree-map-value in local-tree-map to val-arg
           move 'Y' to bool-return-arg
         end-if.

         goback.

       entry "tree-map-erase" using local-tree-map key-arg.
         if tree-map-key in local-tree-map = null
           display "abort: Empty map"
           stop run
         end-if.

         set address of current-node to address of local-tree-map.
         set address of parents-ptr-to-current-node to null.

         perform forever
           if address of current-node = null
             display "abort: Unknown key " key-arg
             stop run
           end-if

           call tree-map-key-cmp in current-node using
                key-arg tree-map-key in current-node cmp-return

           if cmp-return < 0
             set address of parents-ptr-to-current-node to
                 address of tree-map-left-node in current-node
             set address of current-node to
                 tree-map-left-node in current-node
           else if cmp-return > 0
             set address of parents-ptr-to-current-node to
                 address of tree-map-right-node in current-node
             set address of current-node to
                 tree-map-right-node in current-node
           else
             exit perform
           end-if
         end-perform.

      * FIXME: This is wildly inefficient, but it gets the job done.
         call "tree-map-construct" using left-tree-cpy
              tree-map-key-cmp in current-node
              tree-map-key-ctor in current-node
              tree-map-key-dtor in current-node.
         call "tree-map-construct" using right-tree-cpy
              tree-map-key-cmp in current-node
              tree-map-key-ctor in current-node
              tree-map-key-dtor in current-node.

         if tree-map-left-node in current-node not = null
           set address of node-storage to
               tree-map-left-node in current-node
           call "tree-map-clone" using left-tree-cpy node-storage
         end-if.
         if tree-map-right-node in current-node not = null
           set address of node-storage to
               tree-map-right-node in current-node
           call "tree-map-clone" using right-tree-cpy node-storage
         end-if.

         if address of parents-ptr-to-current-node not = null
           move null to parents-ptr-to-current-node
         end-if.
         call "tree-map-destroy" using current-node.
         free address of current-node.

         call "tree-map-add" using local-tree-map left-tree-cpy.
         call "tree-map-add" using local-tree-map right-tree-cpy.

         call "tree-map-destroy" using left-tree-cpy.
         call "tree-map-destroy" using right-tree-cpy.

         goback.

      * Copy the contents of the `local-tree-map` into the `dst-tree-map`.
       entry "tree-map-add" using dst-tree-map local-tree-map.
         if tree-map-key in local-tree-map = null
           goback.

         if tree-map-left-node in local-tree-map not = null
           set address of node-storage to
               tree-map-left-node in local-tree-map
           call "tree-map-add" using dst-tree-map node-storage
         end-if.

         if tree-map-right-node in local-tree-map not = null
           set address of node-storage to
               tree-map-right-node in local-tree-map
           call "tree-map-add" using dst-tree-map node-storage
         end-if.

         call "tree-map-set" using dst-tree-map
              tree-map-key in local-tree-map
              tree-map-value in local-tree-map.

         goback.

       entry "tree-map-clear" using local-tree-map.
         move tree-map-key-cmp in local-tree-map to
              local-tree-map-cmp-arg.
         move tree-map-key-ctor in local-tree-map to
              local-tree-map-ctor-arg.
         move tree-map-key-dtor in local-tree-map to
              local-tree-map-dtor-arg.
         call "tree-map-destroy" using local-tree-map.
         call "tree-map-construct" using local-tree-map
              local-tree-map-cmp-arg
              local-tree-map-ctor-arg
              local-tree-map-dtor-arg.
         goback.

       entry "tree-map-clone" using dst-tree-map local-tree-map.
         call "tree-map-clear" using dst-tree-map.

         if tree-map-key in local-tree-map = null
           goback.

         call tree-map-key-ctor in local-tree-map using
              tree-map-key in local-tree-map
              tree-map-key in dst-tree-map.

         move tree-map-value in local-tree-map to
              tree-map-value in dst-tree-map.

         if tree-map-left-node in local-tree-map not = null
           allocate tree-map-size characters returning
                    tree-map-left-node in dst-tree-map
           set address of tree-map-storage to
               tree-map-left-node in dst-tree-map
           move tree-map-key-cmp in local-tree-map to
                local-tree-map-cmp-arg
           move tree-map-key-ctor in local-tree-map to
                local-tree-map-ctor-arg
           move tree-map-key-dtor in local-tree-map to
                local-tree-map-dtor-arg
           call "tree-map-construct" using
                tree-map-storage local-tree-map-cmp-arg
                local-tree-map-ctor-arg local-tree-map-dtor-arg

           set address of tree-map-storage2 to
               tree-map-left-node in local-tree-map
           call "tree-map-clone" using
                tree-map-storage tree-map-storage2
         else
           move null to tree-map-left-node in dst-tree-map
         end-if.

         if tree-map-right-node in local-tree-map not = null
           allocate tree-map-size characters returning
                    tree-map-right-node in dst-tree-map
           set address of tree-map-storage to
               tree-map-right-node in dst-tree-map
           move tree-map-key-cmp in local-tree-map to
                local-tree-map-cmp-arg
           move tree-map-key-ctor in local-tree-map to
                local-tree-map-ctor-arg
           move tree-map-key-dtor in local-tree-map to
                local-tree-map-dtor-arg
           call "tree-map-construct" using
                tree-map-storage local-tree-map-cmp-arg
                local-tree-map-ctor-arg local-tree-map-dtor-arg

           set address of tree-map-storage2 to
               tree-map-right-node in local-tree-map
           call "tree-map-clone" using
                tree-map-storage tree-map-storage2
         else
           move null to tree-map-right-node in dst-tree-map
         end-if.

         goback.
