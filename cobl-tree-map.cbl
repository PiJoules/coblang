       IDENTIFICATION DIVISION.
      * Mark as recursive since the tree-map-dtor can call the default
      * key-dtor function also defined here.
         PROGRAM-ID. tree-map recursive.
       DATA DIVISION.
         working-storage section.
      * TODO: Can these be in local-storage? This leads to a heap-use-after
      * free when placed in local-storage.
           01 tree-map-storage based.
              copy "cobl-tree-map-node.cpy".
           01 tree-map-storage2 based.
              copy "cobl-tree-map-node.cpy".
           78 tree-map-size value length of tree-map-storage.

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
              tree-map-key in local-tree-map key-arg cmp-return.
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
              tree-map-key in local-tree-map key-arg cmp-return.
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
