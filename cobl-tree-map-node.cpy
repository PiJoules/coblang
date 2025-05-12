      * NOTE: This must always be aligned with the CoblTreeMap struct
      * in coblang.h
       49 tree-map-key usage pointer.
       49 tree-map-value usage pointer.
       49 tree-map-left-node usage pointer.
       49 tree-map-right-node usage pointer.
      * The cmp function takes two keys and returns zero if the keys are equal,
      * a negative number if the left key is less than the right key, or a
      * positive number if the left key is greater than the right key.
       49 tree-map-key-cmp usage program-pointer.
      * The key-ctor function takes a key as a pointer then returns the key
      * (also as a pointer) to be stored in the actual map.
       49 tree-map-key-ctor usage program-pointer.
       49 tree-map-key-dtor usage program-pointer.
