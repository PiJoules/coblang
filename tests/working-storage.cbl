      * RUN: coblang %s 2>&1 | FileCheck %s

      * CHECK: @hello-world-str = global [16 x i8] c"Hello world    \00"
      * CHECK: [[FORMAT:@.*]] = private unnamed_addr constant [3 x i8]
      * CHECK-SAME:             c"%s\00", align 1
      * CHECK: [[NEWLINE:@.*]] = private unnamed_addr constant [2 x i8]
      * CHECK-SAME:             c"\0A\00", align 1

      * CHECK-LABEL: main
      * CHECK:      call i32 (ptr, ...) @printf(ptr [[FORMAT]], ptr @hello-world-str)
      * CHECK-NEXT: call i32 (ptr, ...) @printf(ptr [[NEWLINE]])
       IDENTIFICATION DIVISION.
         PROGRAM-ID. WORKING-STORAGE.
       DATA DIVISION.
         WORKING-STORAGE SECTION.
           01 hello-world-str PIC X(15) VALUE "Hello world".
       PROCEDURE DIVISION.
         DISPLAY hello-world-str.
         EXIT PROGRAM.
