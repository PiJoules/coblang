      * RUN: coblang %s 2>&1 | FileCheck %s

      * CHECK-DAG: @hello-world-str = global [16 x i8] c"Hello world    \00"
      * CHECK-DAG: [[NEWLINE:@.*]] = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
      * CHECK-DAG: [[FORMAT:@.*]] = private unnamed_addr constant [3 x i8] c"%s\00", align 1

      * CHECK: define internal void @module-program
      * CHECK:      call i32 (ptr, ...) @printf(ptr [[FORMAT]], ptr @hello-world-str)
      * CHECK-NEXT: call i32 (ptr, ...) @printf(ptr [[NEWLINE]])

      * CHECK:      define void @WORKING-STORAGE(ptr %0, i32 %1, ptr %2) {
      * CHECK-NEXT: entry:
      * CHECK-NEXT:   call void @module-program(i32 0)
      * CHECK-NEXT:   ret void
      * CHECK-NEXT: }
       IDENTIFICATION DIVISION.
         PROGRAM-ID. WORKING-STORAGE.
       DATA DIVISION.
         WORKING-STORAGE SECTION.
           01 hello-world-str PIC X(15) VALUE "Hello world".
       PROCEDURE DIVISION.
         DISPLAY hello-world-str.
         EXIT PROGRAM.
