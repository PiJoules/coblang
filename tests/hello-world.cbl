      * RUN: coblang %s 2>&1 | FileCheck %s

      * CHECK-DAG: [[GLOB:@.*]] = private unnamed_addr constant [12 x i8] c"Hello world\00", align 1
      * CHECK-DAG: [[NEWLINE:@.*]] = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
      * CHECK-DAG: [[FORMAT:@.*]] = private unnamed_addr constant [3 x i8] c"%s\00", align 1
      
      * CHECK: define internal void @module-program
      * CHECK:      call i32 (ptr, ...) @printf(ptr [[FORMAT]], ptr [[GLOB]])
      * CHECK-NEXT: call i32 (ptr, ...) @printf(ptr [[NEWLINE]])

      * CHECK:      define void @HELLO-WORLD(ptr %0, i32 %1, ptr %2) {
      * CHECK-NEXT: entry:
      * CHECK-NEXT:   call void @module-program(i32 0)
      * CHECK-NEXT:   ret void
      * CHECK-NEXT: }
       IDENTIFICATION DIVISION.
         PROGRAM-ID. HELLO-WORLD.
       PROCEDURE DIVISION.
         DISPLAY 'Hello world'.
         EXIT PROGRAM.
