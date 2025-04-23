      * RUN: coblang %s 2>&1 | FileCheck %s

      * CHECK: [[GLOB:@.*]] = private unnamed_addr constant
      * CHECK-SAME:           [12 x i8] c"Hello world\00", align 1
      * CHECK: [[NEWLINE:@.*]] = private unnamed_addr constant
      * CHECK-SAME:              [2 x i8] c"\0A\00", align 1
      
      * CHECK-LABEL: main
      * CHECK:      call i32 (ptr, ...) @printf(ptr [[GLOB]])
      * CHECK-NEXT: call i32 (ptr, ...) @printf(ptr [[NEWLINE]])
       IDENTIFICATION DIVISION.
         PROGRAM-ID. HELLO-WORLD.
       PROCEDURE DIVISION.
         DISPLAY 'Hello world'.
         EXIT PROGRAM.
