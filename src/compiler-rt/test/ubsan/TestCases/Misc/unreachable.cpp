// RUN: %clangxx -fsanitize=unreachable %s -O3 -o %t && %run %t 2>&1 | FileCheck %s

int main(int, char **argv) {
  // CHECK: unreachable.cpp:5:3: runtime error: execution reached a __builtin_unreachable() call
  __builtin_unreachable();
}
