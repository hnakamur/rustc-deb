// RUN: %clang_builtins %s %librt -o %t && %run %t
//===-- udivsi3_test.c - Test __udivsi3 -----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file tests __udivsi3 for the compiler_rt library.
//
//===----------------------------------------------------------------------===//

#include "int_lib.h"
#include <stdio.h>

// Returns: a / b

COMPILER_RT_ABI su_int __udivsi3(su_int a, su_int b);

int test__udivsi3(su_int a, su_int b, su_int expected_q)
{
    su_int q = __udivsi3(a, b);
    if (q != expected_q)
        printf("error in __udivsi3: %X / %X = %X, expected %X\n",
               a, b, q, expected_q);
    return q != expected_q;
}

su_int tests[][4] =
{
{0x00000000, 0x00000001, 0x00000000},
{0x00000000, 0x00000002, 0x00000000},
{0x00000000, 0x00000003, 0x00000000},
{0x00000000, 0x00000010, 0x00000000},
{0x00000000, 0x078644FA, 0x00000000},
{0x00000000, 0x0747AE14, 0x00000000},
{0x00000000, 0x7FFFFFFF, 0x00000000},
{0x00000000, 0x80000000, 0x00000000},
{0x00000000, 0xFFFFFFFD, 0x00000000},
{0x00000000, 0xFFFFFFFE, 0x00000000},
{0x00000000, 0xFFFFFFFF, 0x00000000},
{0x00000001, 0x00000001, 0x00000001},
{0x00000001, 0x00000002, 0x00000000},
{0x00000001, 0x00000003, 0x00000000},
{0x00000001, 0x00000010, 0x00000000},
{0x00000001, 0x078644FA, 0x00000000},
{0x00000001, 0x0747AE14, 0x00000000},
{0x00000001, 0x7FFFFFFF, 0x00000000},
{0x00000001, 0x80000000, 0x00000000},
{0x00000001, 0xFFFFFFFD, 0x00000000},
{0x00000001, 0xFFFFFFFE, 0x00000000},
{0x00000001, 0xFFFFFFFF, 0x00000000},
{0x00000002, 0x00000001, 0x00000002},
{0x00000002, 0x00000002, 0x00000001},
{0x00000002, 0x00000003, 0x00000000},
{0x00000002, 0x00000010, 0x00000000},
{0x00000002, 0x078644FA, 0x00000000},
{0x00000002, 0x0747AE14, 0x00000000},
{0x00000002, 0x7FFFFFFF, 0x00000000},
{0x00000002, 0x80000000, 0x00000000},
{0x00000002, 0xFFFFFFFD, 0x00000000},
{0x00000002, 0xFFFFFFFE, 0x00000000},
{0x00000002, 0xFFFFFFFF, 0x00000000},
{0x00000003, 0x00000001, 0x00000003},
{0x00000003, 0x00000002, 0x00000001},
{0x00000003, 0x00000003, 0x00000001},
{0x00000003, 0x00000010, 0x00000000},
{0x00000003, 0x078644FA, 0x00000000},
{0x00000003, 0x0747AE14, 0x00000000},
{0x00000003, 0x7FFFFFFF, 0x00000000},
{0x00000003, 0x80000000, 0x00000000},
{0x00000003, 0xFFFFFFFD, 0x00000000},
{0x00000003, 0xFFFFFFFE, 0x00000000},
{0x00000003, 0xFFFFFFFF, 0x00000000},
{0x00000010, 0x00000001, 0x00000010},
{0x00000010, 0x00000002, 0x00000008},
{0x00000010, 0x00000003, 0x00000005},
{0x00000010, 0x00000010, 0x00000001},
{0x00000010, 0x078644FA, 0x00000000},
{0x00000010, 0x0747AE14, 0x00000000},
{0x00000010, 0x7FFFFFFF, 0x00000000},
{0x00000010, 0x80000000, 0x00000000},
{0x00000010, 0xFFFFFFFD, 0x00000000},
{0x00000010, 0xFFFFFFFE, 0x00000000},
{0x00000010, 0xFFFFFFFF, 0x00000000},
{0x078644FA, 0x00000001, 0x078644FA},
{0x078644FA, 0x00000002, 0x03C3227D},
{0x078644FA, 0x00000003, 0x028216FE},
{0x078644FA, 0x00000010, 0x0078644F},
{0x078644FA, 0x078644FA, 0x00000001},
{0x078644FA, 0x0747AE14, 0x00000001},
{0x078644FA, 0x7FFFFFFF, 0x00000000},
{0x078644FA, 0x80000000, 0x00000000},
{0x078644FA, 0xFFFFFFFD, 0x00000000},
{0x078644FA, 0xFFFFFFFE, 0x00000000},
{0x078644FA, 0xFFFFFFFF, 0x00000000},
{0x0747AE14, 0x00000001, 0x0747AE14},
{0x0747AE14, 0x00000002, 0x03A3D70A},
{0x0747AE14, 0x00000003, 0x026D3A06},
{0x0747AE14, 0x00000010, 0x00747AE1},
{0x0747AE14, 0x078644FA, 0x00000000},
{0x0747AE14, 0x0747AE14, 0x00000001},
{0x0747AE14, 0x7FFFFFFF, 0x00000000},
{0x0747AE14, 0x80000000, 0x00000000},
{0x0747AE14, 0xFFFFFFFD, 0x00000000},
{0x0747AE14, 0xFFFFFFFE, 0x00000000},
{0x0747AE14, 0xFFFFFFFF, 0x00000000},
{0x7FFFFFFF, 0x00000001, 0x7FFFFFFF},
{0x7FFFFFFF, 0x00000002, 0x3FFFFFFF},
{0x7FFFFFFF, 0x00000003, 0x2AAAAAAA},
{0x7FFFFFFF, 0x00000010, 0x07FFFFFF},
{0x7FFFFFFF, 0x078644FA, 0x00000011},
{0x7FFFFFFF, 0x0747AE14, 0x00000011},
{0x7FFFFFFF, 0x7FFFFFFF, 0x00000001},
{0x7FFFFFFF, 0x80000000, 0x00000000},
{0x7FFFFFFF, 0xFFFFFFFD, 0x00000000},
{0x7FFFFFFF, 0xFFFFFFFE, 0x00000000},
{0x7FFFFFFF, 0xFFFFFFFF, 0x00000000},
{0x80000000, 0x00000001, 0x80000000},
{0x80000000, 0x00000002, 0x40000000},
{0x80000000, 0x00000003, 0x2AAAAAAA},
{0x80000000, 0x00000010, 0x08000000},
{0x80000000, 0x078644FA, 0x00000011},
{0x80000000, 0x0747AE14, 0x00000011},
{0x80000000, 0x7FFFFFFF, 0x00000001},
{0x80000000, 0x80000000, 0x00000001},
{0x80000000, 0xFFFFFFFD, 0x00000000},
{0x80000000, 0xFFFFFFFE, 0x00000000},
{0x80000000, 0xFFFFFFFF, 0x00000000},
{0xFFFFFFFD, 0x00000001, 0xFFFFFFFD},
{0xFFFFFFFD, 0x00000002, 0x7FFFFFFE},
{0xFFFFFFFD, 0x00000003, 0x55555554},
{0xFFFFFFFD, 0x00000010, 0x0FFFFFFF},
{0xFFFFFFFD, 0x078644FA, 0x00000022},
{0xFFFFFFFD, 0x0747AE14, 0x00000023},
{0xFFFFFFFD, 0x7FFFFFFF, 0x00000001},
{0xFFFFFFFD, 0x80000000, 0x00000001},
{0xFFFFFFFD, 0xFFFFFFFD, 0x00000001},
{0xFFFFFFFD, 0xFFFFFFFE, 0x00000000},
{0xFFFFFFFD, 0xFFFFFFFF, 0x00000000},
{0xFFFFFFFE, 0x00000001, 0xFFFFFFFE},
{0xFFFFFFFE, 0x00000002, 0x7FFFFFFF},
{0xFFFFFFFE, 0x00000003, 0x55555554},
{0xFFFFFFFE, 0x00000010, 0x0FFFFFFF},
{0xFFFFFFFE, 0x078644FA, 0x00000022},
{0xFFFFFFFE, 0x0747AE14, 0x00000023},
{0xFFFFFFFE, 0x7FFFFFFF, 0x00000002},
{0xFFFFFFFE, 0x80000000, 0x00000001},
{0xFFFFFFFE, 0xFFFFFFFD, 0x00000001},
{0xFFFFFFFE, 0xFFFFFFFE, 0x00000001},
{0xFFFFFFFE, 0xFFFFFFFF, 0x00000000},
{0xFFFFFFFF, 0x00000001, 0xFFFFFFFF},
{0xFFFFFFFF, 0x00000002, 0x7FFFFFFF},
{0xFFFFFFFF, 0x00000003, 0x55555555},
{0xFFFFFFFF, 0x00000010, 0x0FFFFFFF},
{0xFFFFFFFF, 0x078644FA, 0x00000022},
{0xFFFFFFFF, 0x0747AE14, 0x00000023},
{0xFFFFFFFF, 0x7FFFFFFF, 0x00000002},
{0xFFFFFFFF, 0x80000000, 0x00000001},
{0xFFFFFFFF, 0xFFFFFFFD, 0x00000001},
{0xFFFFFFFF, 0xFFFFFFFE, 0x00000001},
{0xFFFFFFFF, 0xFFFFFFFF, 0x00000001}
};

int main()
{
    const unsigned N = sizeof(tests) / sizeof(tests[0]);
    unsigned i;
    for (i = 0; i < N; ++i)
        if (test__udivsi3(tests[i][0], tests[i][1], tests[i][2]))
            return 1;

    return 0;
}