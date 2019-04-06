#![warn(clippy::large_digit_groups)]
#![warn(clippy::mixed_case_hex_literals)]
#![warn(clippy::unseparated_literal_suffix)]
#![warn(clippy::zero_prefixed_literal)]
#![allow(dead_code)]

fn main() {
    let ok1 = 0xABCD;
    let ok3 = 0xab_cd;
    let ok4 = 0xab_cd_i32;
    let ok5 = 0xAB_CD_u32;
    let ok5 = 0xAB_CD_isize;
    let fail1 = 0xabCD;
    let fail2 = 0xabCD_u32;
    let fail2 = 0xabCD_isize;
    let fail_multi_zero = 000_123usize;

    let ok6 = 1234_i32;
    let ok7 = 1234_f32;
    let ok8 = 1234_isize;
    let fail3 = 1234i32;
    let fail4 = 1234u32;
    let fail5 = 1234isize;
    let fail6 = 1234usize;
    let fail7 = 1.5f32;

    let ok9 = 0;
    let ok10 = 0_i64;
    let fail8 = 0123;

    let ok11 = 0o123;
    let ok12 = 0b10_1010;

    let ok13 = 0xab_abcd;
    let ok14 = 0xBAFE_BAFE;
    let ok15 = 0xab_cabc_abca_bcab_cabc;
    let ok16 = 0xFE_BAFE_ABAB_ABCD;
    let ok17 = 0x123_4567_8901_usize;

    let fail9 = 0xabcdef;
    let fail10 = 0xBAFEBAFE;
    let fail11 = 0xabcdeff;
    let fail12 = 0xabcabcabcabcabcabc;
    let fail13 = 0x1_23456_78901_usize;

    let fail14 = 2_32;
    let fail15 = 4_64;
    let fail16 = 7_8;
    let fail17 = 23_16;
    let ok18 = 23_128;
    let fail19 = 12_3456_21;
    let fail20 = 2__8;
    let fail21 = 4___16;
    let fail22 = 3__4___23;
    let fail23 = 3__16___23;

    let fail24 = 12.34_64;
    let fail25 = 1E2_32;
    let fail26 = 43E7_64;
    let fail27 = 243E17_32;
    let fail28 = 241251235E723_64;
    let fail29 = 42279.911_32;
}
