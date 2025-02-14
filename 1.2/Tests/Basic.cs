﻿/*
 * Regression tests for the mono JIT.
 *
 * Each test needs to be of the form:
 *
 * public static int test_<result>_<name> ();
 *
 * where <result> is an integer (the value that needs to be returned by
 * the method to make it pass.
 * <name> is a user-displayed name used to identify the test.
 *
 * The tests can be driven in two ways:
 * *) running the program directly: Main() uses reflection to find and invoke
 * 	the test methods (this is useful mostly to check that the tests are correct)
 * *) with the --regression switch of the jit (this is the preferred way since
 * 	all the tests will be run with optimizations on and off)
 *
 * The reflection logic could be moved to a .dll since we need at least another
 * regression test file written in IL code to have better control on how
 * the IL code looks.
 */

namespace Cortus.TestSuite
{
    class Basic
    {
        static int all = 0;
        static int passed = 0;

        static void Test(bool ok, string name)
        {
            all++;

            if (ok)
            {
                System.Console.Write("Y  : ");
                passed++;
            }
            else
            {
                System.Console.Write("  N: ");
            }

            System.Console.WriteLine(name);
        }

        public static void Process()
        {
            Test(test_0_rem_opt() == 0, "test_0_rem_opt");
            Test(test_0_branch_to_cmov_opt() == 0, "test_0_branch_to_cmov_opt");
            Test(test_1_neg() == 1, "test_1_neg");
            Test(test_2_not() == 2, "test_2_not");
            Test(test_16_shift() == 16, "test_16_shift");
            Test(test_16_shift_add() == 16, "test_16_shift_add");
            Test(test_16_shift_add2() == 16, "test_16_shift_add2");
            Test(test_16_shift_imm() == 16, "test_16_shift_imm");
            Test(test_524288_shift_imm_large() == 524288, "test_524288_shift_imm_large");
            Test(test_12_shift_imm_inv() == 12, "test_12_shift_imm_inv");
            Test(test_12_shift_imm_inv_sbyte() == 12, "test_12_shift_imm_inv_sbyte");
            Test(test_1_rshift_imm() == 1, "test_1_rshift_imm");
            Test(test_2_unrshift_imm() == 2, "test_2_unrshift_imm");
            Test(test_0_bigunrshift_imm() == 0, "test_0_bigunrshift_imm");
            Test(test_0_bigrshift_imm() == 0, "test_0_bigrshift_imm");
            Test(test_1_rshift() == 1, "test_1_rshift");
            Test(test_2_unrshift() == 2, "test_2_unrshift");
            Test(test_0_bigunrshift() == 0, "test_0_bigunrshift");
            Test(test_0_bigrshift() == 0, "test_0_bigrshift");
            Test(test_2_cond() == 2, "test_2_cond");
            Test(test_2_cond_short() == 2, "test_2_cond_short");
            Test(test_2_cond_sbyte() == 2, "test_2_cond_sbyte");
            Test(test_6_cascade_cond() == 6, "test_6_cascade_cond");
            Test(test_6_cascade_short() == 6, "test_6_cascade_short");
            Test(test_0_short_sign_extend() == 0, "test_0_short_sign_extend");
            Test(test_127_iconv_to_i1() == 127, "test_127_iconv_to_i1");
            Test(test_384_iconv_to_i2() == 384, "test_384_iconv_to_i2");
            Test(test_15_for_loop() == 15, "test_15_for_loop");
            Test(test_11_nested_for_loop() == 11, "test_11_nested_for_loop");
            Test(test_11_several_nested_for_loops() == 11, "test_11_several_nested_for_loops");
            Test(test_0_conv_ovf_i1() == 0, "test_0_conv_ovf_i1");
            Test(test_0_conv_ovf_i1_un() == 0, "test_0_conv_ovf_i1_un");
            Test(test_0_conv_ovf_i2() == 0, "test_0_conv_ovf_i2");
            Test(test_0_conv_ovf_i2_un() == 0, "test_0_conv_ovf_i2_un");
            Test(test_0_conv_ovf_u2() == 0, "test_0_conv_ovf_u2");
            Test(test_0_conv_ovf_u2_un() == 0, "test_0_conv_ovf_u2_un");
            Test(test_0_conv_ovf_u4() == 0, "test_0_conv_ovf_u4");
            Test(test_0_conv_ovf_i4_un() == 0, "test_0_conv_ovf_i4_un");
            Test(test_0_bool() == 0, "test_0_bool");
            Test(test_1_bool_inverted() == 1, "test_1_bool_inverted");
            Test(test_1_bool_assign() == 1, "test_1_bool_assign");
            Test(test_1_bool_multi() == 1, "test_1_bool_multi");
            Test(test_16_spill() == 16, "test_16_spill");
            Test(test_1_switch() == 1, "test_1_switch");
            Test(test_0_switch_constprop() == 0, "test_0_switch_constprop");
            Test(test_0_switch_constprop2() == 0, "test_0_switch_constprop2");
            Test(test_0_while_loop_1() == 0, "test_0_while_loop_1");
            Test(test_0_while_loop_2() == 0, "test_0_while_loop_2");
            Test(test_0_char_conv() == 0, "test_0_char_conv");
            Test(test_3_shift_regalloc() == 3, "test_3_shift_regalloc");
            Test(test_2_optimize_branches() == 2, "test_2_optimize_branches");
            Test(test_0_checked_byte_cast() == 0, "test_0_checked_byte_cast");
            Test(test_0_checked_byte_cast_un() == 0, "test_0_checked_byte_cast_un");
            Test(test_0_checked_short_cast() == 0, "test_0_checked_short_cast");
            Test(test_0_checked_short_cast_un() == 0, "test_0_checked_short_cast_un");
            Test(test_1_a_eq_b_plus_a() == 1, "test_1_a_eq_b_plus_a");
            Test(test_0_comp() == 0, "test_0_comp");
            Test(test_0_comp_unsigned() == 0, "test_0_comp_unsigned");
            Test(test_16_cmov() == 16, "test_16_cmov");
            Test(test_0_and_cmp() == 0, "test_0_and_cmp");
            Test(test_0_mul_imm_opt() == 0, "test_0_mul_imm_opt");
            Test(test_0_cne() == 0, "test_0_cne");
            Test(test_0_cmp_regvar_zero() == 0, "test_0_cmp_regvar_zero");
            Test(test_5_div_un_cfold() == 5, "test_5_div_un_cfold");
            Test(test_1_rem_un_cfold() == 1, "test_1_rem_un_cfold");
            Test(test_0_div_opt() == 0, "test_0_div_opt");
            Test(test_0_return() == 0, "test_0_return");
            Test(test_100000_return_large() == 100000, "test_100000_return_large");
            Test(test_1_load_bool() == 1, "test_1_load_bool");
            Test(test_0_load_bool_false() == 0, "test_0_load_bool_false");
            Test(test_200_load_byte() == 200, "test_200_load_byte");
            Test(test_100_load_sbyte() == 100, "test_100_load_sbyte");
            Test(test_200_load_short() == 200, "test_200_load_short");
            Test(test_100_load_ushort() == 100, "test_100_load_ushort");
            Test(test_3_add_simple() == 3, "test_3_add_simple");
            Test(test_3_add_imm() == 3, "test_3_add_imm");
            Test(test_13407573_add_largeimm() == 13407573, "test_13407573_add_largeimm");
            Test(test_1_sub_simple() == 1, "test_1_sub_simple");
            Test(test_1_sub_simple_un() == 1, "test_1_sub_simple_un");
            Test(test_1_sub_imm() == 1, "test_1_sub_imm");
            Test(test_2_sub_large_imm() == 2, "test_2_sub_large_imm");
            Test(test_0_sub_inv_imm() == 0, "test_0_sub_inv_imm");
            Test(test_2_and() == 2, "test_2_and");
            Test(test_0_and_imm() == 0, "test_0_and_imm");
            Test(test_0_and_large_imm() == 0, "test_0_and_large_imm");
            Test(test_0_and_large_imm2() == 0, "test_0_and_large_imm2");
            Test(test_2_div() == 2, "test_2_div");
            Test(test_4_div_imm() == 4, "test_4_div_imm");
            Test(test_4_divun_imm() == 4, "test_4_divun_imm");
            Test(test_0_div_fold() == 0, "test_0_div_fold");
            Test(test_2_div_fold4() == 2, "test_2_div_fold4");
            Test(test_2_div_fold16() == 2, "test_2_div_fold16");
            Test(test_719177_div_destreg() == 719177, "test_719177_div_destreg");
            Test(test_1_remun_imm() == 1, "test_1_remun_imm");
            Test(test_2_bigremun_imm() == 2, "test_2_bigremun_imm");
            Test(test_2_rem() == 2, "test_2_rem");
            Test(test_4_rem_imm() == 4, "test_4_rem_imm");
            Test(test_0_rem_imm_0() == 0, "test_0_rem_imm_0");
            Test(test_4_rem_big_imm() == 4, "test_4_rem_big_imm");
            Test(test_9_mul() == 9, "test_9_mul");
            Test(test_15_mul_imm() == 15, "test_15_mul_imm");
            Test(test_24_mul() == 24, "test_24_mul");
            Test(test_24_mul_ovf() == 24, "test_24_mul_ovf");
            Test(test_24_mul_un() == 24, "test_24_mul_un");
            Test(test_24_mul_ovf_un() == 24, "test_24_mul_ovf_un");
            Test(test_0_add_ovf1() == 0, "test_0_add_ovf1");
            Test(test_0_add_ovf2() == 0, "test_0_add_ovf2");
            Test(test_0_add_ovf3() == 0, "test_0_add_ovf3");
            Test(test_0_add_ovf4() == 0, "test_0_add_ovf4");
            Test(test_0_add_ovf5() == 0, "test_0_add_ovf5");
            Test(test_0_add_ovf6() == 0, "test_0_add_ovf6");
            Test(test_0_add_un_ovf() == 0, "test_0_add_un_ovf");
            Test(test_0_sub_ovf1() == 0, "test_0_sub_ovf1");
            Test(test_0_sub_ovf2() == 0, "test_0_sub_ovf2");
            Test(test_0_sub_ovf3() == 0, "test_0_sub_ovf3");
            Test(test_0_sub_ovf4() == 0, "test_0_sub_ovf4");
            Test(test_0_sub_ovf5() == 0, "test_0_sub_ovf5");
            Test(test_0_sub_ovf6() == 0, "test_0_sub_ovf6");
            Test(test_0_sub_ovf_un() == 0, "test_0_sub_ovf_un");
            Test(test_3_or() == 3, "test_3_or");
            Test(test_3_or_un() == 3, "test_3_or_un");
            Test(test_3_or_short_un() == 3, "test_3_or_short_un");
            Test(test_18_or_imm() == 18, "test_18_or_imm");
            Test(test_268435458_or_large_imm() == 268435458, "test_268435458_or_large_imm");
            Test(test_268435459_or_large_imm2() == 268435459, "test_268435459_or_large_imm2");
            Test(test_1_xor() == 1, "test_1_xor");
            Test(test_1_xor_imm() == 1, "test_1_xor_imm");
            Test(test_983041_xor_imm_large() == 983041, "test_983041_xor_imm_large");

            if (passed == 128)
            {
                System.Console.WriteLine("SUCCESS");
            }
            else
            {
                System.Console.WriteLine("FAIL");
            }

            Program.TotalPassed += passed;
            Program.TotalFailed += 128 - passed;
        }

        public static int test_0_return()
        {
            return 0;
        }

        public static int test_100000_return_large()
        {
            return 100000;
        }

        public static int test_1_load_bool()
        {
            bool a = true;
            return a ? 1 : 0;
        }

        public static int test_0_load_bool_false()
        {
            bool a = false;
            return a ? 1 : 0;
        }

        public static int test_200_load_byte()
        {
            byte a = 200;
            return a;
        }

        public static int test_100_load_sbyte()
        {
            sbyte a = 100;
            return a;
        }

        public static int test_200_load_short()
        {
            short a = 200;
            return a;
        }

        public static int test_100_load_ushort()
        {
            ushort a = 100;
            return a;
        }

        public static int test_3_add_simple()
        {
            int a = 1;
            int b = 2;
            return a + b;
        }

        public static int test_3_add_imm()
        {
            int a = 1;
            return a + 2;
        }

        public static int test_13407573_add_largeimm()
        {
            int a = 1;
            return a + 13407572;
        }

        public static int test_1_sub_simple()
        {
            int a = 1;
            int b = 2;
            return b - a;
        }

        public static int test_1_sub_simple_un()
        {
            uint a = 1;
            uint b = 2;
            return (int)(b - a);
        }

        public static int test_1_sub_imm()
        {
            int b = 2;
            return b - 1;
        }

        public static int test_2_sub_large_imm()
        {
            int b = 0xff0f0f;
            return b - 0xff0f0d;
        }

        public static int test_0_sub_inv_imm()
        {
            int b = 2;
            return 2 - b;
        }

        public static int test_2_and()
        {
            int b = 2;
            int a = 3;
            return b & a;
        }

        public static int test_0_and_imm()
        {
            int b = 2;
            return b & 0x10;
        }

        public static int test_0_and_large_imm()
        {
            int b = 2;
            return b & 0x10000000;
        }

        public static int test_0_and_large_imm2()
        {
            int b = 2;
            return b & 0x100000f0;
        }

        public static int test_2_div()
        {
            int b = 6;
            int a = 3;
            return b / a;
        }

        public static int test_4_div_imm()
        {
            int b = 12;
            return b / 3;
        }

        public static int test_4_divun_imm()
        {
            uint b = 12;
            return (int)(b / 3);
        }

        public static int test_0_div_fold()
        {
            int b = -1;
            return b / 2;
        }

        public static int test_2_div_fold4()
        {
            int b = -8;
            return -(b / 4);
        }

        public static int test_2_div_fold16()
        {
            int b = 32;
            return b / 16;
        }

        public static int test_719177_div_destreg()
        {
            int year = 1970;
            return ((365 * (year - 1)) + ((year - 1) / 4));
        }

        public static int test_1_remun_imm()
        {
            uint b = 13;
            return (int)(b % 3);
        }

        public static int test_2_bigremun_imm()
        {
            unchecked
            {
                uint b = (uint)-2;
                return (int)(b % 3);
            }
        }

        public static int test_2_rem()
        {
            int b = 5;
            int a = 3;
            return b % a;
        }

        public static int test_4_rem_imm()
        {
            int b = 12;
            return b % 8;
        }

        public static int test_0_rem_imm_0()
        {
            int b = 12;
            return b % 1;
        }

        public static int test_4_rem_big_imm()
        {
            int b = 10004;
            return b % 10000;
        }

        public static int test_9_mul()
        {
            int b = 3;
            int a = 3;
            return b * a;
        }

        public static int test_15_mul_imm()
        {
            int b = 3;
            return b * 5;
        }

        public static int test_24_mul()
        {
            int a = 3;
            int b = 8;
            int res;

            res = a * b;

            return res;
        }

        public static int test_24_mul_ovf()
        {
            int a = 3;
            int b = 8;
            int res;

            checked
            {
                res = a * b;
            }

            return res;
        }

        public static int test_24_mul_un()
        {
            uint a = 3;
            uint b = 8;
            uint res;

            res = a * b;

            return (int)res;
        }

        public static int test_24_mul_ovf_un()
        {
            uint a = 3;
            uint b = 8;
            uint res;

            checked
            {
                res = a * b;
            }

            return (int)res;
        }

        public static int test_0_add_ovf1()
        {
            int i, j, k;

            checked
            {
                i = System.Int32.MinValue;
                j = 0;
                k = i + j;
            }

            if (k != System.Int32.MinValue)
                return 1;
            return 0;
        }

        public static int test_0_add_ovf2()
        {
            int i, j, k;

            checked
            {
                i = System.Int32.MaxValue;
                j = 0;
                k = i + j;
            }

            if (k != System.Int32.MaxValue)
                return 2;
            return 0;
        }

        public static int test_0_add_ovf3()
        {
            int i, j, k;

            checked
            {
                i = System.Int32.MinValue;
                j = System.Int32.MaxValue;
                k = i + j;
            }

            if (k != -1)
                return 3;
            return 0;
        }

        public static int test_0_add_ovf4()
        {
            int i, j, k;

            checked
            {
                i = System.Int32.MaxValue;
                j = System.Int32.MinValue;
                k = i + j;
            }

            if (k != -1)
                return 4;
            return 0;
        }

        public static int test_0_add_ovf5()
        {
            int i, j, k;

            checked
            {
                i = System.Int32.MinValue + 1234;
                j = -1234;
                k = i + j;
            }

            if (k != System.Int32.MinValue)
                return 5;
            return 0;
        }

        public static int test_0_add_ovf6()
        {
            int i, j, k;

            checked
            {
                i = System.Int32.MaxValue - 1234;
                j = 1234;
                k = i + j;
            }

            if (k != System.Int32.MaxValue)
                return 6;

            return 0;
        }

        public static int test_0_add_un_ovf()
        {
            uint n = (uint)134217728 * 16;
            uint number = checked(n + (uint)0);

            return number == n ? 0 : 1;
        }

        public static int test_0_sub_ovf1()
        {
            int i, j, k;

            checked
            {
                i = System.Int32.MinValue;
                j = 0;
                k = i - j;
            }

            if (k != System.Int32.MinValue)
                return 1;

            return 0;
        }

        public static int test_0_sub_ovf2()
        {
            int i, j, k;

            checked
            {
                i = System.Int32.MaxValue;
                j = 0;
                k = i - j;
            }

            if (k != System.Int32.MaxValue)
                return 2;

            return 0;
        }

        public static int test_0_sub_ovf3()
        {
            int i, j, k;

            checked
            {
                i = System.Int32.MinValue;
                j = System.Int32.MinValue + 1234;
                k = i - j;
            }

            if (k != -1234)
                return 3;

            return 0;
        }

        public static int test_0_sub_ovf4()
        {
            int i, j, k;

            checked
            {
                i = System.Int32.MaxValue;
                j = 1234;
                k = i - j;
            }

            if (k != System.Int32.MaxValue - 1234)
                return 4;

            return 0;
        }

        public static int test_0_sub_ovf5()
        {
            int i, j, k;

            checked
            {
                i = System.Int32.MaxValue - 1234;
                j = -1234;
                k = i - j;
            }

            if (k != System.Int32.MaxValue)
                return 5;

            return 0;
        }

        public static int test_0_sub_ovf6()
        {
            int i, j, k;

            checked
            {
                i = System.Int32.MinValue + 1234;
                j = 1234;
                k = i - j;
            }

            if (k != System.Int32.MinValue)
                return 6;

            return 0;
        }

        public static int test_0_sub_ovf_un()
        {
            uint i, j, k;

            checked
            {
                i = System.UInt32.MaxValue;
                j = 0;
                k = i - j;
            }

            if (k != System.UInt32.MaxValue)
                return 1;

            checked
            {
                i = System.UInt32.MaxValue;
                j = System.UInt32.MaxValue;
                k = i - j;
            }

            if (k != 0)
                return 2;

            return 0;
        }

        public static int test_3_or()
        {
            int b = 2;
            int a = 3;
            return b | a;
        }

        public static int test_3_or_un()
        {
            uint b = 2;
            uint a = 3;
            return (int)(b | a);
        }

        public static int test_3_or_short_un()
        {
            ushort b = 2;
            ushort a = 3;
            return (int)(b | a);
        }

        public static int test_18_or_imm()
        {
            int b = 2;
            return b | 0x10;
        }

        public static int test_268435458_or_large_imm()
        {
            int b = 2;
            return b | 0x10000000;
        }

        public static int test_268435459_or_large_imm2()
        {
            int b = 2;
            return b | 0x10000001;
        }

        public static int test_1_xor()
        {
            int b = 2;
            int a = 3;
            return b ^ a;
        }

        public static int test_1_xor_imm()
        {
            int b = 2;
            return b ^ 3;
        }

        public static int test_983041_xor_imm_large()
        {
            int b = 2;
            return b ^ 0xf0003;
        }

        public static int test_1_neg()
        {
            int b = -2;
            b++;
            return -b;
        }

        public static int test_2_not()
        {
            int b = ~2;
            b = ~b;
            return b;
        }

        public static int test_16_shift()
        {
            int b = 2;
            int a = 3;
            return b << a;
        }

        public static int test_16_shift_add()
        {
            int b = 2;
            int a = 3;
            int c = 0;
            return b << (a + c);
        }

        public static int test_16_shift_add2()
        {
            int b = 2;
            int a = 3;
            int c = 0;
            return (b + c) << a;
        }

        public static int test_16_shift_imm()
        {
            int b = 2;
            return b << 3;
        }

        public static int test_524288_shift_imm_large()
        {
            int b = 2;
            return b << 18;
        }

        public static int test_12_shift_imm_inv()
        {
            int b = 2;
            return 3 << 2;
        }

        public static int test_12_shift_imm_inv_sbyte()
        {
            sbyte b = 2;
            return 3 << 2;
        }

        public static int test_1_rshift_imm()
        {
            int b = 8;
            return b >> 3;
        }

        public static int test_2_unrshift_imm()
        {
            uint b = 16;
            return (int)(b >> 3);
        }

        public static int test_0_bigunrshift_imm()
        {
            unchecked
            {
                uint b = (uint)-1;
                b = b >> 1;
                if (b != 0x7fffffff)
                    return 1;
                return 0;
            }
        }

        public static int test_0_bigrshift_imm()
        {
            int b = -1;
            b = b >> 1;
            if (b != -1)
                return 1;
            return 0;
        }

        public static int test_1_rshift()
        {
            int b = 8;
            int a = 3;
            return b >> a;
        }

        public static int test_2_unrshift()
        {
            uint b = 16;
            int a = 3;
            return (int)(b >> a);
        }

        public static int test_0_bigunrshift()
        {
            unchecked
            {
                uint b = (uint)-1;
                int a = 1;
                b = b >> a;
                if (b != 0x7fffffff)
                    return 1;
                return 0;
            }
        }

        public static int test_0_bigrshift()
        {
            int b = -1;
            int a = 1;
            b = b >> a;
            if (b != -1)
                return 1;
            return 0;
        }

        public static int test_2_cond()
        {
            int b = 2, a = 3, c;
            if (a == b)
                return 0;
            return 2;
        }

        public static int test_2_cond_short()
        {
            short b = 2, a = 3, c;
            if (a == b)
                return 0;
            return 2;
        }

        public static int test_2_cond_sbyte()
        {
            sbyte b = 2, a = 3, c;
            if (a == b)
                return 0;
            return 2;
        }

        public static int test_6_cascade_cond()
        {
            int b = 2, a = 3, c;
            if (a == b)
                return 0;
            else if (b > a)
                return 1;
            else if (b != b)
                return 2;
            else
            {
                c = 1;
            }
            return a + b + c;
        }

        public static int test_6_cascade_short()
        {
            short b = 2, a = 3, c;
            if (a == b)
                return 0;
            else if (b > a)
                return 1;
            else if (b != b)
                return 2;
            else
            {
                c = 1;
            }
            return a + b + c;
        }

        public static int test_0_short_sign_extend()
        {
            int t1 = 0xffeedd;
            short s1 = (short)t1;
            int t2 = s1;

            if ((uint)t2 != 0xffffeedd)
                return 1;
            else
                return 0;
        }

        public static int test_127_iconv_to_i1()
        {
            int i = 0x100017f;
            sbyte s = (sbyte)i;

            return s;
        }

        public static int test_384_iconv_to_i2()
        {
            int i = 0x1000180;
            short s = (short)i;

            return s;
        }

        public static int test_15_for_loop()
        {
            int i;
            for (i = 0; i < 15; ++i)
            {
            }
            return i;
        }

        public static int test_11_nested_for_loop()
        {
            int i, j = 0; /* mcs bug here if j not set */
            for (i = 0; i < 15; ++i)
            {
                for (j = 200; j >= 5; --j) ;
            }
            return i - j;
        }

        public static int test_11_several_nested_for_loops()
        {
            int i, j = 0; /* mcs bug here if j not set */
            for (i = 0; i < 15; ++i)
            {
                for (j = 200; j >= 5; --j) ;
            }
            i = j = 0;
            for (i = 0; i < 15; ++i)
            {
                for (j = 200; j >= 5; --j) ;
            }
            return i - j;
        }

        public static int test_0_conv_ovf_i1()
        {
            int c;

            //for (int j = 0; j < 10000000; j++)
            checked
            {
                c = 127;
                sbyte b = (sbyte)c;
                c = -128;
                b = (sbyte)c;
            }

            return 0;
        }

        public static int test_0_conv_ovf_i1_un()
        {
            uint c;

            checked
            {
                c = 127;
                sbyte b = (sbyte)c;
            }

            return 0;
        }

        public static int test_0_conv_ovf_i2()
        {
            int c;

            checked
            {
                c = 32767;
                Int16 b = (Int16)c;
                c = -32768;
                b = (Int16)c;
                unchecked
                {
                    uint u = 0xfffffffd;
                    c = (int)u;
                }
                b = (Int16)c;
            }

            return 0;
        }

        public static int test_0_conv_ovf_i2_un()
        {
            uint c;

            checked
            {
                c = 32767;
                Int16 b = (Int16)c;
            }

            return 0;
        }

        public static int test_0_conv_ovf_u2()
        {
            int c;

            checked
            {
                c = 65535;
                UInt16 b = (UInt16)c;
            }

            return 0;
        }

        public static int test_0_conv_ovf_u2_un()
        {
            uint c;

            checked
            {
                c = 65535;
                UInt16 b = (UInt16)c;
            }

            return 0;
        }

        public static int test_0_conv_ovf_u4()
        {
            int c;

            checked
            {
                c = 0x7fffffff;
                uint b = (uint)c;
            }

            return 0;
        }

        public static int test_0_conv_ovf_i4_un()
        {
            uint c;

            checked
            {
                c = 0x7fffffff;
                int b = (int)c;
            }

            return 0;
        }

        public static int test_0_bool()
        {
            bool val = true;
            if (val)
                return 0;
            return 1;
        }

        public static int test_1_bool_inverted()
        {
            bool val = true;
            if (!val)
                return 0;
            return 1;
        }

        public static int test_1_bool_assign()
        {
            bool val = true;
            val = !val; // this should produce a ceq
            if (val)
                return 0;
            return 1;
        }

        public static int test_1_bool_multi()
        {
            bool val = true;
            bool val2 = true;
            val = !val;
            if ((val && !val2) && (!val2 && val))
                return 0;
            return 1;
        }

        public static int test_16_spill()
        {
            int a = 1;
            int b = 2;
            int c = 3;
            int d = 4;
            int e = 5;

            return (1 + (a + (b + (c + (d + e)))));
        }

        public static int test_1_switch()
        {
            int n = 0;

            switch (n)
            {
                case 0: return 1;
                case 1: return 2;
                case -1: return 3;
                default:
                    return 4;
            }
            return 1;
        }

        public static int test_0_switch_constprop()
        {
            int n = -1;

            switch (n)
            {
                case 0: return 2;
                case 1: return 3;
                case 2: return 3;
                default:
                    return 0;
            }
            return 3;
        }

        public static int test_0_switch_constprop2()
        {
            int n = 3;

            switch (n)
            {
                case 0: return 2;
                case 1: return 3;
                case 2: return 3;
                default:
                    return 0;
            }
            return 3;
        }

        public static int test_0_while_loop_1()
        {

            int value = 255;

            do
            {
                value = value >> 4;
            } while (value != 0);

            return 0;
        }

        public static int test_0_while_loop_2()
        {
            int value = 255;
            int position = 5;

            do
            {
                value = value >> 4;
            } while (value != 0 && position > 1);

            return 0;
        }

        public static int test_0_char_conv()
        {
            int i = 1;

            char tc = (char)('0' + i);

            if (tc != '1')
                return 1;

            return 0;
        }

        public static int test_3_shift_regalloc()
        {
            int shift = 8;
            int orig = 1;
            byte value = 0xfe;

            orig &= ~(0xff << shift);
            orig |= value << shift;

            if (orig == 0xfe01)
                return 3;
            return 0;
        }

        enum E { A, B };

        public static int test_2_optimize_branches()
        {
            switch (E.A)
            {
                case E.A:
                    if (E.A == E.B)
                    {
                    }
                    break;
            }
            return 2;
        }

        public static int test_0_checked_byte_cast()
        {
            int v = 250;
            int b = checked((byte)(v));

            if (b != 250)
                return 1;
            return 0;
        }

        public static int test_0_checked_byte_cast_un()
        {
            uint v = 250;
            uint b = checked((byte)(v));

            if (b != 250)
                return 1;
            return 0;
        }

        public static int test_0_checked_short_cast()
        {
            int v = 250;
            int b = checked((ushort)(v));

            if (b != 250)
                return 1;
            return 0;
        }

        public static int test_0_checked_short_cast_un()
        {
            uint v = 250;
            uint b = checked((ushort)(v));

            if (b != 250)
                return 1;
            return 0;
        }

        public static int test_1_a_eq_b_plus_a()
        {
            int a = 0, b = 1;
            a = b + a;
            return a;
        }

        public static int test_0_comp()
        {
            int a = 0;
            int b = -1;
            int error = 1;
            bool val;

            val = a < b;
            if (val)
                return error;
            error++;

            val = a > b;
            if (!val)
                return error;
            error++;

            val = a == b;
            if (val)
                return error;
            error++;

            val = a == a;
            if (!val)
                return error;
            error++;

            return 0;
        }

        public static int test_0_comp_unsigned()
        {
            uint a = 1;
            uint b = 0xffffffff;
            int error = 1;
            bool val;

            val = a < b;
            if (!val)
                return error;
            error++;

            val = a <= b;
            if (!val)
                return error;
            error++;

            val = a == b;
            if (val)
                return error;
            error++;

            val = a >= b;
            if (val)
                return error;
            error++;

            val = a > b;
            if (val)
                return error;
            error++;

            val = b < a;
            if (val)
                return error;
            error++;

            val = b <= a;
            if (val)
                return error;
            error++;

            val = b == a;
            if (val)
                return error;
            error++;

            val = b > a;
            if (!val)
                return error;
            error++;

            val = b >= a;
            if (!val)
                return error;
            error++;

            return 0;
        }

        public static int test_16_cmov()
        {
            int n = 0;
            if (n == 0)
                n = 16;

            return n;
        }

        public static int test_0_and_cmp()
        {
            /* test esi, imm */
            int local = 0x01020304;

            if ((local & 0x01020304) == 0)
                return 7;

            if ((local & 0x00000304) == 0)
                return 8;

            if ((local & 0x00000004) == 0)
                return 9;

            if ((local & 0x00000300) == 0)
                return 10;

            if ((local & 0x00020000) == 0)
                return 11;

            if ((local & 0x01000000) == 0)
                return 12;

            return 0;
        }

        public static int test_0_mul_imm_opt()
        {
            int i;

            i = 1;
            if ((i * 2) != 2)
                return 1;
            i = -1;
            if ((i * 2) != -2)
                return 2;
            i = 1;
            if ((i * 3) != 3)
                return 3;
            i = -1;
            if ((i * 3) != -3)
                return 4;
            i = 1;
            if ((i * 5) != 5)
                return 5;
            i = -1;
            if ((i * 5) != -5)
                return 6;
            i = 1;
            if ((i * 6) != 6)
                return 7;
            i = -1;
            if ((i * 6) != -6)
                return 8;
            i = 1;
            if ((i * 9) != 9)
                return 9;
            i = -1;
            if ((i * 9) != -9)
                return 10;
            i = 1;
            if ((i * 10) != 10)
                return 11;
            i = -1;
            if ((i * 10) != -10)
                return 12;
            i = 1;
            if ((i * 12) != 12)
                return 13;
            i = -1;
            if ((i * 12) != -12)
                return 14;
            i = 1;
            if ((i * 25) != 25)
                return 15;
            i = -1;
            if ((i * 25) != -25)
                return 16;
            i = 1;
            if ((i * 100) != 100)
                return 17;
            i = -1;
            if ((i * 100) != -100)
                return 18;

            return 0;
        }

        public static int test_0_cne()
        {
            int x = 0;
            int y = 1;

            bool b = x != y;
            bool bb = x != x;

            if (!b)
                return 1;
            if (bb)
                return 2;

            return 0;
        }

        public static int test_0_cmp_regvar_zero()
        {
            int n = 10;

            if (!(n > 0 && n >= 0 && n != 0))
                return 1;
            if (n < 0 || n <= 0 || n == 0)
                return 1;

            return 0;
        }

        public static int test_5_div_un_cfold()
        {
            uint i = 10;
            uint j = 2;

            return (int)(i / j);
        }

        public static int test_1_rem_un_cfold()
        {
            uint i = 11;
            uint j = 2;

            return (int)(i % j);
        }

        public static int test_0_div_opt()
        {
            int i;

            // Avoid cfolding this
            i = 0;
            for (int j = 0; j < 1234567; ++j)
                i++;
            if ((i / 2) != 617283)
                return 1;
            if ((i / 4) != 308641)
                return 2;
            if ((i / 8) != 154320)
                return 3;
            if ((i / 16) != 77160)
                return 4;

            // Avoid cfolding this
            i = 0;
            for (int j = 0; j < 1234567; ++j)
                i--;
            if ((i / 2) != -617283)
                return 5;
            if ((i / 4) != -308641)
                return 6;
            if ((i / 8) != -154320)
                return 7;
            if ((i / 16) != -77160)
                return 8;

            return 0;
        }

        public static int test_0_rem_opt()
        {
            int i;

            // Avoid cfolding this
            i = 0;
            for (int j = 0; j < 29; ++j)
                i++;
            if ((i % 2) != 1)
                return 1;
            if ((i % 4) != 1)
                return 2;
            if ((i % 8) != 5)
                return 3;
            if ((i % 16) != 13)
                return 4;

            // Avoid cfolding this
            i = 0;
            for (int j = 0; j < 29; ++j)
                i--;
            if ((i % 2) != -1)
                return 5;
            if ((i % 4) != -1)
                return 6;
            if ((i % 8) != -5)
                return 7;
            if ((i % 16) != -13)
                return 8;

            return 0;
        }

        public static int cmov(int i)
        {
            int j = 0;

            if (i > 0)
                j = 1;

            return j;
        }

        public static int cmov2(int i)
        {
            int j = 0;

//            #pragma warning disable 0642
            if (i <= 0)
                ;
            else
                j = 1;
//            #pragma warning restore 0642

            return j;
        }

        public static int test_0_branch_to_cmov_opt()
        {
            if (cmov(0) != 0)
                return 1;
            if (cmov(1) != 1)
                return 2;
            if (cmov2(0) != 0)
                return 1;
            if (cmov2(1) != 1)
                return 2;
            return 0;
        }
    }

}
