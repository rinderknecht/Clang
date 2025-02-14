﻿using System;

namespace Cortus.TestSuite
{
    class Exceptions
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
            Test(test_0_stack_unwind() == 0, "test_0_stack_unwind");
            Test(test_0_throw_unwind() == 0, "test_0_throw_unwind");
            Test(test_0_regress_73242() == 0, "test_0_regress_73242");
            Test(test_0_nullref() == 0, "test_0_nullref");
            Test(test_0_nonvirt_nullref_at_clause_start() == 0, "test_0_nonvirt_nullref_at_clause_start");
            Test(test_0_inline_throw_only_gettext() == 0, "test_0_inline_throw_only_gettext");
            Test(test_0_inline_throw() == 0, "test_0_inline_throw");
            Test(test_8_local_deadce_causes() == 8, "test_8_local_deadce_causes");
            Test(test_0_except_opt_two_clauses() == 0, "test_0_except_opt_two_clauses");
            Test(test_100_long_vars_in_clauses_initlocals_opt() == 100, "test_100_long_vars_in_clauses_initlocals_opt");
            Test(test_0_ldflda_null() == 0, "test_0_ldflda_null");
            Test(test_0_ldflda_null_pointer() == 0, "test_0_ldflda_null_pointer");
            Test(test_0_catch() == 0, "test_0_catch");
            Test(test_0_simple_double_casts() == 0, "test_0_simple_double_casts");
            Test(test_0_div_zero() == 0, "test_0_div_zero");
            Test(test_0_cfold_div_zero() == 0, "test_0_cfold_div_zero");
            Test(test_0_udiv_zero() == 0, "test_0_udiv_zero");
            Test(test_0_ulong_div_zero() == 0, "test_0_ulong_div_zero");
            Test(test_0_float_div_zero() == 0, "test_0_float_div_zero");
            Test(test_0_invalid_unbox() == 0, "test_0_invalid_unbox");
            Test(test_0_rethrow_nested() == 0, "test_0_rethrow_nested");
            Test(test_1_array_mismatch_2() == 1, "test_1_array_mismatch_2");
            Test(test_1_array_mismatch_3() == 1, "test_1_array_mismatch_3");
            Test(test_1_array_mismatch_4() == 1, "test_1_array_mismatch_4");
            Test(test_0_throw_to_branch_opt_outer_clause() == 0, "test_0_throw_to_branch_opt_outer_clause");
            Test(test_0_try_inside_finally_cmov_opt() == 0, "test_0_try_inside_finally_cmov_opt");
            Test(test_0_finally_without_exc() == 0, "test_0_finally_without_exc");
            Test(test_0_finally() == 0, "test_0_finally");
            Test(test_0_nested_finally() == 0, "test_0_nested_finally");
            Test(test_2_multiple_finally_clauses() == 2, "test_2_multiple_finally_clauses");
            Test(test_5_regalloc() == 5, "test_5_regalloc");
            Test(test_0_invalid_unbox_arrays() == 0, "test_0_invalid_unbox_arrays");

            // Arithmetic overflow

            Test(test_0_byte_cast() == 0, "test_0_byte_cast");
            Test(test_0_sbyte_cast() == 0, "test_0_sbyte_cast");
            Test(test_0_ushort_cast() == 0, "test_0_ushort_cast");
            Test(test_0_short_cast() == 0, "test_0_short_cast");
            Test(test_0_int_cast() == 0, "test_0_int_cast");
            Test(test_0_uint_cast() == 0, "test_0_uint_cast");
            Test(test_0_long_cast() == 0, "test_0_long_cast");
            Test(test_0_ulong_cast() == 0, "test_0_ulong_cast");
            Test(test_3_checked_cast_un() == 3, "test_3_checked_cast_un");
            Test(test_4_checked_cast() == 4, "test_4_checked_cast");
            Test(test_0_long_div_zero() == 0, "test_0_long_div_zero");
            Test(test_0_ovf1() == 0, "test_0_ovf1");
            Test(test_1_ovf2() == 1, "test_1_ovf2");
            Test(test_0_ovf3() == 0, "test_0_ovf3");
            Test(test_1_ovf4() == 1, "test_1_ovf4");
            Test(test_0_ovf5() == 0, "test_0_ovf5");
            Test(test_1_ovf6() == 1, "test_1_ovf6");
            Test(test_0_ovf7() == 0, "test_0_ovf7");
            Test(test_1_ovf8() == 1, "test_1_ovf8");
            Test(test_0_ovf9() == 0, "test_0_ovf9");
            Test(test_1_ovf10() == 1, "test_1_ovf10");
            Test(test_0_ovf11() == 0, "test_0_ovf11");
            Test(test_1_ovf12() == 1, "test_1_ovf12");
            Test(test_0_ovf13() == 0, "test_0_ovf13");
            Test(test_1_ovf14() == 1, "test_1_ovf14");
            Test(test_0_ovf15() == 0, "test_0_ovf15");
            Test(test_1_ovf16() == 1, "test_1_ovf16");
            Test(test_0_ovf17() == 0, "test_0_ovf17");
            Test(test_0_ovf18() == 0, "test_0_ovf18");
            Test(test_1_ovf19() == 1, "test_1_ovf19");
            Test(test_0_ovf20() == 0, "test_0_ovf20");
            Test(test_1_ovf21() == 1, "test_1_ovf21");
            Test(test_1_ovf22() == 1, "test_1_ovf22");
            Test(test_1_ovf23() == 1, "test_1_ovf23");

            if (passed == 66)
            {
                System.Console.WriteLine("SUCCESS");
            }
            else
            {
                System.Console.WriteLine("FAIL");
            }

            Program.TotalPassed += passed;
            Program.TotalFailed += 66 - passed;
        }

        public static int test_0_catch()
        {
            Exception x = new Exception();

            try
            {
                throw x;
            }
            catch (Exception e)
            {
                if (e == x)
                    return 0;
            }
            return 1;
        }

        public static int test_0_finally_without_exc()
        {
            int x;

            try
            {
                x = 1;
            }
            catch (Exception e)
            {
                x = 2;
            }
            finally
            {
                x = 0;
            }

            return x;
        }

        public static int test_0_finally()
        {
            int x = 1;

            try
            {
                throw new Exception();
            }
            catch (Exception e)
            {
                x = 2;
            }
            finally
            {
                x = 0;
            }
            return x;
        }

        public static int test_0_nested_finally()
        {
            int a;

            try
            {
                a = 1;
            }
            finally
            {
                try
                {
                    a = 2;
                }
                finally
                {
                    a = 0;
                }
            }
            return a;
        }

        public static int test_0_byte_cast()
        {
            int a;
            long l;
            ulong ul;
            byte b = 0;
            bool failed;

            try
            {
                a = 255;
                failed = false;
                checked
                {
                    b = (byte)a;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 1;
            if (b != 255)
                return -1;

            try
            {
                a = 0;
                failed = false;
                checked
                {
                    b = (byte)a;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 2;
            if (b != 0)
                return -2;


            try
            {
                a = 256;
                failed = true;
                checked
                {
                    b = (byte)a;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 3;
            if (b != 0)
                return -3;

            try
            {
                a = -1;
                failed = true;
                checked
                {
                    b = (byte)a;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 4;
            if (b != 0)
                return -4;

            try
            {
                double d = 0;
                failed = false;
                checked
                {
                    b = (byte)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 5;
            if (b != 0)
                return -5;

            try
            {
                double d = -1;
                failed = true;
                checked
                {
                    b = (byte)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 6;
            if (b != 0)
                return -6;

            try
            {
                double d = 255;
                failed = false;
                checked
                {
                    b = (byte)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 7;
            if (b != 255)
                return -7;

            try
            {
                double d = 256;
                failed = true;
                checked
                {
                    b = (byte)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 8;
            if (b != 255)
                return -8;

            try
            {
                l = 255;
                failed = false;
                checked
                {
                    b = (byte)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 9;
            if (b != 255)
                return -9;

            try
            {
                l = 0;
                failed = false;
                checked
                {
                    b = (byte)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 10;
            if (b != 0)
                return -10;

            try
            {
                l = 256;
                failed = true;
                checked
                {
                    b = (byte)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 11;
            if (b != 0)
                return -11;

            try
            {
                l = -1;
                failed = true;
                checked
                {
                    b = (byte)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 12;
            if (b != 0)
                return -12;

            try
            {
                ul = 256;
                failed = true;
                checked
                {
                    b = (byte)ul;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 13;
            if (b != 0)
                return -13;

            return 0;
        }

        public static int test_0_sbyte_cast()
        {
            int a;
            long l;
            sbyte b = 0;
            bool failed;

            try
            {
                a = 255;
                failed = true;
                checked
                {
                    b = (sbyte)a;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 1;
            if (b != 0)
                return -1;

            try
            {
                a = 0;
                failed = false;
                checked
                {
                    b = (sbyte)a;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 2;
            if (b != 0)
                return -2;

            try
            {
                a = 256;
                failed = true;
                checked
                {
                    b = (sbyte)a;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 3;
            if (b != 0)
                return -3;

            try
            {
                a = -129;
                failed = true;
                checked
                {
                    b = (sbyte)a;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 4;
            if (b != 0)
                return -4;

            try
            {
                a = -1;
                failed = false;
                checked
                {
                    b = (sbyte)a;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 5;
            if (b != -1)
                return -5;

            try
            {
                a = -128;
                failed = false;
                checked
                {
                    b = (sbyte)a;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 6;
            if (b != -128)
                return -6;

            try
            {
                a = 127;
                failed = false;
                checked
                {
                    b = (sbyte)a;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 7;
            if (b != 127)
                return -7;

            try
            {
                a = 128;
                failed = true;
                checked
                {
                    b = (sbyte)a;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 8;
            if (b != 127)
                return -8;

            try
            {
                double d = 127;
                failed = false;
                checked
                {
                    b = (sbyte)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 9;
            if (b != 127)
                return -9;

            try
            {
                double d = -128;
                failed = false;
                checked
                {
                    b = (sbyte)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 10;
            if (b != -128)
                return -10;

            try
            {
                double d = 128;
                failed = true;
                checked
                {
                    b = (sbyte)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 11;
            if (b != -128)
                return -11;

            try
            {
                double d = -129;
                failed = true;
                checked
                {
                    b = (sbyte)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 12;
            if (b != -128)
                return -12;

            try
            {
                l = 255;
                failed = true;
                checked
                {
                    b = (sbyte)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 13;
            if (b != -128)
                return -13;

            try
            {
                l = 0;
                failed = false;
                checked
                {
                    b = (sbyte)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 14;
            if (b != 0)
                return -14;

            try
            {
                l = 256;
                failed = true;
                checked
                {
                    b = (sbyte)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 15;
            if (b != 0)
                return -15;

            try
            {
                l = -129;
                failed = true;
                checked
                {
                    b = (sbyte)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 16;
            if (b != 0)
                return -16;

            try
            {
                l = -1;
                failed = false;
                checked
                {
                    b = (sbyte)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 17;
            if (b != -1)
                return -17;

            try
            {
                l = -128;
                failed = false;
                checked
                {
                    b = (sbyte)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 18;
            if (b != -128)
                return -18;

            try
            {
                l = 127;
                failed = false;
                checked
                {
                    b = (sbyte)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 19;
            if (b != 127)
                return -19;

            try
            {
                l = 128;
                failed = true;
                checked
                {
                    b = (sbyte)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 20;
            if (b != 127)
                return -20;

            try
            {
                ulong ul = 128;
                failed = true;
                checked
                {
                    b = (sbyte)ul;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 21;
            if (b != 127)
                return -21;

            return 0;
        }

        public static int test_0_ushort_cast()
        {
            int a;
            long l;
            ulong ul;
            ushort b;
            bool failed;

            try
            {
                a = System.UInt16.MaxValue;
                failed = false;
                checked
                {
                    b = (ushort)a;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 1;

            try
            {
                a = 0;
                failed = false;
                checked
                {
                    b = (ushort)a;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 2;

            try
            {
                a = System.UInt16.MaxValue + 1;
                failed = true;
                checked
                {
                    b = (ushort)a;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 3;

            try
            {
                a = -1;
                failed = true;
                checked
                {
                    b = (ushort)a;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 4;

            try
            {
                double d = 0;
                failed = false;
                checked
                {
                    b = (ushort)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 5;

            try
            {
                double d = System.UInt16.MaxValue;
                failed = false;
                checked
                {
                    b = (ushort)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 6;

            try
            {
                double d = -1;
                failed = true;
                checked
                {
                    b = (ushort)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 7;

            try
            {
                double d = System.UInt16.MaxValue + 1.0;
                failed = true;
                checked
                {
                    b = (ushort)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 8;

            try
            {
                l = System.UInt16.MaxValue;
                failed = false;
                checked
                {
                    b = (ushort)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 9;

            try
            {
                l = 0;
                failed = false;
                checked
                {
                    b = (ushort)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 10;

            try
            {
                l = System.UInt16.MaxValue + 1;
                failed = true;
                checked
                {
                    b = (ushort)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 11;

            try
            {
                l = -1;
                failed = true;
                checked
                {
                    b = (ushort)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 12;

            try
            {
                ul = 0xfffff;
                failed = true;
                checked
                {
                    b = (ushort)ul;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 13;

            return 0;
        }

        public static int test_0_short_cast()
        {
            int a;
            long l;
            short b;
            bool failed;

            try
            {
                a = System.UInt16.MaxValue;
                failed = true;
                checked
                {
                    b = (short)a;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 1;

            try
            {
                a = 0;
                failed = false;
                checked
                {
                    b = (short)a;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 2;

            try
            {
                a = System.Int16.MaxValue + 1;
                failed = true;
                checked
                {
                    b = (short)a;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 3;

            try
            {
                a = System.Int16.MinValue - 1;
                failed = true;
                checked
                {
                    b = (short)a;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 4;

            try
            {
                a = -1;
                failed = false;
                checked
                {
                    b = (short)a;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 5;

            try
            {
                a = System.Int16.MinValue;
                failed = false;
                checked
                {
                    b = (short)a;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 6;

            try
            {
                a = System.Int16.MaxValue;
                failed = false;
                checked
                {
                    b = (short)a;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 7;

            try
            {
                a = System.Int16.MaxValue + 1;
                failed = true;
                checked
                {
                    b = (short)a;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 8;

            try
            {
                double d = System.Int16.MaxValue;
                failed = false;
                checked
                {
                    b = (short)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 9;

            try
            {
                double d = System.Int16.MinValue;
                failed = false;
                checked
                {
                    b = (short)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 10;

            try
            {
                double d = System.Int16.MaxValue + 1.0;
                failed = true;
                checked
                {
                    b = (short)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 11;

            try
            {
                double d = System.Int16.MinValue - 1.0;
                failed = true;
                checked
                {
                    b = (short)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 12;

            try
            {
                l = System.Int16.MaxValue + 1;
                failed = true;
                checked
                {
                    b = (short)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 13;

            try
            {
                l = System.Int16.MaxValue;
                failed = false;
                checked
                {
                    b = (short)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 14;

            try
            {
                l = System.Int16.MinValue - 1;
                failed = true;
                checked
                {
                    b = (short)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 15;


            try
            {
                l = System.Int16.MinValue;
                failed = false;
                checked
                {
                    b = (short)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 16;

            try
            {
                l = 0x00000000ffffffff;
                failed = true;
                checked
                {
                    b = (short)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 17;

            try
            {
                ulong ul = 32768;
                failed = true;
                checked
                {
                    b = (short)ul;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 18;

            return 0;
        }

        public static int test_0_int_cast()
        {
            int a;
            long l;
            bool failed;

            try
            {
                double d = System.Int32.MaxValue + 1.0;
                failed = true;
                checked
                {
                    a = (int)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 1;

            try
            {
                double d = System.Int32.MaxValue;
                failed = false;
                checked
                {
                    a = (int)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 2;


            try
            {
                double d = System.Int32.MinValue;
                failed = false;
                checked
                {
                    a = (int)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 3;


            try
            {
                double d = System.Int32.MinValue - 1.0;
                failed = true;
                checked
                {
                    a = (int)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 4;

            try
            {
                l = System.Int32.MaxValue + (long)1;
                failed = true;
                checked
                {
                    a = (int)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 5;

            try
            {
                l = System.Int32.MaxValue;
                failed = false;
                checked
                {
                    a = (int)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 6;


            try
            {
                l = System.Int32.MinValue;
                failed = false;
                checked
                {
                    a = (int)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 7;


            try
            {
                l = System.Int32.MinValue - (long)1;
                failed = true;
                checked
                {
                    a = (int)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 8;

            try
            {
                uint ui = System.UInt32.MaxValue;
                failed = true;
                checked
                {
                    a = (int)ui;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 9;

            try
            {
                ulong ul = (long)(System.Int32.MaxValue) + 1;
                failed = true;
                checked
                {
                    a = (int)ul;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 10;

            try
            {
                ulong ul = UInt64.MaxValue;
                failed = true;
                checked
                {
                    a = (int)ul;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 11;

            {
                int i;
                float f = 1.1f;
                checked
                {
                    i = (int)f;
                }
            }

            return 0;
        }

        public static int test_0_uint_cast()
        {
            uint a;
            long l;
            bool failed;

            try
            {
                double d = System.UInt32.MaxValue;
                failed = false;
                checked
                {
                    a = (uint)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 1;

            try
            {
                double d = System.UInt32.MaxValue + 1.0;
                failed = true;
                checked
                {
                    a = (uint)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 2;

            try
            {
                double d = System.UInt32.MinValue;
                failed = false;
                checked
                {
                    a = (uint)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 3;

            try
            {
                double d = System.UInt32.MinValue - 1.0;
                failed = true;
                checked
                {
                    a = (uint)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 4;

            try
            {
                l = System.UInt32.MaxValue;
                failed = false;
                checked
                {
                    a = (uint)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 5;

            try
            {
                l = System.UInt32.MaxValue + (long)1;
                failed = true;
                checked
                {
                    a = (uint)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 6;

            try
            {
                l = System.UInt32.MinValue;
                failed = false;
                checked
                {
                    a = (uint)l;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 7;

            try
            {
                l = System.UInt32.MinValue - (long)1;
                failed = true;
                checked
                {
                    a = (uint)l;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 8;

            try
            {
                int i = -1;
                failed = true;
                checked
                {
                    a = (uint)i;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 9;

            {
                uint i;
                float f = 1.1f;
                checked
                {
                    i = (uint)f;
                }
            }

            return 0;
        }

        public static int test_0_long_cast()
        {

            /*
             * These tests depend on properties of x86 fp arithmetic so they won't work
             * on other platforms.
             */
            /*
            long a;
            bool failed;

            try {
                double d = System.Int64.MaxValue - 512.0;
                failed = true;
                checked {
                    a = (long)d;
                }
            } catch (OverflowException) {
                failed = false;
            }
            if (failed)
                return 1;

            try {
                double d = System.Int64.MaxValue - 513.0;
                failed = false;
                checked {
                    a = (long)d;
                }
            } catch (OverflowException) {
                failed = true;
            }
            if (failed)
                return 2;
		
            try {
                double d = System.Int64.MinValue - 1024.0;
                failed = false;			
                checked {
                    a = (long)d;
                }
            } catch (OverflowException) {
                failed = true;
            }
            if (failed)
                return 3;

            try {
                double d = System.Int64.MinValue - 1025.0;
                failed = true;
                checked {
                    a = (long)d;
                }
            } catch (OverflowException) {
                failed = false;
            }
            if (failed)
                return 4;
            */

            {
                long i;
                float f = 1.1f;
                checked
                {
                    i = (long)f;
                }
            }

            return 0;
        }

        public static int test_0_ulong_cast()
        {
            ulong a;
            bool failed;

            /*
             * These tests depend on properties of x86 fp arithmetic so they won't work
             * on other platforms.
             */

            /*
            try {
                double d = System.UInt64.MaxValue - 1024.0;
                failed = true;
                checked {
                    a = (ulong)d;
                }
            } catch (OverflowException) {
                failed = false;
            }
            if (failed)
                return 1;

            try {
                double d = System.UInt64.MaxValue - 1025.0;
                failed = false;
                checked {
                    a = (ulong)d;
                }
            } catch (OverflowException) {
                failed = true;
            }
            if (failed)
                return 2;
            */

            try
            {
                double d = 0;
                failed = false;
                checked
                {
                    a = (ulong)d;
                }
            }
            catch (OverflowException)
            {
                failed = true;
            }
            if (failed)
                return 3;

            try
            {
                double d = -1;
                failed = true;
                checked
                {
                    a = (ulong)d;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 4;

            {
                ulong i;
                float f = 1.1f;
                checked
                {
                    i = (ulong)f;
                }
            }

            try
            {
                int i = -1;
                failed = true;
                checked
                {
                    a = (ulong)i;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 5;

            try
            {
                int i = Int32.MinValue;
                failed = true;
                checked
                {
                    a = (ulong)i;
                }
            }
            catch (OverflowException)
            {
                failed = false;
            }
            if (failed)
                return 6;

            return 0;
        }

        public static int test_0_simple_double_casts()
        {

            double d = 0xffffffff;

            if ((uint)d != 4294967295)
                return 1;

            /*
             * These tests depend on properties of x86 fp arithmetic so they won't work
             * on other platforms.
             */
            /*
            d = 0xffffffffffffffff;

            if ((ulong)d != 0)
                return 2;

            if ((ushort)d != 0)
                return 3;
			
            if ((byte)d != 0)
                return 4;
            */

            d = 0xffff;

            if ((ushort)d != 0xffff)
                return 5;

            if ((byte)d != 0xff)
                return 6;

            return 0;
        }

        public static int test_0_div_zero()
        {
            int d = 1;
            int q = 0;
            int val;
            bool failed;

            try
            {
                failed = true;
                val = d / q;
            }
            catch (DivideByZeroException)
            {
                failed = false;
            }
            if (failed)
                return 1;

            try
            {
                failed = true;
                val = d % q;
            }
            catch (DivideByZeroException)
            {
                failed = false;
            }
            if (failed)
                return 2;

            try
            {
                failed = true;
                q = -1;
                d = Int32.MinValue;
                val = d / q;
            }
            catch (DivideByZeroException)
            {
                /* wrong exception */
            }
            catch (ArithmeticException)
            {
                failed = false;
            }
            if (failed)
                return 3;

            try
            {
                failed = true;
                q = -1;
                d = Int32.MinValue;
                val = d % q;
            }
            catch (DivideByZeroException)
            {
                /* wrong exception */
            }
            catch (ArithmeticException)
            {
                failed = false;
            }
            if (failed)
                return 4;

            return 0;
        }

        public static int return_55()
        {
            return 55;
        }

        public static int test_0_cfold_div_zero()
        {
            // Test that constant folding doesn't cause division by zero exceptions
            if (return_55() != return_55())
            {
                int d = 1;
                int q = 0;
                int val;

                val = d / q;
                val = d % q;

                q = -1;
                d = Int32.MinValue;
                val = d / q;

                q = -1;
                val = d % q;
            }

            return 0;
        }

        public static int test_0_udiv_zero()
        {
            uint d = 1;
            uint q = 0;
            uint val;
            bool failed;

            try
            {
                failed = true;
                val = d / q;
            }
            catch (DivideByZeroException)
            {
                failed = false;
            }
            if (failed)
                return 1;

            try
            {
                failed = true;
                val = d % q;
            }
            catch (DivideByZeroException)
            {
                failed = false;
            }
            if (failed)
                return 2;

            return 0;
        }

        public static int test_0_long_div_zero()
        {
            long d = 1;
            long q = 0;
            long val;
            bool failed;

            try
            {
                failed = true;
                val = d / q;
            }
            catch (DivideByZeroException)
            {
                failed = false;
            }
            if (failed)
                return 1;

            try
            {
                failed = true;
                val = d % q;
            }
            catch (DivideByZeroException)
            {
                failed = false;
            }
            if (failed)
                return 2;

            try
            {
                failed = true;
                q = -1;
                d = Int64.MinValue;
                val = d / q;
            }
            catch (DivideByZeroException)
            {
                /* wrong exception */
            }
            catch (ArithmeticException)
            {
                failed = false;
            }
            if (failed)
                return 3;

            try
            {
                failed = true;
                q = -1;
                d = Int64.MinValue;
                val = d % q;
            }
            catch (DivideByZeroException)
            {
                /* wrong exception */
            }
            catch (ArithmeticException)
            {
                failed = false;
            }
            if (failed)
                return 4;

            return 0;
        }

        public static int test_0_ulong_div_zero()
        {
            ulong d = 1;
            ulong q = 0;
            ulong val;
            bool failed;

            try
            {
                failed = true;
                val = d / q;
            }
            catch (DivideByZeroException)
            {
                failed = false;
            }
            if (failed)
                return 1;

            try
            {
                failed = true;
                val = d % q;
            }
            catch (DivideByZeroException)
            {
                failed = false;
            }
            if (failed)
                return 2;

            return 0;
        }

        public static int test_0_float_div_zero()
        {
            double d = 1;
            double q = 0;
            double val;
            bool failed;

            try
            {
                failed = false;
                val = d / q;
            }
            catch (DivideByZeroException)
            {
                failed = true;
            }
            if (failed)
                return 1;

            try
            {
                failed = false;
                val = d % q;
            }
            catch (DivideByZeroException)
            {
                failed = true;
            }
            if (failed)
                return 2;

            return 0;
        }

        public static int test_0_invalid_unbox()
        {

            int i = 123;
            object o = "Some string";
            int res = 1;

            try
            {
                // Illegal conversion; o contains a string not an int
                i = (int)o;
            }
            catch (Exception e)
            {
                if (i == 123)
                    res = 0;
            }

            return res;
        }

        // Test that double[] can't be cast to double (bug #46027)
        public static int test_0_invalid_unbox_arrays()
        {
            double[] d1 = { 1.0 };
            double[][] d2 = { d1 };
            Array a = d2;

            try
            {
                foreach (double d in a)
                {
                }
                return 1;
            }
            catch (InvalidCastException e)
            {
                return 0;
            }
        }

        /* bug# 42190, at least mcs generates a leave for the return that
         * jumps out of multiple exception clauses: we used to execute just 
         * one enclosing finally block.
         */
        public static int finally_level;
        static void do_something()
        {
            int a = 0;
            try
            {
                try
                {
                    return;
                }
                finally
                {
                    a = 1;
                }
            }
            finally
            {
                finally_level++;
            }
        }

        public static int test_2_multiple_finally_clauses()
        {
            finally_level = 0;
            do_something();
            if (finally_level == 1)
                return 2;
            return 0;
        }

        public static int test_3_checked_cast_un()
        {
            ulong i = 0x8000000034000000;
            long j;

            try
            {
                checked { j = (long)i; }
            }
            catch (OverflowException)
            {
                j = 2;
            }

            if (j != 2)
                return 0;
            return 3;
        }

        public static int test_4_checked_cast()
        {
            long i;
            ulong j;

            unchecked { i = (long)0x8000000034000000; };
            try
            {
                checked { j = (ulong)i; }
            }
            catch (OverflowException)
            {
                j = 3;
            }

            if (j != 3)
                return 0;
            return 4;
        }

    //    static readonly int[] mul_dim_results = new int[] {
    //    0, 0, 0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0, 8,
    //    1, 0, 1, 1, 1, 2, 1, 3, 1, 4, 1, 5, 1, 6, 1, 7, 1, 8,
    //    2, 0, 2, 1, 2, 8, 
    //    3, 0, 3, 1, 3, 8, 
    //    4, 0, 4, 1, 4, 8, 
    //    5, 0, 5, 1, 5, 2, 5, 3, 5, 4, 5, 5, 5, 6, 5, 7, 5, 8,
    //    6, 0, 6, 1, 6, 2, 6, 3, 6, 4, 6, 5, 6, 6, 6, 7, 6, 8,
    //    7, 0, 7, 1, 7, 2, 7, 3, 7, 4, 7, 5, 7, 6, 7, 7, 7, 8,
    //};

    //    public static int test_0_multi_dim_array_access()
    //    {
    //        int[,] a = new int[2, 2] { { 3, 6 }, { 2, 2 } };
    //        int x, y;
    //        int result_idx = 0;
    //        for (x = 0; x < 8; ++x)
    //        {
    //            for (y = 0; y < 9; ++y)
    //            {
    //                bool got_ex = false;
    //                try
    //                {
    //                    a[x, y] = 1;
    //                }
    //                catch
    //                {
    //                    got_ex = true;
    //                }
    //                if (got_ex)
    //                {
    //                    if (result_idx >= mul_dim_results.Length)
    //                        return -1;
    //                    if (mul_dim_results[result_idx] != x || mul_dim_results[result_idx + 1] != y)
    //                    {
    //                        return result_idx + 1;
    //                    }
    //                    result_idx += 2;
    //                }
    //            }
    //        }
    //        if (result_idx == mul_dim_results.Length)
    //            return 0;
    //        return 200;
    //    }

        static void helper_out_obj(out object o)
        {
            o = (object)"buddy";
        }

        static void helper_out_string(out string o)
        {
            o = "buddy";
        }

        //public static int test_2_array_mismatch()
        //{
        //    string[] a = { "hello", "world" };
        //    object[] b = a;
        //    bool passed = false;

        //    try
        //    {
        //        helper_out_obj(out b[1]);
        //    }
        //    catch (ArrayTypeMismatchException)
        //    {
        //        passed = true;
        //    }
        //    if (!passed)
        //        return 0;
        //    helper_out_string(out a[1]);
        //    if (a[1] != "buddy")
        //        return 1;
        //    return 2;
        //}

        public static int test_0_ovf1()
        {
            int exception = 0;

            checked
            {
                try
                {
                    ulong a = UInt64.MaxValue - 1;
                    ulong t = a++;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_1_ovf2()
        {
            int exception = 0;

            checked
            {
                try
                {
                    ulong a = UInt64.MaxValue;
                    ulong t = a++;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_0_ovf3()
        {
            int exception = 0;

            long a = Int64.MaxValue - 1;
            checked
            {
                try
                {
                    long t = a++;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_1_ovf4()
        {
            int exception = 0;

            long a = Int64.MaxValue;
            checked
            {
                try
                {
                    long t = a++;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_0_ovf5()
        {
            int exception = 0;

            ulong a = UInt64.MaxValue - 1;
            checked
            {
                try
                {
                    ulong t = a++;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_1_ovf6()
        {
            int exception = 0;

            ulong a = UInt64.MaxValue;
            checked
            {
                try
                {
                    ulong t = a++;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_0_ovf7()
        {
            int exception = 0;

            long a = Int64.MinValue + 1;
            checked
            {
                try
                {
                    long t = a--;
                }
                catch
                {
                    exception = 1;
                }
            }
            return 0;
        }

        public static int test_1_ovf8()
        {
            int exception = 0;

            long a = Int64.MinValue;
            checked
            {
                try
                {
                    long t = a--;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_0_ovf9()
        {
            int exception = 0;

            ulong a = UInt64.MinValue + 1;
            checked
            {
                try
                {
                    ulong t = a--;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_1_ovf10()
        {
            int exception = 0;

            ulong a = UInt64.MinValue;
            checked
            {
                try
                {
                    ulong t = a--;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_0_ovf11()
        {
            int exception = 0;

            int a = Int32.MinValue + 1;
            checked
            {
                try
                {
                    int t = a--;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_1_ovf12()
        {
            int exception = 0;

            int a = Int32.MinValue;
            checked
            {
                try
                {
                    int t = a--;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_0_ovf13()
        {
            int exception = 0;

            uint a = 1;
            checked
            {
                try
                {
                    uint t = a--;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_1_ovf14()
        {
            int exception = 0;

            uint a = 0;
            checked
            {
                try
                {
                    uint t = a--;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_0_ovf15()
        {
            int exception = 0;

            sbyte a = 126;
            checked
            {
                try
                {
                    sbyte t = a++;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_1_ovf16()
        {
            int exception = 0;

            sbyte a = 127;
            checked
            {
                try
                {
                    sbyte t = a++;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_0_ovf17()
        {
            int exception = 0;

            checked
            {
                try
                {
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_0_ovf18()
        {
            int exception = 0;

            int a = 1 << 29;
            checked
            {
                try
                {
                    int t = a * 2;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_1_ovf19()
        {
            int exception = 0;

            int a = 1 << 30;
            checked
            {
                try
                {
                    int t = a * 2;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_0_ovf20()
        {
            int exception = 0;

            checked
            {
                try
                {
                    ulong a = 0xffffffffff;
                    ulong t = a * 0x0ffffff;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_1_ovf21()
        {
            int exception = 0;

            ulong a = 0xffffffffff;
            checked
            {
                try
                {
                    ulong t = a * 0x0fffffff;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_1_ovf22()
        {
            int exception = 0;

            long a = Int64.MinValue;
            long b = 10;
            checked
            {
                try
                {
                    long v = a * b;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        public static int test_1_ovf23()
        {
            int exception = 0;

            long a = 10;
            long b = Int64.MinValue;
            checked
            {
                try
                {
                    long v = a * b;
                }
                catch
                {
                    exception = 1;
                }
            }
            return exception;
        }

        //class Broken
        //{
        //    public static int i;

        //    static Broken()
        //    {
        //        throw new Exception("Ugh!");
        //    }

        //    public static int DoSomething()
        //    {
        //        return i;
        //    }
        //}

        //public static int test_0_exception_in_cctor()
        //{
        //    try
        //    {
        //        Broken.DoSomething();
        //    }
        //    catch (TypeInitializationException)
        //    {
        //        // This will only happen once even if --regression is used
        //    }
        //    return 0;
        //}

        public static int test_5_regalloc()
        {
            int i = 0;

            try
            {
                for (i = 0; i < 10; ++i)
                {
                    if (i == 5)
                        throw new Exception();
                }
            }
            catch (Exception)
            {
                if (i != 5)
                    return i;
            }

            // Check that variables written in catch clauses are volatile
            int j = 0;
            try
            {
                throw new Exception();
            }
            catch (Exception)
            {
                j = 5;
            }
            if (j != 5)
                return 6;

            int k = 0;
            try
            {
                try
                {
                    throw new Exception();
                }
                finally
                {
                    k = 5;
                }
            }
            catch (Exception)
            {
            }
            if (k != 5)
                return 7;

            return i;
        }

        public static void rethrow()
        {
            try
            {
                throw new ApplicationException();
            }
            catch (ApplicationException)
            {
                try
                {
                    throw new OverflowException();
                }
                catch (Exception)
                {
                    throw;
                }
            }
        }

        // Test that a rethrow rethrows the correct exception
        public static int test_0_rethrow_nested()
        {
            try
            {
                rethrow();
            }
            catch (OverflowException)
            {
                return 0;
            }
            catch (Exception)
            {
                return 1;
            }
            return 2;
        }

        /* MarshalByRefObject prevents the methods from being inlined */
        class ThrowClass// : MarshalByRefObject
        {
            public static void rethrow1()
            {
                throw new Exception();
            }

            public static void rethrow2()
            {
                rethrow1();
                /* This disables tailcall opts */
                Console.WriteLine();
            }
        }

        //public static int test_0_rethrow_stacktrace()
        //{
        //    // Check that rethrowing an exception preserves the original stack trace
        //    try
        //    {
        //        try
        //        {
        //            ThrowClass.rethrow2();
        //        }
        //        catch (Exception ex)
        //        {
        //            // Check that each catch clause has its own exception variable
        //            // If not, the throw below will overwrite the exception used
        //            // by the rethrow
        //            try
        //            {
        //                throw new DivideByZeroException();
        //            }
        //            catch (Exception foo)
        //            {
        //            }

        //            throw;
        //        }
        //    }
        //    catch (Exception ex)
        //    {
        //        if (ex.StackTrace.IndexOf("rethrow2") != -1)
        //            return 0;
        //    }

        //    return 1;
        //}

        interface IFace { }
        class Face : IFace { }

        public static int test_1_array_mismatch_2()
        {
            try
            {
                object[] o = new Face[1];
                o[0] = 1;
                return 0;
            }
            catch (ArrayTypeMismatchException)
            {
                return 1;
            }
        }

        public static int test_1_array_mismatch_3()
        {
            try
            {
                object[] o = new IFace[1];
                o[0] = 1;
                return 0;
            }
            catch (ArrayTypeMismatchException)
            {
                return 1;
            }
        }

        public static int test_1_array_mismatch_4()
        {
            try
            {
                object[][] o = new Face[5][];
                o[0] = new object[5];

                return 0;
            }
            catch (ArrayTypeMismatchException)
            {
                return 1;
            }
        }

        //public static int test_0_array_size()
        //{
        //    bool failed;

        //    try
        //    {
        //        failed = true;
        //        int[] mem1 = new int[Int32.MaxValue];
        //    }
        //    catch (OutOfMemoryException e)
        //    {
        //        failed = false;
        //    }
        //    if (failed)
        //        return 1;

        //    try
        //    {
        //        failed = true;
        //        int[,] mem2 = new int[Int32.MaxValue, Int32.MaxValue];
        //    }
        //    catch (OutOfMemoryException e)
        //    {
        //        failed = false;
        //    }
        //    if (failed)
        //        return 2;

        //    return 0;
        //}

        struct S
        {
            int i, j, k, l, m, n;
        }

        static IntPtr[] addr;

        static unsafe void throw_func(int i, S s)
        {
            addr[i] = new IntPtr(&i);
            throw new Exception();
        }

        /* Test that arguments are correctly popped off the stack during unwinding */
        public static int test_0_stack_unwind()
        {
            addr = new IntPtr[1000];
            S s = new S();
            for (int j = 0; j < 1000; j++)
            {
                try
                {
                    throw_func(j, s);
                }
                catch (Exception)
                {
                }
            }
            return (addr[0].ToInt64() - addr[100].ToInt64() < 100) ? 0 : 1;
        }

        static unsafe void get_sp(int i)
        {
            addr[i] = new IntPtr(&i);
        }

        /* Test that the arguments to the throw trampoline are correctly popped off the stack */
        public static int test_0_throw_unwind()
        {
            addr = new IntPtr[1000];
            S s = new S();
            for (int j = 0; j < 1000; j++)
            {
                try
                {
                    get_sp(j);
                    throw new Exception();
                }
                catch (Exception)
                {
                }
            }
            return (addr[0].ToInt64() - addr[100].ToInt64() < 100) ? 0 : 1;
        }

        public static int test_0_regress_73242()
        {
            int[] arr = new int[10];
            for (int i = 0; i < 10; ++i)
                arr[i] = 0;
            try
            {
                throw new Exception();
            }
            catch
            {
            }
            return 0;
        }

        public static int test_0_nullref()
        {
            try
            {
                Array foo = null;
                foo.Clone();
            }
            catch (NullReferenceException e)
            {
                return 0;
            }
            return 1;
        }

        public int amethod()
        {
            return 1;
        }

        public static int test_0_nonvirt_nullref_at_clause_start()
        {
            Exceptions t = null;
            try
            {
                t.amethod();
            }
            catch (NullReferenceException)
            {
                return 0;
            }

            return 1;
        }

        //public static int throw_only()
        //{
        //    throw new Exception();
        //}

        //[MethodImpl(MethodImplOptions.NoInlining)]
        //public static int throw_only2()
        //{
        //    return throw_only();
        //}

        //public static int test_0_inline_throw_only()
        //{
        //    try
        //    {
        //        return throw_only2();
        //    }
        //    catch (Exception ex)
        //    {
        //        return 0;
        //    }
        //}

        public static string GetText(string s)
        {
            return s;
        }

        public static int throw_only_gettext()
        {
            throw new Exception(GetText("FOO"));
        }

        public static int test_0_inline_throw_only_gettext()
        {
            object o = null;
            try
            {
                o = throw_only_gettext();
            }
            catch (Exception ex)
            {
                return 0;
            }

            return o != null ? 0 : 1;
        }

        // bug #78633
        public static int test_0_throw_to_branch_opt_outer_clause()
        {
            int i = 0;

            try
            {
                try
                {
                    string[] files = new string[1];

                    string s = files[2];
                }
                finally
                {
                    i++;
                }
            }
            catch
            {
            }

            return (i == 1) ? 0 : 1;
        }

        // bug #485721
        public static int test_0_try_inside_finally_cmov_opt()
        {
            bool Reconect = false;

            object o = new object();

            try
            {
            }
            catch (Exception ExCon)
            {
                if (o != null)
                    Reconect = true;

                try
                {
                }
                catch (Exception Last)
                {
                }
            }
            finally
            {
                if (Reconect == true)
                {
                    try
                    {
                    }
                    catch (Exception ex)
                    {
                    }
                }
            }

            return 0;
        }

        public static int test_0_inline_throw()
        {
            try
            {
                inline_throw1(5);
                return 1;
            }
            catch
            {
                return 0;
            }
        }

        // for llvm, the end bblock is unreachable
        public static int inline_throw1(int i)
        {
            if (i == 0)
                throw new Exception();
            else
                return inline_throw2(i);
        }

        public static int inline_throw2(int i)
        {
            throw new Exception();
        }

        //// bug #539550
        //public static int test_0_lmf_filter()
        //{
        //    try
        //    {
        //        // The invoke calls a runtime-invoke wrapper which has a filter clause
        //        Exceptions.lmf_filter();
        //    }
        //    catch (TargetInvocationException)
        //    {
        //    }
        //    return 0;
        //}

        //public static void lmf_filter()
        //{
        //    try
        //    {
        //        Connect();
        //    }
        //    catch
        //    {
        //        throw new NotImplementedException();
        //    }
        //}

        //public static void Connect()
        //{
        //    Stop();
        //    throw new Exception();
        //}

        //public static void Stop()
        //{
        //    try
        //    {
        //        lock (null) { }
        //    }
        //    catch
        //    {
        //    }
        //}

        private static void do_raise()
        {
            throw new System.Exception();
        }

        private static int int_func(int i)
        {
            return i;
        }

        // #559876
        public static int test_8_local_deadce_causes()
        {
            int myb = 4;

            try
            {
                myb = int_func(8);
                do_raise();
                myb = int_func(2);
            }
            catch (System.Exception)
            {
                return myb;
            }
            return 0;
        }

        public static int test_0_except_opt_two_clauses()
        {
            int size;
            size = -1;
            uint ui = (uint)size;
            try
            {
                checked
                {
                    uint v = ui * (uint)4;
                }
            }
            catch (OverflowException e)
            {
                return 0;
            }
            catch (Exception)
            {
                return 1;
            }

            return 2;
        }

        class Child
        {
            public virtual long Method()
            {
                throw new Exception();
            }
        }

        /* #612206 */
        public static int test_100_long_vars_in_clauses_initlocals_opt()
        {
            Child c = new Child();
            long value = 100;
            try
            {
                value = c.Method();
            }
            catch { }
            return (int)value;
        }

        class A
        {
            public object AnObj;
        }

        public static void DoSomething(ref object o)
        {
        }

        public static int test_0_ldflda_null()
        {
            A a = null;

            try
            {
                DoSomething(ref a.AnObj);
            }
            catch (NullReferenceException)
            {
                return 0;
            }

            return 1;
        }

        unsafe struct Foo
        {
            public int i;

            public static Foo* pFoo;
        }

        /* MS.NET doesn't seem to throw in this case */
        public unsafe static int test_0_ldflda_null_pointer()
        {
            int* pi = &Foo.pFoo->i;

            return 0;
        }
    }
}
