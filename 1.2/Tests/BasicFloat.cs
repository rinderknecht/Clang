using System;

/*
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

/* A comparison made to same variable. */
//#pragma warning disable 1718

namespace Cortus.TestSuite
{
    class BasicFloat
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
            Test(test_0_beq() == 0, "test_0_beq");
            Test(test_0_bne_un() == 0, "test_0_bne_un");
            Test(test_0_conv_r8() == 0, "test_0_conv_r8");
            Test(test_0_conv_i() == 0, "test_0_conv_i");
            Test(test_5_conv_r4() == 5, "test_5_conv_r4");
            Test(test_0_conv_r4_m1() == 0, "test_0_conv_r4_m1");
            Test(test_5_double_conv_r4() == 5, "test_5_double_conv_r4");
            Test(test_5_float_conv_r8() == 5, "test_5_float_conv_r8");
            Test(test_5_conv_r8() == 5, "test_5_conv_r8");
            Test(test_5_add() == 5, "test_5_add");
            Test(test_5_sub() == 5, "test_5_sub");
            Test(test_24_mul() == 24, "test_24_mul");
            Test(test_4_div() == 4, "test_4_div");
            Test(test_2_rem() == 2, "test_2_rem");
            Test(test_2_neg() == 2, "test_2_neg");
            Test(test_46_float_add_spill() == 46, "test_46_float_add_spill");
            Test(test_4_float_sub_spill() == 4, "test_4_float_sub_spill");
            Test(test_362880_float_mul_spill() == 362880, "test_362880_float_mul_spill");
            Test(test_4_long_cast() == 4, "test_4_long_cast");
            Test(test_4_ulong_cast() == 4, "test_4_ulong_cast");
            Test(test_4_single_long_cast() == 4, "test_4_single_long_cast");
            Test(test_0_lconv_to_r8() == 0, "test_0_lconv_to_r8");
            Test(test_0_lconv_to_r4() == 0, "test_0_lconv_to_r4");
            Test(test_0_ftol_clobber() == 0, "test_0_ftol_clobber");
            Test(test_0_rounding() == 0, "test_0_rounding");
            Test(test_16_float_cmp() == 16, "test_16_float_cmp");
            Test(test_15_float_cmp_un() == 15, "test_15_float_cmp_un");
            Test(test_15_float_branch() == 15, "test_15_float_branch");
            Test(test_15_float_branch_un() == 15, "test_15_float_branch_un");
            Test(test_0_float_precision() == 0, "test_0_float_precision");

            if (passed == 30)
            {
                System.Console.WriteLine("SUCCESS");
            }
            else
            {
                System.Console.WriteLine("FAIL");
            }

            Program.TotalPassed += passed;
            Program.TotalFailed += 30 - passed;
        }

        public static int test_0_beq()
        {
            double a = 2.0;
            if (a != 2.0)
                return 1;
            return 0;
        }

        public static int test_0_bne_un()
        {
            double a = 2.0;
            if (a == 1.0)
                return 1;
            return 0;
        }

        public static int test_0_conv_r8()
        {
            double a = 2;
            if (a != 2.0)
                return 1;
            return 0;
        }

        public static int test_0_conv_i()
        {
            double a = 2.0;
            int i = (int)a;
            if (i != 2)
                return 1;
            uint ui = (uint)a;
            if (ui != 2)
                return 2;
            short s = (short)a;
            if (s != 2)
                return 3;
            ushort us = (ushort)a;
            if (us != 2)
                return 4;
            byte b = (byte)a;
            if (b != 2)
                return 5;
            sbyte sb = (sbyte)a;
            if (sb != 2)
                return 6;
            return 0;
        }

        public static int test_5_conv_r4()
        {
            int i = 5;
            float f = (float)i;
            return (int)f;
        }

        public static int test_0_conv_r4_m1()
        {
            int i = -1;
            float f = (float)i;
            return (int)f + 1;
        }

        public static int test_5_double_conv_r4()
        {
            double d = 5.0;
            float f = (float)d;
            return (int)f;
        }

        public static int test_5_float_conv_r8()
        {
            float f = 5.0F;
            double d = (double)f;
            return (int)d;
        }

        public static int test_5_conv_r8()
        {
            int i = 5;
            double f = (double)i;
            return (int)f;
        }

        public static int test_5_add()
        {
            double a = 2.0;
            double b = 3.0;
            return (int)(a + b);
        }

        public static int test_5_sub()
        {
            double a = 8.0;
            double b = 3.0;
            return (int)(a - b);
        }

        public static int test_24_mul()
        {
            double a = 8.0;
            double b = 3.0;
            return (int)(a * b);
        }

        public static int test_4_div()
        {
            double a = 8.0;
            double b = 2.0;
            return (int)(a / b);
        }

        public static int test_2_rem()
        {
            double a = 8.0;
            double b = 3.0;
            return (int)(a % b);
        }

        public static int test_2_neg()
        {
            double a = -2.0;
            return (int)(-a);
        }

        public static int test_46_float_add_spill()
        {
            // we overflow the FP stack
            double a = 1;
            double b = 2;
            double c = 3;
            double d = 4;
            double e = 5;
            double f = 6;
            double g = 7;
            double h = 8;
            double i = 9;

            return (int)(1.0 + (a + (b + (c + (d + (e + (f + (g + (h + i)))))))));
        }

        public static int test_4_float_sub_spill()
        {
            // we overflow the FP stack
            double a = 1;
            double b = 2;
            double c = 3;
            double d = 4;
            double e = 5;
            double f = 6;
            double g = 7;
            double h = 8;
            double i = 9;

            return -(int)(1.0 - (a - (b - (c - (d - (e - (f - (g - (h - i)))))))));
            ////// -(int)(1.0 - (1 - (2 - (3 - (4 - (5 - (6 - (7 - (8 - 9)))))))));
        }

        public static int test_362880_float_mul_spill()
        {
            // we overflow the FP stack
            double a = 1;
            double b = 2;
            double c = 3;
            double d = 4;
            double e = 5;
            double f = 6;
            double g = 7;
            double h = 8;
            double i = 9;

            return (int)(1.0 * (a * (b * (c * (d * (e * (f * (g * (h * i)))))))));
        }

        public static int test_4_long_cast()
        {
            long a = 1000;
            double d = (double)a;
            long b = (long)d;
            if (b != 1000)
                return 0;
            a = -1;
            d = (double)a;
            b = (long)d;
            if (b != -1)
                return 1;
            return 4;
        }

        public static int test_4_ulong_cast()
        {
            ulong a = 1000;
            double d = (double)a;
            ulong b = (ulong)d;
            if (b != 1000)
                return 0;
            return 4;
        }

        public static int test_4_single_long_cast()
        {
            long a = 1000;
            float d = (float)a;
            long b = (long)d;
            if (b != 1000)
                return 0;
            a = -1;
            d = (float)a;
            b = (long)d;
            if (b != -1)
                return 1;
            return 4;
        }

        public static int test_0_lconv_to_r8()
        {
            long a = 150;
            double b = (double)a;

            if (b != 150.0)
                return 1;
            return 0;
        }

        public static int test_0_lconv_to_r4()
        {
            long a = 3000;
            float b = (float)a;

            if (b != 3000.0F)
                return 1;
            return 0;
        }

        static void doit(double value, out long m)
        {
            m = (long)value;
        }

        public static int test_0_ftol_clobber()
        {
            long m;
            doit(1.3, out m);
            if (m != 1)
                return 2;
            return 0;
        }

        public static int test_0_rounding()
        {
            long ticks = 631502475130080000L;
            long ticksperday = 864000000000L;

            double days = (double)ticks / ticksperday;

            if ((int)days != 730905)
                return 1;

            return 0;
        }

        /* FIXME: This only works on little-endian machines */
        
        static unsafe int test_2_negative_zero () {
            int result = 0;
            double d = -0.0;
            float f = -0.0f;

            byte *ptr = (byte*)&d;
            if (ptr [7] == 0)
                return result;
            result ++;

            ptr = (byte*)&f;
            if (ptr [3] == 0)
                return result;
            result ++;

            return result;
        }
        

        public static int test_16_float_cmp()
        {
            double a = 2.0;
            double b = 1.0;
            int result = 0;
            bool val;

            val = a == a;
            if (!val)
                return result;
            result++;

            val = (a != a);
            if (val)
                return result;
            result++;

            val = a < a;
            if (val)
                return result;
            result++;

            val = a > a;
            if (val)
                return result;
            result++;

            val = a <= a;
            if (!val)
                return result;
            result++;

            val = a >= a;
            if (!val)
                return result;
            result++;

            val = b == a;
            if (val)
                return result;
            result++;

            val = b < a;
            if (!val)
                return result;
            result++;

            val = b > a;
            if (val)
                return result;
            result++;

            val = b <= a;
            if (!val)
                return result;
            result++;

            val = b >= a;
            if (val)
                return result;
            result++;

            val = a == b;
            if (val)
                return result;
            result++;

            val = a < b;
            if (val)
                return result;
            result++;

            val = a > b;
            if (!val)
                return result;
            result++;

            val = a <= b;
            if (val)
                return result;
            result++;

            val = a >= b;
            if (!val)
                return result;
            result++;

            return result;
        }

        public static int test_15_float_cmp_un()
        {
            double a = Double.NaN;
            double b = 1.0;
            int result = 0;
            bool val;

            val = a == a;
            if (val)
                return result;
            result++;

            val = a < a;
            if (val)
                return result;
            result++;

            val = a > a;
            if (val)
                return result;
            result++;

            val = a <= a;
            if (val)
                return result;
            result++;

            val = a >= a;
            if (val)
                return result;
            result++;

            val = b == a;
            if (val)
                return result;
            result++;

            val = b < a;
            if (val)
                return result;
            result++;

            val = b > a;
            if (val)
                return result;
            result++;

            val = b <= a;
            if (val)
                return result;
            result++;

            val = b >= a;
            if (val)
                return result;
            result++;

            val = a == b;
            if (val)
                return result;
            result++;

            val = a < b;
            if (val)
                return result;
            result++;

            val = a > b;
            if (val)
                return result;
            result++;

            val = a <= b;
            if (val)
                return result;
            result++;

            val = a >= b;
            if (val)
                return result;
            result++;

            return result;
        }

        public static int test_15_float_branch()
        {
            double a = 2.0;
            double b = 1.0;
            int result = 0;

            if (!(a == a))
                return result;
            result++;

            if (a < a)
                return result;
            result++;

            if (a > a)
                return result;
            result++;

            if (!(a <= a))
                return result;
            result++;

            if (!(a >= a))
                return result;
            result++;

            if (b == a)
                return result;
            result++;

            if (!(b < a))
                return result;
            result++;

            if (b > a)
                return result;
            result++;

            if (!(b <= a))
                return result;
            result++;

            if (b >= a)
                return result;
            result++;

            if (a == b)
                return result;
            result++;

            if (a < b)
                return result;
            result++;

            if (!(a > b))
                return result;
            result++;

            if (a <= b)
                return result;
            result++;

            if (!(a >= b))
                return result;
            result++;

            return result;
        }

        public static int test_15_float_branch_un()
        {
            double a = Double.NaN;
            double b = 1.0;
            int result = 0;

            if (a == a)
                return result;
            result++;

            if (a < a)
                return result;
            result++;

            if (a > a)
                return result;
            result++;

            if (a <= a)
                return result;
            result++;

            if (a >= a)
                return result;
            result++;

            if (b == a)
                return result;
            result++;

            if (b < a)
                return result;
            result++;

            if (b > a)
                return result;
            result++;

            if (b <= a)
                return result;
            result++;

            if (b >= a)
                return result;
            result++;

            if (a == b)
                return result;
            result++;

            if (a < b)
                return result;
            result++;

            if (a > b)
                return result;
            result++;

            if (a <= b)
                return result;
            result++;

            if (a >= b)
                return result;
            result++;

            return result;
        }

        public static int test_0_float_precision()
        {
            float f1 = 3.40282346638528859E+38f;
            float f2 = 3.40282346638528859E+38f;
            float PositiveInfinity = 1.0f / 0.0f;
            float f = f1 + f2;

            return f == PositiveInfinity ? 0 : 1;
        }
    }
}
