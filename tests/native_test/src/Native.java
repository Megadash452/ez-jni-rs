package me.test;

import java.util.Arrays;

public final class Native {
    static { System.loadLibrary("native_test"); }
    public Native() { }

    public static void main(String[] args) {
        System.out.println("Hello, Rust ~ from Java");

        Native n = new Native();
        n.test_native_fns();

        int len = native_test_static("Hello, World!");
        assert len == 13;
    }

    private void test_native_fns() {
        boolean z = native_test_bool(true);
        char c = native_test_char('a');
        byte b = native_test_byte((byte)3);
        short s = native_test_short((short)3);
        int i = native_test_int(3);
        long j = native_test_long(3L);
        float f = native_test_float(3.3f);
        double d = native_test_double(3.3);
        assert z == true;
        assert c == 'a';
        assert b == 3;
        assert s == 3;
        assert i == 3;
        assert j == 3;
        assert f == 3.3f;
        assert d == 3.3;
        // native_test_ubyte(u8);
        // native_test_ushort(u16);
        // native_test_uint(u32);
        // native_test_ulong(u64);

        boolean[] z_a = native_test_bool_array(new boolean[] {true, false});
        char[] c_a = native_test_char_array(new char[] {'a', 'b'});
        byte[] b_a = native_test_byte_array(new byte[] {(byte)1, (byte)2, (byte)3});
        short[] s_a = native_test_short_array(new short[] {(short)1, (short)2, (short)3});
        int[] i_a = native_test_int_array(new int[] {1, 2, 3});
        long[] j_a = native_test_long_array(new long[] {1L, 2L, 3L});
        float[] f_a = native_test_float_array(new float[] {1.1f, 2.2f, 3.3f});
        double[] d_a = native_test_double_array(new double[] {1.1, 2.2, 3.3});
        assert Arrays.equals(z_a, new boolean[] {true, false});
        assert Arrays.equals(c_a, new char[] {'a', 'b'});
        assert Arrays.equals(b_a, new byte[] {(byte)1, (byte)2, (byte)3});
        assert Arrays.equals(s_a, new short[] {(short)1, (short)2, (short)3});
        assert Arrays.equals(i_a, new int[] {1, 2, 3});
        assert Arrays.equals(j_a, new long[] {1L, 2L, 3L});
        assert Arrays.equals(f_a, new float[] {1.1f, 2.2f, 3.3f});
        assert Arrays.equals(d_a, new double[] {1.1, 2.2, 3.3});
        // public u8[] native_test_ubyte_array(u8[]);
        // public u16[] native_test_ushort_array(u16[]);
        // public u32[] native_test_uint_array(u32[]);
        // public u64[] native_test_ulong_array(u64[]);

        Object obj_arg = new java.lang.Object();
        native_test_void();
        String str = native_test_str("Hello, World!");
        Object obj = native_test_obj(obj_arg);
        String[] str_arr = native_test_str_arr(new String[] {"Hello", "World"});
        Object[] obj_arr = native_test_obj_arr(new Object[] {obj_arg});
        String null_str = native_test_null_str(null);
        String[] str_null_arr = native_test_str_null_arr(null);
        String[] null_str_arr = native_test_null_str_arr(new String[] {"Hello", null});
        String[][] _2d_str_arr = native_test_2d_str_arr(new String[][] {{"Hello", "Rust"}, {"From", "Java"}});
        String[][][] _3d_str_arr = native_test_3d_str_arr(new String[][][] {
            {
                {"I", "am", "Groot"},
                {"here", "it", "is"},
            }, {
                {"Hello", "Rust"},
                {"From", "Java"},
            }
        });
        String[][] _2d_str_null_arr = native_test_2d_str_null_arr(new String[][] {{"Hello", "World"}, null});
        String[][][] _3d_null_str_arr = native_test_3d_null_str_arr(new String[][][] {
            {
                {"I", "am", null},
                {"here", "it", "is"},
            }, {
                {"Hello", null},
                {"From", "Java"},
            }
        });
        assert str.equals("Hello, World!");
        assert obj == obj_arg; // Object references should be the same
        assert Arrays.equals(str_arr, new String[] {"Hello", "World"});
        assert obj_arr[0] == obj_arg; // Object references should be the same
        assert null_str == null;
        assert str_null_arr == null;
        assert Arrays.equals(null_str_arr, new String[] {"Hello", null});
        assert Arrays.deepEquals(_2d_str_arr, new String[][] {{"Hello", "Rust"}, {"From", "Java"}});
        assert Arrays.deepEquals(_3d_str_arr, new String[][][] {
            {
                {"I", "am", "Groot"},
                {"here", "it", "is"},
            }, {
                {"Hello", "Rust"},
                {"From", "Java"},
            }
        });
        assert Arrays.deepEquals(_2d_str_null_arr, new String[][] {{"Hello", "World"}, null});
        assert Arrays.deepEquals(_3d_null_str_arr, new String[][][] {
            {
                {"I", "am", null},
                {"here", "it", "is"},
            }, {
                {"Hello", null},
                {"From", "Java"},
            }
        });

        try {
            native_test_panic();
        } catch (Exception e) {
            assert e.getMessage().equals("LALALA");
        }
    }

    public native boolean native_test_bool(boolean z);
    public native char native_test_char(char c);
    public native byte native_test_byte(byte b);
    public native short native_test_short(short s);
    public native int native_test_int(int i);
    public native long native_test_long(long j);
    public native float native_test_float(float f);
    public native double native_test_double(double d);
    // public native u8 native_test_ubyte(u8);
    // public native u16 native_test_ushort(u16);
    // public native u32 native_test_uint(u32);
    // public native u64 native_test_ulong(u64);

    public native boolean[] native_test_bool_array(boolean[] z);
    public native char[] native_test_char_array(char[] c);
    public native byte[] native_test_byte_array(byte[] b);
    public native short[] native_test_short_array(short[] s);
    public native int[] native_test_int_array(int[] i);
    public native long[] native_test_long_array(long[] j);
    public native float[] native_test_float_array(float[] f);
    public native double[] native_test_double_array(double[] d);
    // public native u8[] native_test_ubyte_array(u8[]);
    // public native u16[] native_test_ushort_array(u16[]);
    // public native u32[] native_test_uint_array(u32[]);
    // public native u64[] native_test_ulong_array(u64[]);

    public native void native_test_void();
    public native void native_test_recursion(byte b);
    public static native int native_test_static(String t);
    public native String native_test_str(String t);
    public native Object native_test_obj(Object l);
    public native String[] native_test_str_arr(String[] t);
    public native Object[] native_test_obj_arr(Object[] l);
    public native String native_test_null_str( String t);
    public native String[] native_test_str_null_arr( String[] t);
    public native String[] native_test_null_str_arr(String[] t);
    public native String[][] native_test_2d_str_arr(String[][] t);
    public native String[][][] native_test_3d_str_arr(String[][][] t);
    public native String[][] native_test_2d_str_null_arr(String[][] t);
    public native String[][][] native_test_3d_null_str_arr(String[][][] t);

    public native void native_test_panic();
}
