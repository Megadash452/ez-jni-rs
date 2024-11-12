package me.test;

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

    public void test_native_fns() {
        boolean z = native_test_bool(true);
        assert true == false; // FIXME WHY DOES THIS PASS????
        char c = native_test_char('a');
        assert c == 'a';
        byte b = native_test_byte((byte)3);
        assert b == 3;
        short s = native_test_short((short)3);
        assert s == 3;
        int i = native_test_int(3);
        assert i == 3;
        long j = native_test_long(3L);
        assert j == 3;
        float f = native_test_float(3.3f);
        assert f == 3.3;
        double d = native_test_double(3.3);
        assert d == 3.3;
        // native_test_ubyte(u8);
        // native_test_ushort(u16);
        // native_test_uint(u32);
        // native_test_ulong(u64);

        boolean[] z_a = native_test_bool_array(new boolean[] {true, false});
        assert z_a == new boolean[] {true, false};
        char[] c_a = native_test_char_array(new char[] {'a', 'b'});
        assert c_a == new char[] {'a', 'b'};
        byte[] b_a = native_test_byte_array(new byte[] {(byte)1, (byte)2, (byte)3});
        assert b_a == new byte[] {(byte)1, (byte)2, (byte)3};
        short[] s_a = native_test_short_array(new short[] {(short)1, (short)2, (short)3});
        assert s_a == new short[] {(short)1, (short)2, (short)3};
        int[] i_a = native_test_int_array(new int[] {1, 2, 3});
        assert i_a == new int[] {1, 2, 3};
        long[] j_a = native_test_long_array(new long[] {1L, 2L, 3L});
        assert j_a == new long[] {1L, 2L, 3L};
        float[] f_a = native_test_float_array(new float[] {1.1f, 2.2f, 3.3f});
        assert f_a == new float[] {1.1f, 2.2f, 3.3f};
        double[] d_a = native_test_double_array(new double[] {1.1, 2.2, 3.3});
        assert d_a == new double[] {1.1, 2.2, 3.3};
        // public u8[] native_test_ubyte_array(u8[]);
        // public u16[] native_test_ushort_array(u16[]);
        // public u32[] native_test_uint_array(u32[]);
        // public u64[] native_test_ulong_array(u64[]);

        native_test_void();
        assert native_test_str("Hello, World!") == "Hello, World";
        native_test_obj(new java.lang.Object());
        native_test_str_arr(new String[] {"Hello", "World"});
        native_test_obj_arr(new Object[] {new java.lang.Object()});
        native_test_null_str(null);
        native_test_str_null_arr(null);
        native_test_null_str_arr(new String[] {"Hello", null});
        native_test_2d_str_arr(new String[][] {{"Hello", "Rust"}, {"From", "Java"}});
        native_test_3d_str_arr(new String[][][] {
            {
                {"I", "am", "Groot"},
                {"here", "it", "is"},
            }, {
                {"Hello", "Rust"},
                {"From", "Java"},
            }
        });
        native_test_2d_str_null_arr(new String[][] {{"Hello", "World"}, null});
        native_test_3d_null_str_arr(new String[][][] {
            {
                {"I", "am", null},
                {"here", "it", "is"},
            }, {
                {"Hello", null},
                {"From", "Java"},
            }
        });
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
    public static native int native_test_static(String t);
    public native String native_test_str(String t);
    public native Object native_test_obj(Object l);
    public native String[] native_test_str_arr(String[] t);
    public native Object[] native_test_obj_arr(Object[] l);
    public native String native_test_null_str( String t);
    public native  String[] native_test_str_null_arr( String[] t);
    public native String[] native_test_null_str_arr(String[] t);
    public native String[][] native_test_2d_str_arr(String[][] t);
    public native String[][][] native_test_3d_str_arr(String[][][] t);
    public native String[][] native_test_2d_str_null_arr(String[][] t);
    public native String[][][] native_test_3d_null_str_arr(String[][][] t);
}
