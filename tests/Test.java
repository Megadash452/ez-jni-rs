package me.test;

public final class Test {
    Test() {
        this.memberField = 3;
    }
    Test(int member) {
        this.memberField = member;
    }
    Test(String len) throws NullPointerException {
        if (len == null) {
            throw new NullPointerException("String was null");
        } else {
            this.memberField = len.length();
        }
    }

    public int memberField;
    public Integer memberObject() {
        return new Integer(this.memberField);
    }
    public int memberGetter() {
        return this.memberField;
    }

    public static void    getVoid()    { return; }
    public static boolean getBoolean() { return true; }
    public static char    getChar()    { return 'a'; }
    public static byte    getByte()    { return 3; }
    public static short   getShort()   { return 3; }
    public static int     getInt()     { return 3; }
    public static long    getLong()    { return 3; }
    public static float   getFloat()   { return 3.3f; }
    public static double  getDouble()  { return 3.3d; }
    public static Object  getObject()  { return new Object(); }
    public static String  getString()  { return "Hello, World"; }
    public static Object  nullable()   { return null; }
    public static boolean throwPrim() throws Exception { throw new Exception("exception"); }
    public static Object  throwObj()  throws Exception { throw new Exception("exception"); }

    public static boolean[] getBooleanArray() { return new boolean[] {true, false}; }
    public static char[]    getCharArray()    { return new char[]    {'a', 'b', 'c'}; }
    public static byte[]    getByteArray()    { return new byte[]    {1, 2, 3}; }
    public static short[]   getShortArray()   { return new short[]   {1, 2, 3}; }
    public static int[]     getIntArray()     { return new int[]     {1, 2, 3}; }
    public static long[]    getLongArray()    { return new long[]    {1, 2, 3}; }
    public static float[]   getFloatArray()   { return new float[]   {1.1f, 2.2f, 3.3f}; }
    public static double[]  getDoubleArray()  { return new double[]  {1.1d, 2.2d, 3.3d}; }
    public static Object[]  getObjectArray()  { return new Object[]  {new Object()}; }
    public static Object[]  getNullObjectArray()  { return new Object[]  {new Object(), null}; }
    public static String[]  getStringArray()  { return new String[]  {"Hello", "World"}; }
    public static String[]  getNullStringArray()  { return new String[]  {"Hello", null}; }
    public static int[][]   get2DIntArray() { return new int[][] {new int[] {1, 2}, new int[] {3, 4} }; }
    public static String[][] get2DStringArray() { return new String[][] {new String[] {"Hello", "World"}, new String[] {"How", "are", "you"} }; }

    public static boolean[] nullPrimArray() { return null; }
    public static Object[]  nullObjArray()    { return null; }
    public static boolean[] throwPrimArray() throws Exception { throw new Exception("exception"); }
    public static Object[]  throwObjArray()   throws Exception { throw new Exception("exception"); }
    
    public static void primArgs(boolean z, char c, byte b, short s, int i, long j, float f, double d) { }
    public static void objArgs(Object l, String s) { }
    public static void primArrayArgs(boolean[] z, char[] c, byte[] b, short[] s, int[] i, long[] j, float[] f, double[] d) { }
    public static void objArrayArgs(Object[] l, String[] s) { }

    public static class Instanced {
        Instanced() {}
        // Just test one primitive and one object this time
        public boolean getBoolean() { return true; }
        public Object getObject() { return new Object(); }
        public void args(boolean z, Object l) { }
        public void arrayArgs(boolean[] z, Object[] l) { }
    }

    public static sealed class SumClass {
        public static final class SumClass1 extends SumClass {
            SumClass1(int number) {
                this.number = number;
            }
            public int number;
        }
        public static final class SumClass2 extends SumClass {
            SumClass2(String str) {
                this.str = str;
            }
            private String str;
            public String getStr() { return this.str; }
        }
    }

    public void testNativeMethods() {
        this.test_jni_fn_1();
        this.test_jni_fn_2("Hello, World!");
        this.test_jni_fn_3("Hello, World!");
    }
    private native void test_jni_fn_1();
    private native int test_jni_fn_2(String s);
    private native String test_jni_fn_3(String s);
}
