package me.test;

public final class Test {
    Test(int member) {
        this.memberField = member;
    }

    public int memberField;
    public Integer memberObject() {
        return new Integer(this.memberField);
    }
    public int memberGetter() {
        return this.memberField;
    }

    public static void getVoid() { return; }
    public static boolean getBoolean() { return true; }
    public static char getChar() { return 'a'; }
    public static byte getByte() { return 1; }
    public static short getShort() { return 1; }
    public static int getInt() { return 1; }
    public static long getLong() { return 1; }
    public static float getFloat() { return 1f; }
    public static double getDouble() { return 1d; }
    public static Object getObject() { return new Object(); }
    public static String getString() { return "Hello, World"; }
    public static boolean throwPrim() throws Exception { throw new Exception("exception"); }
    public static Object throwObj() throws Exception { throw new Exception("exception"); }
    public static Object nullable() { return null; }
    
    public static void multiArg(boolean z, char c, byte b, short s, int i, long j, float f, double d, Object l) { }
    public static void arrayArg(boolean[] z, char[] c, byte[] b, short[] s, int[] i, long[] j, float[] f, double[] d, Object[] l) { }

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
            public String str;
        }
    }
}
