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

    public String[] arrayField = new String[] { "Hello", "World" };

    public int memberField;
    public Integer memberObject() {
        return new Integer(this.memberField);
    }
    public int memberGetter() {
        return this.memberField;
    }

    public static int member1 = 3;
    public static String member2 = "Hello, World!";
    public static String[] memberWeird = new String[] {"Hello", "World"};
    private static char _member3 = 'a';
    public static char getMember3() { return _member3; }
    public static void setMember3(char val) { _member3 = val; }

    public static void      getVoid()      { return; }
    public static boolean   getBoolean()   { return true; }
    public static char      getChar()      { return 'a'; }
    public static byte      getByte()      { return 3; }
    public static short     getShort()     { return 3; }
    public static int       getInt()       { return 3; }
    public static long      getLong()      { return 3; }
    public static float     getFloat()     { return 3.3f; }
    public static double    getDouble()    { return 3.3d; }
    public static Boolean   getBooleanObj(){ return new Boolean(true); }
    public static Character getCharObj()   { return new Character('a'); }
    public static Byte      getByteObj()   { return new Byte(3); }
    public static Short     getShortObj()  { return new Short(3); }
    public static Integer   getIntObj()    { return new Integer(3); }
    public static Long      getLongObj()   { return new Long(3); }
    public static Float     getFloatObj()  { return new Float(3.3f); }
    public static Double    getDoubleObj() { return new Double(3.3d); }
    public static Object    getObject()    { return new Object(); }
    public static String    getString()    { return "Hello, World"; }
    public static Class     getMyClass()   { return Test.class; }
    public static Exception getException() { return new Exception("hi"); }
    public static Object    nullable()     { return null; }
    public static Boolean   getNullPrim()  { return null; }
    public static boolean   throwPrim() throws Exception { throw new Exception("exception"); }
    public static Object    throwObj()  throws Exception { throw new Exception("exception"); }

    public static boolean[]   getBooleanArray()    { return new boolean[]   {true, false}; }
    public static char[]      getCharArray()       { return new char[]      {'a', 'b', 'c'}; }
    public static byte[]      getByteArray()       { return new byte[]      {1, 2, 3}; }
    public static short[]     getShortArray()      { return new short[]     {1, 2, 3}; }
    public static int[]       getIntArray()        { return new int[]       {1, 2, 3}; }
    public static long[]      getLongArray()       { return new long[]      {1, 2, 3}; }
    public static float[]     getFloatArray()      { return new float[]     {1.1f, 2.2f, 3.3f}; }
    public static double[]    getDoubleArray()     { return new double[]    {1.1d, 2.2d, 3.3d}; }
    public static Object[]    getObjectArray()     { return new Object[]    {new Object()}; }
    public static Boolean[]   getBooleanObjArray() { return new Boolean[]   {new Boolean(true), null}; }
    public static Character[] getCharObjArray()    { return new Character[] {new Character('a'), null}; }
    public static Byte[]      getByteObjArray()    { return new Byte[]      {new Byte(1), null}; }
    public static Short[]     getShortObjArray()   { return new Short[]     {new Short(1), null}; }
    public static Integer[]   getIntObjArray()     { return new Integer[]   {new Integer(1), null}; }
    public static Long[]      getLongObjArray()    { return new Long[]      {new Long(1), null}; }
    public static Float[]     getFloatObjArray()   { return new Float[]     {new Float(1.1f), null}; }
    public static Double[]    getDoubleObjArray()  { return new Double[]    {new Double(1.1), null}; }
    public static Object[]    getNullObjectArray() { return new Object[]    {new Object(), null}; }
    public static String[]    getStringArray()     { return new String[]    {"Hello", "World"}; }
    public static String[]    getNullStringArray() { return new String[]    {"Hello", null}; }
    public static int[][]     get2DIntArray()      { return new int[][]     { new int[] {1, 2}, new int[] {3, 4} }; }
    public static int[][][]   get3DIntArray()      { return new int[][][]   { new int[][] { new int[] {1, 2}, new int[] {3, 4} }, new int[][] { new int[] {5, 6}, new int[] {7, 8} } }; }
    public static String[][]  get2DStringArray()   { return new String[][]  { new String[] {"Hello", "World"}, new String[] {"How", "are", "you"} }; }

    public static boolean[] primNullArray() { return null; }
    public static Object[]  nullObjArray()  { return null; }
    public static boolean[] throwPrimArray() throws IndexOutOfBoundsException { throw new IndexOutOfBoundsException("exception"); }
    public static Object[]  throwObjArray()  throws IndexOutOfBoundsException { throw new IndexOutOfBoundsException("exception"); }
    
    public static void primArgs(boolean z, char c, byte b, short s, int i, long j, float f, double d) { }
    public static void primObjArgs(Boolean z, Character c, Byte b, Short s, Integer i, Long j, Float f, Double d) { }
    public static void objArgs(Object l, String s) { }
    public static void otherArgs(Class c, Exception e) {  }
    public static void primObjArrayArgs(Boolean[] z, Character[] c, Byte[] b, Short[] s, Integer[] i, Long[] j, Float[] f, Double[] d) { }
    public static void objArrayArgs(Object[] l, String[] s) { }
    public static void prim2DArrayArgs(boolean[][] z, char[][] c, int[][] i) { }
    public static void prim3DArrayArgs(int[][][] i, float[][][] f) { }
    public static void obj2DArrayArgs(Object[][] l, String[][] s) { }

    // Test methods and fields on an Object rather than on a Class
    public static class Instanced {
        Instanced() {}

        public int member1 = 3;
        public String member2 = "Hello, World!";
        private char _member3 = 'a';
        public char getMember3() { return this._member3; }
        public void setMember3(char val) { this._member3 = val; } 

        // Just test one primitive and one object this time
        public boolean getBoolean() { return true; }
        public Object getObject() { return new Object(); }
        public void args(boolean z, Object l) { }
        public void arrayArgs(boolean[] z, Object[] l) { }
    }

    public static class Singleton {
        private static Singleton instance = null;
        public static Singleton getInstance() {
            if (instance == null)
                instance = new Singleton();
 
            return instance;
        }

        public int member;

        public int method() { return 3; }

        private Singleton() {
            this.member = 3;
        }
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
}
