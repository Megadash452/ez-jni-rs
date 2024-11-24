package me.marti.ezjni;

public final class PanicLocation {
    public String file;
    /** Unsigned int */
    public int line;
    /** Unsigned int */
    public int col;

    /** Line and column are **Unsigned Ints**. */
    PanicLocation(String file, int line, int col) {
        this.file = file;
        this.line = line;
        this.col = col;
    }
}
