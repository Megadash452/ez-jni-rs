package me.marti.ezjni;

// Seems that doing a Location class was veryb hard :/
// import me.marti.ezjni.PanicLocation;

public final class RustPanic extends RuntimeException {
    public String file;
    /** Unsigned int */
    public int line;
    /** Unsigned int */
    public int col;

    RustPanic(String file, int line, int col, String message) {
        super(message);
        this.file = file;
        this.line = line;
        this.col = col;
    }
}

