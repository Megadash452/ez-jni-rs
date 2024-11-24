package me.marti.ezjni;

// Seems that doing a Location class was veryb hard :/
// import me.marti.ezjni.PanicLocation;

public final class RustPanic extends RuntimeException {
    public String file;
    /** Unsigned int */
    public int line;
    /** Unsigned int */
    public int col;
    private String[] backtrace;

    // TODO: RustPanic(PanicLocation location, String message, String[] backtrace) {
    //     super(message);
    //     this.location = location;
    //     this.backtrace = backtrace;
    // }
    RustPanic(String file, int line, int col, String message) {
        super(message);
        this.file = file;
        this.line = line;
        this.col = col;
    }

    @Override
    public String toString() {
        return "RustPanic(location: \"" + this.file + ":" + Integer.toUnsignedString(this.line) + ":" + Integer.toUnsignedString(this.col)
            + "\", message: \"" + this.getMessage()
            + "\", backtrace: \"" + backtrace + "\")";
    }   
}

