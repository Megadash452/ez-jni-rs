package me.ezjni;

public final class RustPanic extends RuntimeException {
    String file;
    /** Unsigned int */
    int line;
    /** Unsigned int */
    int col;

    /** Enables throwing a *Rust panic* as if it was an `Exception`.
     * Instantiate this class from the Rust side and provide the constructor with the `panic!` data.
     * 
     * Check out the other constructor to pass a **cause** `Exception`. */
    public RustPanic(String file, int line, int col, String message) {
        super(message);
        this.file = file;
        this.line = line;
        this.col = col;
    }
    /** Enables throwing a *Rust panic* as if it was an `Exception`.
     * Instantiate this class from the Rust side and provide the constructor with the `panic!` data.
     * 
     * Check out the other constructor if the `panic!` was not **caused** by an `Exception`.
     * 
     * @param cause The `Exception` or `Error` that the `panic!` was caused by.
     *   This argument CAN'T be `null`. */
    public RustPanic(String file, int line, int col, String message, Throwable cause) {
        super(message, cause);
        this.file = file;
        this.line = line;
        this.col = col;
    }
    

    /** Returns the Rust *source file* that the `panic!` occurred in. */
    public String getFile() { return this.file; }
    /** Returns the line in the Rust *source file* that the `panic!` occurred in. */
    public int getLine() { return this.line; }
    /** Returns the column in the Rust *source file* that the `panic!` occurred in. */
    public int getColumn() { return this.col; }
}

