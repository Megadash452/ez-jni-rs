package me.marti.ezjni;

import me.marti.ezjni.PanicLocation;

public final class RustPanic extends RuntimeException {
    public PanicLocation location;
    private String[] backtrace;

    // TODO: RustPanic(PanicLocation location, String message, String[] backtrace) {
    //     super(message);
    //     this.location = location;
    //     this.backtrace = backtrace;
    // }
    RustPanic(PanicLocation location, String message) {
        super(message);
        this.location = location;
    }

    @Override
    public String toString() {
        return "RustPanic(location: \"" + this.location
            + "\", message: \"" + this.getMessage()
            + "\", backtrace: \"" + backtrace + "\")";
    }   
}

