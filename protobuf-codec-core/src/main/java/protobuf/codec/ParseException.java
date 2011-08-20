package protobuf.codec;

/**
 * A generic exception in case of a parsing error. Basically a wrapper
 * around checked exceptions that are thrown by underlying parsing framework used.
 * @author sijuv
 *
 */
public class ParseException extends RuntimeException {

    private static final long serialVersionUID = -6183310495238101156L;

    public ParseException() {
        super();
    }

    public ParseException(String message, Throwable cause) {
        super(message, cause);
    }

    public ParseException(String message) {
        super(message);
    }

    public ParseException(Throwable cause) {
        super(cause);
    }
}
