package de.viadee.xai.anchor.adapter.model.h2o;

/**
 */
public class H2oPredictException extends RuntimeException {
    private static final long serialVersionUID = -2761825202915608071L;

    public H2oPredictException() {
        super();
    }

    public H2oPredictException(String message) {
        super(message);
    }

    public H2oPredictException(String message, Throwable cause) {
        super(message, cause);
    }

    public H2oPredictException(Throwable cause) {
        super(cause);
    }

    protected H2oPredictException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
