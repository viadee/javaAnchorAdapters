package de.viadee.xai.anchor.adapter.tabular.builder;

import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;

/**
 * Exception signalling an error while loading and preparing the data
 */
class AnchorTabularBuilderException extends RuntimeException {

    private AnchorTabularBuilderException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * Throws an exception indicating transformation did not execute properly
     *
     * @param column the column that an operation failed for
     * @param cause the thrown exception
     * @return the exception
     */
    static AnchorTabularBuilderException transformationException(GenericColumn column, Throwable cause) {
        return new AnchorTabularBuilderException(
                String.format("An exception occurred while transforming column named [%s]: %s",
                        column.getName(),
                        cause.getMessage()),
                cause);
    }

    /**
     * Throws an exception indicating discretization fit did not execute properly
     *
     * @param column the column that an operation failed for
     * @param cause the thrown exception
     * @return the exception
     */
    static AnchorTabularBuilderException discretizationFitException(GenericColumn column, Throwable cause) {
        return new AnchorTabularBuilderException(
                String.format("An exception occurred while fitting discretizers for column named [%s]: %s",
                        column.getName(),
                        cause.getMessage()),
                cause);
    }

    /**
     * Throws an exception indicating discretization did not execute properly
     *
     * @param column the column that an operation failed for
     * @param cause the thrown exception
     * @return the exception
     */
    static AnchorTabularBuilderException discretizationException(GenericColumn column, Throwable cause) {
        return new AnchorTabularBuilderException(
                String.format("An exception occurred while discretizing column named [%s]: %s",
                        column.getName(),
                        cause.getMessage()),
                cause);
    }
}
