package de.viadee.xai.anchor.adapter.remotemodule;

import de.viadee.xai.anchor.algorithm.DataInstance;

/**
 * Represents a remotely held {@link DataInstance}
 */
class RemoteInstance implements DataInstance<Object[]> {

    private final int dim;

    /**
     * Creates the instance
     *
     * @param dimension the number of features available
     */
    RemoteInstance(int dimension){
        this.dim = dimension;
    }

    @Override
    public Object[] getInstance() {
        throw new UnsupportedOperationException("Instance is not being stored server-side");
    }

    @Override
    public int getFeatureCount() {
        return dim;
    }
}
