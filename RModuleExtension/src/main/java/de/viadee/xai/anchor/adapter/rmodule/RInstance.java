package de.viadee.xai.anchor.adapter.rmodule;

import de.viadee.xai.anchor.algorithm.DataInstance;

public class RInstance implements DataInstance<Object[]> {

    private int dim = 0;

    RInstance(int dimension){
        dim = dimension;
    }

    @Override
    public Object[] getInstance() {
        return new Object[0];
    }

    @Override
    public int getFeatureCount() {
        return dim;
    }
}
