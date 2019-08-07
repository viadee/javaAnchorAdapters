package de.viadee.xai.anchor.adapter.tabular.discretizer.impl;

import de.viadee.xai.anchor.adapter.tabular.discretizer.AbstractDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.DiscretizationTransition;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class MDLPDiscretizer extends AbstractDiscretizer {

    public MDLPDiscretizer() {
        super(true);
    }

    @Override
    protected List<DiscretizationTransition> fitCreateTransitions(Serializable[] values, Double[] labels) {
        List<DiscretizationTransition> discretizationTransitions = new ArrayList<>();
        return discretizationTransitions;
    }
}
