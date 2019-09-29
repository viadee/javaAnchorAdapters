package smac.util;

import ca.ubc.cs.beta.aeatk.parameterconfigurationspace.ParameterConfiguration;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;

public abstract class AnchorsConfig {

    public static AnchorConstructionBuilder setParameters(ParameterConfiguration configuration, AnchorConstructionBuilder<TabularInstance> anchorBuilder){
        if (configuration.get("tau") != null)
            anchorBuilder.setTau(Double.valueOf(configuration.get("tau")));
        if (configuration.get("epsilon") != null)
            anchorBuilder.setEpsilon(Double.valueOf(configuration.get("epsilon")));
        if (configuration.get("delta") != null)
            anchorBuilder.setDelta(Double.valueOf(configuration.get("delta")));
        if (configuration.get("beamsize") != null)
            anchorBuilder.setBeamSize(Integer.valueOf(configuration.get("beamsize")));
        if (configuration.get("initSampleCount") != null)
            anchorBuilder.setInitSampleCount(Integer.valueOf(configuration.get("initSampleCount")));
        if (configuration.get("tauDiscrepancy") != null)
            anchorBuilder.setTauDiscrepancy(Double.valueOf(configuration.get("tauDiscrepancy")));
        if (configuration.get("allowSuboptimalSteps") != null)
            anchorBuilder.setAllowSuboptimalSteps(Boolean.valueOf(configuration.get("allowSuboptimalSteps")));

        return anchorBuilder;
    }
}
