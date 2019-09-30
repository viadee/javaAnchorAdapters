package smac.util;

import ca.ubc.cs.beta.aeatk.parameterconfigurationspace.ParameterConfiguration;
import configurationSpace.discretizerInstantiation.DiscretizerInstantiation;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.UniqueValueDiscretizer;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The abstract class for automatically fitting Anchors to the by SMAC provided configuration
 * <p>
 * Provides functionalities to set Anchors' hyperparameters and set new Discretizers to the AnchorTabular
 */
public abstract class AnchorsConfig {

    /**
     * Set Anchors' hyperparameters to the new selected configuration
     *
     * @param configuration the configuration provided by SMAC
     * @param anchorBuilder the anchorBuilder that is adapted to the new configuration
     * @return new fitted anchorBuilder
     */
    public static AnchorConstructionBuilder setParametersForSmac(ParameterConfiguration configuration, AnchorConstructionBuilder<TabularInstance> anchorBuilder){
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

    /**
     * Get a Map of discretizers from the SMAC configuration
     *
     * @param configuration the configuration provided by SMAC
     * @param anchorTabular the anchorBuilder to get all discretized columns
     * @return a map of discretizers
     */
    public static Map<String, Discretizer> setDiscretizerForSmac(ParameterConfiguration configuration, AnchorTabular anchorTabular) {
        final Map<String, String> parameters = configuration.getActiveParameters().stream()
                .map(p -> new HashMap.SimpleImmutableEntry<>(p, configuration.get(p)))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        Map<String, Discretizer> nameToDiscretizer = new HashMap<>();

        for (GenericColumn column : anchorTabular.getColumns()) {

            if (!column.getDiscretizer().getClass().getSimpleName().equals(UniqueValueDiscretizer.class.getSimpleName())) {
                Map<String, String> columnDiscretizerParameters = parameters.entrySet().stream()
                        .sorted(Map.Entry.comparingByKey())
                        .filter(e -> e.getKey().toLowerCase().contains(column.getName().toLowerCase()))
                        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                                (oldValue, newValue) -> oldValue, LinkedHashMap::new));

                List<Map.Entry<String, String>> test = columnDiscretizerParameters.entrySet().stream()
                        .collect(Collectors.toList());

                try {
                    final String constructorClassName = "configurationSpace.discretizerInstantiation." +
                            test.get(0).getValue() + "Instantiation";

                    final DiscretizerInstantiation discretizerConstructor = (DiscretizerInstantiation) Class.forName(constructorClassName)
                            .getConstructor().newInstance();

                    Discretizer newDiscretizer = discretizerConstructor.constructDiscretizer(test.subList(1, test.size()));

                    nameToDiscretizer.put(column.getName(), newDiscretizer);
                } catch (ClassNotFoundException | NoSuchMethodException | InstantiationException | IllegalAccessException | InvocationTargetException e) {
                    throw new RuntimeException(e);
                }
            }
        }

        return nameToDiscretizer;
    }
}
