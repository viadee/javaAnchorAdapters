package de.viadee.xai.anchor.adapter.tabular;

import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

/**
 * Provides default means to use the Anchors algorithm on tabular data
 * <p>
 * To make use of this, use the AbstractTabularBuilder and its
 * various implementations to create an instance of this class.
 *
 * TODO This class used to contain all preprocessing logic. This logic has been separated. Check, whether to deprecate
 */
public class AnchorTabular {
    private final TabularInstance[] tabularInstances;
    private final TabularInstanceVisualizer tabularInstanceVisualizer;

    /**
     * Creates the object.
     * <p>
     * A builder may be used to easily construct the instance
     *
     * @param tabularInstances          the instances, resembling the kept data
     * @param tabularInstanceVisualizer the visualizer used to format the output
     */
    public AnchorTabular(TabularInstance[] tabularInstances,
                         final TabularInstanceVisualizer tabularInstanceVisualizer) {
        this.tabularInstances = tabularInstances;
        this.tabularInstanceVisualizer = tabularInstanceVisualizer;
    }

    /**
     * Returns the kept data
     *
     * @return the tabular instances after transformation and discretization
     */
    public TabularInstance[] getTabularInstances() {
        return tabularInstances;
    }

    /**
     * Returns all used features
     *
     * @return an UnmodifiableList of the contained columns
     */
    public List<GenericColumn> getColumns() {
        return Collections.unmodifiableList(Arrays.asList(tabularInstances[0].getFeatures()));
    }

    /**
     * Returns the target feature
     *
     * @return the target column
     */
    public GenericColumn getTargetColumn() {
        return tabularInstances[0].getTargetFeature();
    }


    /**
     * Returns a visualizer which can be used to format the output to a readable format
     *
     * @return an instance of the {@link TabularInstanceVisualizer} to visualize explanations and instances
     */
    public TabularInstanceVisualizer getVisualizer() {
        return tabularInstanceVisualizer;
    }

    /**
     * Provides a default builder configures with the contents of this class
     *
     * @param classificationFunction the classificationFunction to use
     * @param explainedInstance      the instance to explain
     * @return the further configurable or directly usable builder
     */
    public AnchorConstructionBuilder<TabularInstance> createDefaultBuilder(final Function<TabularInstance, Integer> classificationFunction,
                                                                           final TabularInstance explainedInstance) {
        TabularPerturbationFunction tabularPerturbationFunction = new TabularPerturbationFunction(
                explainedInstance,
                this.tabularInstances);
        return new AnchorConstructionBuilder<>(classificationFunction::apply, tabularPerturbationFunction, explainedInstance);
    }
}
