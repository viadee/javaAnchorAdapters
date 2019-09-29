package configurationSpace;

import dataInitialization.DataInitializer;
import configurationSpace.DiscretizerInstantiation.DiscretizerInstantiation;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.AmevaDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.UniqueValueDiscretizer;

import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DiscretizationSpace {

    private DiscretizerInstantiation[] instantiations;
    private List<Discretizer> discretizers;
    private AnchorTabular anchorTabular;

    public DiscretizationSpace(DataInitializer dataInitializer, DiscretizerInstantiation... instantiations) {
        this.instantiations = instantiations;
        this.anchorTabular = dataInitializer.createTabular(null);
    }

    public DiscretizationSpace(DiscretizerInstantiation... instantiations) {
        this.instantiations = instantiations;
    }

    public DiscretizationSpace(List<Discretizer> discretizers) {
        this.discretizers = discretizers;
    }

    public void addDiscretizer(Discretizer discretizer) {
        this.discretizers.add(discretizer);
    }

    public Discretizer getRandomDiscretizer() {
        return this.instantiations[new Random().nextInt(instantiations.length)].getRandomDiscretizer();
    }

    public String transferToConfigurationSpace() {
        StringBuffer stringBuffer = new StringBuffer(100);
        for (GenericColumn column : anchorTabular.getColumns()) {
            String parentParameterName = column.getName() + "_discretizer";
            if (column.getDiscretizer().getClass() != UniqueValueDiscretizer.class) {
                stringBuffer.append(
                        new CategoricalParameter(parentParameterName,
                                AmevaDiscretizer.class.getSimpleName(),
                                Stream.of(instantiations).map(DiscretizerInstantiation::getClassName).flatMap(Stream::of).collect(Collectors.toList()).toArray(new String[0]))
                                .getParameterString());
                stringBuffer.append("\n");
                for (int i = 0; i < instantiations.length; i++) {
                    stringBuffer.append(instantiations[i].getChildParameterConfig(parentParameterName));
                }
            }
        }
        return stringBuffer.toString();
    }
}



