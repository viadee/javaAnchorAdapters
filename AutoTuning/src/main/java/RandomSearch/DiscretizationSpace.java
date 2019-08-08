package RandomSearch;

import Parameter.DiscretizerInstantiation.DiscretizerInstantiation;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DiscretizationSpace {

    List<Discretizer> discretizers;

    public DiscretizationSpace(DiscretizerInstantiation... instantiations) {
        discretizers = Stream.of(instantiations).flatMap(d -> (Stream<Discretizer>) d.selectRandom(10).stream()).collect(Collectors.toList());
    }

    public DiscretizationSpace(List<Discretizer> discretizers) {
        this.discretizers = discretizers;
    }

    public void addDiscretizer(Discretizer discretizer) {
        this.discretizers.add(discretizer);
    }

    public Discretizer getRandomDiscretizer() {
        Collections.shuffle(this.discretizers);
        return this.discretizers.get(0);
    }

}



