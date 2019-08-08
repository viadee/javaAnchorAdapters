package Parameter.DiscretizerInstantiation;

import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.FUSINTERDiscretizer;

import java.util.stream.DoubleStream;
import java.util.stream.IntStream;

public class FUSEINTERDiscretizerInstantiation implements DiscretizerInstantiation {

    @Override
    public Discretizer[] getAllDiscretizers() {

        DoubleStream stream1 = IntStream.range(0, 100).mapToDouble(i -> i / 100);
        DoubleStream stream2 = IntStream.range(0, 100).mapToDouble(i -> i / 100);

        return stream1.mapToObj(lambda -> stream2.mapToObj(alpha -> new FUSINTERDiscretizer(lambda, alpha))).toArray(FUSINTERDiscretizer[]::new);
    }

}
