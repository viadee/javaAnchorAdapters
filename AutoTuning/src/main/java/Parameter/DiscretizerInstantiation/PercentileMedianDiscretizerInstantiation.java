package Parameter.DiscretizerInstantiation;

import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.PercentileMedianDiscretizer;

import java.util.stream.IntStream;

public class PercentileMedianDiscretizerInstantiation implements DiscretizerInstantiation<PercentileMedianDiscretizer> {

    private final int uniqueValueCount;

    public PercentileMedianDiscretizerInstantiation(int uniqueValueCount) {
        this.uniqueValueCount = uniqueValueCount;
    }

    @Override
    public PercentileMedianDiscretizer[] getAllDiscretizers() {
        return IntStream.range(1, uniqueValueCount).mapToObj(PercentileMedianDiscretizer::new).toArray(PercentileMedianDiscretizer[]::new);
    }
}
