package RandomSearch;

import Parameter.CategoricalParameter;

import java.util.Arrays;

public class DiscretizationSpace {

    private CategoricalParameter discretizerParamter;

    public enum Discretizer {
        MEDIAN_PERCENTILE,
        UNIQUE_VALUE
    }

    public DiscretizationSpace() {
        this(Discretizer.values());
    }

    public DiscretizationSpace(Discretizer[] discretizers) {

        this.discretizerParamter = new CategoricalParameter(
                Discretizer.class.getName(),
                Discretizer.values()[0].toString(),
                Discretizer.values()[0].toString(), Arrays.stream(discretizers).map(Enum::name).toArray(String[]::new)
        );
    }
}
