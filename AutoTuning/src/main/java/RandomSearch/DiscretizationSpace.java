package RandomSearch;

import Parameter.CategoricalParameter;
import Parameter.Parameter;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DiscretizationSpace {

    private List<CategoricalParameter> discretizerParamter = new ArrayList<CategoricalParameter>();

    public enum Discretizer {
        MEDIAN_PERCENTILE,
        UNIQUE_VALUE
    }

    public DiscretizationSpace(AnchorTabular anchorTabular) {
        this(anchorTabular, Discretizer.values());
    }

    public DiscretizationSpace(AnchorTabular anchorTabular, Discretizer[] discretizers) {

        for (GenericColumn column : anchorTabular.getColumns()) {
            if (column.getDiscretizer() != null) {
                this.discretizerParamter.add(
                        new CategoricalParameter(
                                Discretizer.class.getName(),
                                Discretizer.values()[0].toString(),
                                Discretizer.values()[0].toString(),
                                Arrays.stream(discretizers).map(Enum::name).toArray(String[]::new))
                );
            }
        }
    }

    /**
     * Randomize all parameters in the discretization space
     */
    public void randomizeParameters() {
        for (Parameter p : discretizerParamter) {
            p.searchRandom();
        }
    }
}
