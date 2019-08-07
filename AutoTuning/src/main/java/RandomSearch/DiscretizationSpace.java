package RandomSearch;

import Parameter.CategoricalParameter;
import Parameter.Parameter;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.UniqueValueDiscretizer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DiscretizationSpace {

    private List<CategoricalParameter> discretizerParamter = new ArrayList<CategoricalParameter>();

    public DiscretizationSpace(AnchorTabular anchorTabular, Discretizer[] discretizers) {

        for (GenericColumn column : anchorTabular.getColumns()) {
            if (column.getDiscretizer().getClass() != UniqueValueDiscretizer.class) {
                this.discretizerParamter.add(
                        new CategoricalParameter(
                                column.getName(),
                                discretizers[0].getClass().getName(),
                                discretizers[0].getClass().getName(),
                                Arrays.stream(discretizers).map(c -> c.getClass().getName()).toArray(String[]::new))
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

    /**
     * Clone a given parameter set space
     *
     * @param original the original parameter set
     * @return the cloned parameter set
     */
    public static List<Parameter> clone(List<Parameter> original) {
        List<Parameter> result = new ArrayList<>();
        for (Parameter p : original) {
            result.add(p.copy());
        }
        return result;
    }
    
    public List<CategoricalParameter> getDiscretizerParamter() {
        return discretizerParamter;
    }
}
