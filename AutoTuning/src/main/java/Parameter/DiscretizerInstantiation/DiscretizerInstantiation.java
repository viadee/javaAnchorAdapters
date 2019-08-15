package Parameter.DiscretizerInstantiation;

import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;

import java.util.*;

public interface DiscretizerInstantiation<T extends Discretizer> {

    /**
     * Return an array of all possible discretizers with all its parametrization. For example the
     * PercentileMedianDiscretizer can have in total as many instances as the column has unique values.
     *
     * @return the array of discretizer instances
     */
    T[] getAllDiscretizers();

    /**
     * Get a number of random selected instances from the total pool of discretizer instances.
     *
     * @param count the number of random discretizer instances that is required
     * @return a list with n many random selected discretizer instances
     */
    default List<T> selectRandom(int count) {
        List<T> arrayList = new ArrayList<>(Arrays.asList(getAllDiscretizers()));
        if (arrayList.size() < count)
            throw new IllegalArgumentException("Less elements than expected");
        Collections.shuffle(arrayList);
        return arrayList.subList(0, count);
    }

    /**
     * Get the simple class name of the discretizer.
     *
     * @return the simple name of the discretizer class
     */
    String getClassName();

    /**
     * Get the configuration string of the parameters needed for the discretizer. This string format is needed for SMAC
     * to recognize the parameter dependencies correctly. The child parameter name consists of the column for the
     * discretizer, the specific disretizer simple class name and the index of the parameter for initialization
     * of the discretizer instance.
     *
     * @param parentName the name of the parent parameter of the discretizer
     * @return the string readable by SMAC to initialize the parameter space
     */
    String getChildParameterConfig(String parentName);

    /**
     * Create an instance of a discretizer with the configuration selected by SMAC. For example, when SMAC chooses the
     * PercentileMedianDiscretizer for a configuration then the instance of this discretizer is created with the
     * respective classCount parameter.
     *
     * @param parameters the list of parameters selected for a column
     * @return the instance of the discretizer that is selected
     */
    T constructDiscretizer(List<Map.Entry<String, String>> parameters);

}
