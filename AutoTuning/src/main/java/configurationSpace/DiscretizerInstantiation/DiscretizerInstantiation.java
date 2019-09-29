package configurationSpace.DiscretizerInstantiation;

import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;

import java.util.*;

public interface DiscretizerInstantiation<T extends Discretizer> {

    /**
     * Return a random discretizer instance with.
     *
     * @return the array of discretizer instances
     */
    T getRandomDiscretizer();

    /**
     * Get the simple class name of the discretizer.
     *
     * @return the simple name of the discretizer class
     */
    String getClassName();

    /**
     * Get the configuration string of the parameters needed for the discretizer. This string format is needed for smac
     * to recognize the parameter dependencies correctly. The child parameter name consists of the column for the
     * discretizer, the specific disretizer simple class name and the index of the parameter for initialization
     * of the discretizer instance.
     *
     * @param parentName the name of the parent parameter of the discretizer
     * @return the string readable by smac to initialize the parameter space
     */
    String getChildParameterConfig(String parentName);

    /**
     * Create an instance of a discretizer with the configuration selected by smac. For example, when smac chooses the
     * PercentileMedianDiscretizer for a configuration then the instance of this discretizer is created with the
     * respective classCount parameter.
     *
     * @param parameters the list of parameters selected for a column
     * @return the instance of the discretizer that is selected
     */
    T constructDiscretizer(List<Map.Entry<String, String>> parameters);

    T getDefaultDiscretizer();

}
