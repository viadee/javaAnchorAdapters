package RandomSearch;

import Parameter.Parameter;
import Parameter.IntegerParameter;
import Parameter.ContinuousParameter;

import java.util.ArrayList;
import java.util.List;

public final class HyperparameterSpace {

    private List<Parameter> hyperParameters;

    public HyperparameterSpace() {

        List<Parameter> parameters = new ArrayList<Parameter>();

        parameters.add(new IntegerParameter("beamsize", 2, 1, 30));
        parameters.add(new ContinuousParameter("tau", 1, 0.1, 1.0));
        parameters.add(new ContinuousParameter("delta", 0.1, 0.1, 0.5));
        parameters.add(new ContinuousParameter("epsilon", 0.1, 0.1, 0.5));
        parameters.add(new ContinuousParameter("tauDiscrepancy", 0.05, 0.01, 0.1));
        parameters.add(new IntegerParameter("initSampleCount", 1, 1, 10));

        this.hyperParameters = parameters;
    }

    public HyperparameterSpace(HyperparameterSpace copyFrom) {
        this.hyperParameters = clone(copyFrom.getHyperParameters());
    }

    public HyperparameterSpace(List<Parameter> hyperParameters) {
        this.hyperParameters = hyperParameters;
    }

    /**
     * Add a parameter to the Hyperparameter space to optimize
     *
     * @param parameter the parameter that should be added to the Hyperparameter space
     */
    public void addParameter(Parameter parameter) {
        hyperParameters.add(parameter);
    }

    /**
     * Get a specific parameter from the hyperparameter set by name
     *
     * @param name the name of the parameter
     * @return the parameter with the given name
     */
    public Parameter getParameterByName(String name) {
        for (Parameter p : hyperParameters) {
            if (p.getName().equals(name)) {
                return p;
            }
        }
        throw new IllegalArgumentException("No parameter found by the name of " + name);
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

    /**
     * Randomize all parameters in this hyperparameter space
     */
    public void randomizeParameters() {
        for (Parameter p : hyperParameters) {
            p.searchRandom();
        }
    }

    public List<Parameter> getHyperParameters() {
        return hyperParameters;
    }

}
