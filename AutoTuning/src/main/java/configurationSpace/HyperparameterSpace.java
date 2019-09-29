package configurationSpace;

import java.util.*;

public final class HyperparameterSpace {

    private List<Parameter> hyperParameters;

    public HyperparameterSpace(List<Parameter> hyperParameters) {
        this.hyperParameters = hyperParameters;
    }

    /**
     * Create the default Hyperparameterspace for the Anchors algorithm
     *
     * @return the instance of the HyperparameterSpace
     */
    public static HyperparameterSpace createDefaultHyperparameterSpace() {
        List<Parameter> parameters = new ArrayList<>();

        parameters.add(new IntegerParameter("beamsize", 2, 1, 30));
        parameters.add(new ContinuousParameter("tau", 0.95, 0.5, 1.0));
        parameters.add(new ContinuousParameter("delta", 0.1, 0.1, 0.5));
        parameters.add(new ContinuousParameter("epsilon", 0.1, 0.1, 0.5));
        parameters.add(new ContinuousParameter("tauDiscrepancy", 0.05, 0.01, 0.1));
        parameters.add(new IntegerParameter("initSampleCount", 1, 1, 10));
        parameters.add(new CategoricalParameter("allowSuboptimalSteps", "true", new String[]{"true", "false"}));

        return new HyperparameterSpace(parameters);
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
        return null;
    }

    /**
     * Get a map of randomized parameters
     *
     * @return the list of parameters with a random value assigned
     */
    public Map<String, String> randomizeParameters() {
        Map<String, String> parameters = new LinkedHashMap<>();
        for (Parameter parameter : hyperParameters) {
            parameters.put(parameter.getName(), parameter.getRandomValue().toString());
        }
        return parameters;
    }


    /**
     * Get the hyperparameters as String format for the smac algorithm. The format for the String for each
     * parameter is described in their respective class.
     *
     * @return the parameters in the String format needed by smac
     */
    public String getConfigurationString() {
        StringBuilder stringBuilder = new StringBuilder();

        for (Parameter p : hyperParameters) {
            stringBuilder
                    .append(p.getParameterString())
                    .append("\n");

        }
        return stringBuilder.toString();
    }

    public List<Parameter> getHyperParameters() {
        return hyperParameters;
    }

}
