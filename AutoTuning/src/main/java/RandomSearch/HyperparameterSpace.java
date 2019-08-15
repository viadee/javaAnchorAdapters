package RandomSearch;

import Parameter.ContinuousParameter;
import Parameter.IntegerParameter;
import Parameter.Parameter;

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


    public String getConfigurationString() {
        StringBuffer stringBuffer = new StringBuffer(300);

        for (Parameter p : hyperParameters) {
            stringBuffer
                    .append(p.getParameterString())
                    .append("\n");

        }
        return stringBuffer.toString();
    }

    public List<Parameter> getHyperParameters() {
        return hyperParameters;
    }

}
