package Parameter.DiscretizerInstantiation;

import Parameter.ContinuousParameter;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.FUSINTERDiscretizer;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

public class FUSINTERDiscretizerInstantiation implements DiscretizerInstantiation<FUSINTERDiscretizer> {

    @Override
    public FUSINTERDiscretizer[] getAllDiscretizers() {

        List<FUSINTERDiscretizer> discretizers = new ArrayList<>();

        IntStream.range(0, 100).mapToDouble(i -> i / 100D).forEach(lambda ->
                IntStream.range(0, 100).mapToDouble(j -> j / 100D).forEach(alpha ->
                        discretizers.add(new FUSINTERDiscretizer(lambda, alpha))));

        return discretizers.toArray(new FUSINTERDiscretizer[0]);
    }

    @Override
    public FUSINTERDiscretizer getDefaultDiscretizer() {
        return new FUSINTERDiscretizer();
    }

    @Override
    public String getChildParameterConfig(String parentName) {
        ContinuousParameter lamda = new ContinuousParameter(parentName + "_" + getClassName() + "_0", 1.0, 0, 1);
        ContinuousParameter alpha = new ContinuousParameter(parentName + "_" + getClassName() + "_1", 0.975, 0, 1);

        StringBuilder stringBuilder = new StringBuilder(100);
        stringBuilder
                .append(lamda.getParameterString())
                .append("\n")
                .append(alpha.getParameterString())
                .append("\n")
                .append(lamda.getName())
                .append("|")
                .append(parentName)
                .append(" == ")
                .append(getClassName())
                .append("\n")
                .append(alpha.getName())
                .append("|")
                .append(parentName)
                .append(" == ")
                .append(getClassName())
                .append("\n");
        return stringBuilder.toString();
    }

    @Override
    public FUSINTERDiscretizer constructDiscretizer(List<Map.Entry<String, String>> parameters) {
        return new FUSINTERDiscretizer(Double.valueOf(parameters.get(0).getValue()), Double.valueOf(parameters.get(1).getValue()));
    }

    @Override
    public String getClassName() {
        return FUSINTERDiscretizer.class.getSimpleName();
    }
}
