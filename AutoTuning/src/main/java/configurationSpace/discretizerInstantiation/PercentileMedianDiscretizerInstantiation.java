package configurationSpace.discretizerInstantiation;

import configurationSpace.IntegerParameter;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.PercentileMedianDiscretizer;

import java.util.List;
import java.util.Map;
import java.util.Random;

public class PercentileMedianDiscretizerInstantiation implements DiscretizerInstantiation<PercentileMedianDiscretizer> {

    private final int minClassCount = 1;
    private final int maxClassCount = 2000;

    public PercentileMedianDiscretizerInstantiation() {
    }

    @Override
    public PercentileMedianDiscretizer getRandomDiscretizer() {
        return new PercentileMedianDiscretizer(new Random().nextInt(maxClassCount) + minClassCount);
    }

    @Override
    public PercentileMedianDiscretizer getDefaultDiscretizer() {
        return new PercentileMedianDiscretizer((int) Math.sqrt(maxClassCount));
    }

    @Override
    public String getChildParameterConfig(String parentName) {
        IntegerParameter classCount = new IntegerParameter(parentName + "_" + getClassName(), (int) Math.sqrt(maxClassCount), minClassCount, maxClassCount);
        return classCount.getParameterString() + "\n" + classCount.getName() + "|" + parentName + " == " + getClassName() + "\n";
    }

    @Override
    public PercentileMedianDiscretizer constructDiscretizer(List<Map.Entry<String, String>> parameters) {
        return new PercentileMedianDiscretizer(Integer.parseInt(parameters.get(0).getValue()));
    }

    @Override
    public String getClassName() {
        return PercentileMedianDiscretizer.class.getSimpleName();
    }
}
