package Parameter.DiscretizerInstantiation;

import Parameter.IntegerParameter;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.PercentileMedianDiscretizer;

import java.util.List;
import java.util.Map;
import java.util.Random;

public class PercentileMedianDiscretizerInstantiation implements DiscretizerInstantiation<PercentileMedianDiscretizer> {

    private final int minClassCount = 1;
    private final int maxClassCount = 100;

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

        StringBuffer stringBuffer = new StringBuffer(100);
        stringBuffer
                .append(classCount.getParameterString())
                .append("\n")
                .append(classCount.getName())
                .append("|")
                .append(parentName)
                .append(" == ")
                .append(getClassName())
                .append("\n");
        return stringBuffer.toString();
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
