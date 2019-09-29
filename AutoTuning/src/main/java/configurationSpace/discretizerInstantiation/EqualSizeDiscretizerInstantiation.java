package configurationSpace.discretizerInstantiation;

import configurationSpace.IntegerParameter;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.EqualSizeDiscretizer;

import java.util.List;
import java.util.Map;
import java.util.Random;

public class EqualSizeDiscretizerInstantiation implements DiscretizerInstantiation<EqualSizeDiscretizer> {

    private final int valueCount = 2000;

    public EqualSizeDiscretizerInstantiation() {
    }

    @Override
    public EqualSizeDiscretizer getRandomDiscretizer() {
        return new EqualSizeDiscretizer(new Random().nextInt(valueCount) + 1);
    }

    @Override
    public EqualSizeDiscretizer getDefaultDiscretizer() {
        return new EqualSizeDiscretizer();
    }

    @Override
    public String getClassName() {
        return EqualSizeDiscretizer.class.getSimpleName();
    }

    @Override
    public String getChildParameterConfig(String parentName) {
        IntegerParameter classSize = new IntegerParameter(parentName + "_" + getClassName(), (int) Math.sqrt(valueCount), 1, valueCount);
        return classSize.getParameterString() + "\n" + classSize.getName() + "|" + parentName + " == " + getClassName() + "\n";
    }

    @Override
    public EqualSizeDiscretizer constructDiscretizer(List<Map.Entry<String, String>> parameters) {
        return new EqualSizeDiscretizer(Integer.parseInt(parameters.get(0).getValue()));
    }
}
