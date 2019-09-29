package configurationSpace.DiscretizerInstantiation;

import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.AmevaDiscretizer;

import java.util.List;
import java.util.Map;

public class AmevaDiscretizerInstantiation implements DiscretizerInstantiation<AmevaDiscretizer> {

    @Override
    public AmevaDiscretizer getRandomDiscretizer() {
        return new AmevaDiscretizer();
    }

    @Override
    public AmevaDiscretizer getDefaultDiscretizer() {
        return new AmevaDiscretizer();
    }

    @Override
    public String getChildParameterConfig(String parentName) {
        return "";
    }

    @Override
    public String getClassName() {
        return AmevaDiscretizer.class.getSimpleName();
    }

    @Override
    public AmevaDiscretizer constructDiscretizer(List<Map.Entry<String, String>> parameters) {
        return new AmevaDiscretizer();
    }
}
