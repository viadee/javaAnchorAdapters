package Parameter.DiscretizerInstantiation;

import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.RandomDiscretizer;

import java.util.List;
import java.util.Map;

public class RandomDiscretizerInstantiation  implements DiscretizerInstantiation<RandomDiscretizer> {

    @Override
    public RandomDiscretizer getRandomDiscretizer() {
        return new RandomDiscretizer();
    }

    @Override
    public String getClassName() {
        return RandomDiscretizer.class.getSimpleName();
    }

    @Override
    public String getChildParameterConfig(String parentName) {
        return "";
    }

    @Override
    public RandomDiscretizer constructDiscretizer(List<Map.Entry<String, String>> parameters) {
        return new RandomDiscretizer();
    }

    @Override
    public RandomDiscretizer getDefaultDiscretizer() {
        return new RandomDiscretizer();
    }
}

