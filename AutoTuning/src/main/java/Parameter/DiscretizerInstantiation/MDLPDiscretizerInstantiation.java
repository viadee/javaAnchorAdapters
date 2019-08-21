package Parameter.DiscretizerInstantiation;

import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.MDLPDiscretizer;

import java.util.List;
import java.util.Map;

public class MDLPDiscretizerInstantiation implements DiscretizerInstantiation<MDLPDiscretizer> {

    @Override
    public MDLPDiscretizer getRandomDiscretizer() {
        return new MDLPDiscretizer();
    }

    @Override
    public String getClassName() {
        return MDLPDiscretizer.class.getSimpleName();
    }

    @Override
    public String getChildParameterConfig(String parentName) {
        return "";
    }

    @Override
    public MDLPDiscretizer constructDiscretizer(List<Map.Entry<String, String>> parameters) {
        return new MDLPDiscretizer();
    }

    @Override
    public MDLPDiscretizer getDefaultDiscretizer() {
        return new MDLPDiscretizer();
    }
}
