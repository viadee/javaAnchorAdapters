package DataInitialization;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;

import java.util.Map;

public interface DataInitializer {

    AnchorTabular createTabular(Map<String, Discretizer> discretizers);

}
