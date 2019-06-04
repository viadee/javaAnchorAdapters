package LossFunctions.Accuracy;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.DiscretizerRelation;
import de.viadee.xai.anchor.algorithm.AnchorResult;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Rule {

    private List<Feature> feature = new ArrayList<>();
    private int label;
    private double precision;


    /**
     * @param anchor
     */
    public Rule(AnchorResult<TabularInstance> anchor) {

        if (anchor == null)
            throw new IllegalArgumentException();

        TabularInstance instance = anchor.getInstance();
        Iterator iterator = anchor.getOrderedFeatures().iterator();

        while (iterator.hasNext()) {
            Integer featureNr = (Integer) iterator.next();
            GenericColumn feature = instance.getFeatures()[featureNr];
            setFeatures(instance, feature);
        }

        // set precision
        this.precision = anchor.getPrecision();

        // set prediction value
        this.label = instance.getDiscretizedLabel();
    }

    /**
     * @param instance
     * @param feature
     * @return
     */
    private void setFeatures(TabularInstance instance, GenericColumn feature) {
        DiscretizerRelation relation = feature.getDiscretizer().unApply(instance.getValue(feature));
        switch (relation.getFeatureType()) {
            case METRIC:
                this.feature.add(new MetricFeature(feature.getName(), relation.getConditionMax(), relation.getConditionMin()));
                break;
            case CATEGORICAL:
                this.feature.add(new CategoricalFeature(feature.getName(), relation.getDiscretizedValue()));
                break;
            case UNDEFINED:
            default:
                throw new IllegalArgumentException("Feature of type " + relation.getFeatureType() + " not handled");
        }
    }

    public List<Feature> getFeature() {
        return feature;
    }

    public int getLabel() {
        return label;
    }

    public double getPrecision() {
        return precision;
    }
}
