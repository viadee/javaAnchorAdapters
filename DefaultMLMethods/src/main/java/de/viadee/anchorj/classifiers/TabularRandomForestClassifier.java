package de.viadee.anchorj.classifiers;

import de.viadee.anchorj.ClassificationFunction;
import de.viadee.anchorj.tabular.FeatureValueMapping;
import de.viadee.anchorj.tabular.TabularFeature;
import de.viadee.anchorj.tabular.TabularInstance;
import de.viadee.anchorj.tabular.TabularInstanceList;
import smile.data.Attribute;
import smile.data.NominalAttribute;
import smile.data.NumericAttribute;
import smile.feature.FeatureGenerator;

import java.io.Serializable;
import java.util.Map;
import java.util.stream.Stream;

/**
 * Classifier for predicting tabular data
 */
public class TabularRandomForestClassifier implements ClassificationFunction<TabularInstance> {

    private final FeatureEncoder encoder;
    private final RandomForestClassifier randomForestClassifier;

    /**
     * Creates a random forest classifier specifically for tabular data
     *
     * @param tabularInstances the tabularInstances
     * @param mappings         the mappings
     */
    public TabularRandomForestClassifier(TabularInstanceList tabularInstances,
                                         Map<TabularFeature, Map<Object, FeatureValueMapping>> mappings) {
        this.encoder = new FeatureEncoder(tabularInstances, mappings);
        this.randomForestClassifier = new RandomForestClassifier(50);
        this.randomForestClassifier.fit(
                encoder.encode(tabularInstances.getInstances().stream().map(TabularInstance::getInstance).toArray(Object[][]::new)),
                tabularInstances.getLabels().stream().mapToInt(i -> i).toArray());
    }


    private int predict(Object[] value) {
        return this.predictMultiple(new Object[][]{value})[0];
    }

    private int[] predictMultiple(Object[][] values) {
        final double[][] encodedInstance = encoder.encode(values);
        return randomForestClassifier.predictMultiple(encodedInstance);
    }

    @Override
    public int predict(TabularInstance instance) {
        return this.predict(instance.getInstance());
    }

    /**
     * Feature Encoder for tabular data using a OneHot encoding
     */
    private class FeatureEncoder implements Serializable {

        private final SerializableOneHotEncoder encoder;

        /**
         * Instantiates a new Feature encoder.
         *
         * @param tabularInstances the tabular instances
         * @param mappings         the mappings
         */
        public FeatureEncoder(TabularInstanceList tabularInstances, Map<TabularFeature, Map<Object, FeatureValueMapping>> mappings) {
            Attribute[] attributes = new Attribute[tabularInstances.getFeatures().size()];

            for (int i = 0; i < attributes.length; i++) {
                TabularFeature feature = tabularInstances.getFeatures().get(i);
                if (feature.getColumnType() == TabularFeature.ColumnType.CATEGORICAL) {
                    attributes[i] = new NominalAttribute("attribute" + i + ", " + feature.getName(),
                            mappings.get(feature).values().stream().map(FeatureValueMapping::getValue).map(String::valueOf).toArray(String[]::new));
                } else {
                    attributes[i] = new NumericAttribute("attribute" + i + ", " + feature.getName());
                }

            }
            this.encoder = new SerializableOneHotEncoder(attributes);
        }

        /**
         * Encode tabular data
         *
         * @param values the values
         * @return the double matrix
         */
        double[][] encode(Object[][] values) {
            double[][] fitData = new double[values.length][values[0].length];
            for (int i = 0; i < values.length; i++) {
                double[] x = Stream.of(values[i])
                        .mapToDouble(d -> (d instanceof Integer) ? ((Integer) d).doubleValue() : (double) d).toArray();
                double[] features = encoder.feature(x);
                fitData[i] = features;
            }
            return fitData;
        }

        // Copied from library
        private class SerializableOneHotEncoder implements FeatureGenerator<double[]>, Serializable {
            private Attribute[] attributes;
            private Attribute[] features;

            SerializableOneHotEncoder(Attribute[] attributes) {
                this.attributes = attributes;
                int p = 0;
                Attribute[] var3 = attributes;
                int j = attributes.length;

                for (int var5 = 0; var5 < j; ++var5) {
                    Attribute attribute = var3[var5];
                    if (attribute instanceof NominalAttribute) {
                        NominalAttribute nominal = (NominalAttribute) attribute;
                        p += nominal.size();
                    } else {
                        ++p;
                    }
                }

                this.features = new Attribute[p];
                int i = 0;

                for (j = 0; j < attributes.length; ++j) {
                    Attribute attribute = attributes[j];
                    if (attribute instanceof NominalAttribute) {
                        NominalAttribute nominal = (NominalAttribute) attribute;
                        double weight = nominal.getWeight();
                        String name = nominal.getName();
                        String description = nominal.getDescription();

                        for (int k = 0; k < nominal.size(); ++i) {
                            this.features[i] = new NumericAttribute(name + "_" + k, description, weight);
                            ++k;
                        }
                    } else {
                        this.features[i++] = attribute;
                    }
                }

            }

            public Attribute[] attributes() {
                return this.features;
            }

            public double[] feature(double[] x) {
                if (x.length != this.attributes.length) {
                    throw new IllegalArgumentException(String.format("Invalid vector size %d, expected %d", x.length, this.attributes.length));
                } else {
                    double[] y = new double[this.features.length];
                    int i = 0;

                    for (int j = 0; j < this.attributes.length; ++j) {
                        Attribute attribute = this.attributes[j];
                        if (attribute instanceof NominalAttribute) {
                            NominalAttribute nominal = (NominalAttribute) attribute;
                            y[i + (int) x[j]] = 1.0D;
                            i += nominal.size();
                        } else {
                            y[i++] = x[j];
                        }
                    }

                    return y;
                }
            }
        }
    }
}
