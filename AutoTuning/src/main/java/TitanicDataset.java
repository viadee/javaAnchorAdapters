package data;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.column.DoubleColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IntegerColumn;
import de.viadee.xai.anchor.adapter.tabular.column.StringColumn;
import de.viadee.xai.anchor.adapter.tabular.transformations.ReplaceEmptyTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.ReplaceNonEmptyTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;
import de.viadee.xai.anchor.adapter.tabular.util.CSVReader;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

/**
 * Loads the dataset and its definitions and prepares the {@link AnchorTabular} object
 */
public class TitanicDataset {

    /**
     * @return the {@link AnchorTabular} object that contains the training data and its definitions
     */
    public static AnchorTabular createTabularTrainingDefinition() {
        InputStream trainingDataStream = ClassLoader.getSystemResourceAsStream("Titanic/train.csv");
        if (trainingDataStream == null)
            throw new RuntimeException("Could not load data");

        try {
            return new AnchorTabular.Builder()
                    .setDoBalance(false)
                    .addIgnoredColumn("PassengerId")
                    .addTargetColumn(IntegerColumn.fromStringInput("Survived"))
                    .addColumn(IntegerColumn.fromStringInput("Pclass"))
                    .addColumn(new StringColumn("Name"))
                    .addColumn(new StringColumn("Sex"))
                    .addColumn(DoubleColumn.fromStringInput("Age", -1, 5))
                    .addColumn(IntegerColumn.fromStringInput("SibSp"))
                    .addColumn(IntegerColumn.fromStringInput("Parch"))
                    .addColumn(IntegerColumn.fromStringInput("Ticket", -1,
                            Collections.singletonList(new TicketNumberTransformer()), null))
                    .addColumn(DoubleColumn.fromStringInput("Fare", -1, 6))
                    .addColumn(new StringColumn("Cabin", Arrays.asList(
                            new ReplaceNonEmptyTransformer(true),
                            new ReplaceEmptyTransformer(false)),
                            null, null))
                    .addColumn(new StringColumn("Embarked"))
                    .build(trainingDataStream, true, false);
        } catch (IOException e) {
            throw new RuntimeException("Could not read data");
        }
    }

    /**
     * @return the {@link AnchorTabular} object that contains the test data and its definitions
     */
    public static AnchorTabular createTabularTestDefinition() {
        // The following implementation is very much similar to the above method.
        // It is contained in an own block to increase the tutorial's readability
        // Main difference: no target label is included in test set data
        InputStream trainingDataStream = ClassLoader.getSystemResourceAsStream("Titanic/test.csv");
        if (trainingDataStream == null)
            throw new RuntimeException("Could not load data");

        try {
            return new AnchorTabular.Builder()
                    .setDoBalance(false)
                    .addIgnoredColumn("PassengerId")
                    // Label is not specified in test data, so we need to skip it
                    .addColumn(IntegerColumn.fromStringInput("Pclass"))
                    .addColumn(new StringColumn("Name"))
                    .addColumn(new StringColumn("Sex"))
                    .addColumn(DoubleColumn.fromStringInput("Age", -1, 5))
                    .addColumn(IntegerColumn.fromStringInput("SibSp"))
                    .addColumn(IntegerColumn.fromStringInput("Parch"))
                    .addColumn(IntegerColumn.fromStringInput("Ticket", -1,
                            Collections.singletonList(new TicketNumberTransformer()), null))
                    .addColumn(DoubleColumn.fromStringInput("Fare", -1, 6))
                    .addColumn(new StringColumn("Cabin", Arrays.asList(
                            new ReplaceNonEmptyTransformer(true),
                            new ReplaceEmptyTransformer(false)),
                            null, null))
                    .addColumn(new StringColumn("Embarked"))
                    .build(trainingDataStream, true, false);
        } catch (IOException e) {
            throw new RuntimeException("Could not read data");
        }
    }

    private static final class TicketNumberTransformer implements Transformer {

        @Override
        public Serializable apply(Serializable serializable) {
            // Transforms the ticket column to contain only the ticket number without ticket prefixes
            String replaced = ((String) serializable).replaceAll("[^\\d]+", "");

            return ("".equals(replaced)) ? -1 : replaced;
        }
    }

    /**
     * @return the labels of the test set as specified in gender_submission.
     */
    public static int[] readTestLabels() {
        InputStream trainingDataStream = ClassLoader.getSystemResourceAsStream("Titanic/gender_submission.csv");
        if (trainingDataStream == null)
            throw new RuntimeException("Could not load data");

        try {
            final Collection<String[]> csv = CSVReader.readCSV(trainingDataStream, true);
            final int[] result = new int[csv.size() - 1];
            final Iterator<String[]> iter = csv.iterator();
            iter.next(); // Skip header entry
            int i = 0;
            while (iter.hasNext()) {
                result[i++] = Integer.parseInt(iter.next()[1]);
            }
            return result;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }


    }
}
