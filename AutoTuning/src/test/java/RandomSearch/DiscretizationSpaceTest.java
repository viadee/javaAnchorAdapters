package RandomSearch;

import Parameter.Parameter;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.builder.AnchorTabularBuilderByName;
import de.viadee.xai.anchor.adapter.tabular.column.DoubleColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IntegerColumn;
import de.viadee.xai.anchor.adapter.tabular.column.StringColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.PercentileMedianDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.ReplaceEmptyTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.ReplaceNonEmptyTransformer;
import org.junit.Test;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import static org.junit.Assert.*;

public class DiscretizationSpaceTest {

    private static AnchorTabular createTabular() {

        InputStream trainingDataStream = ClassLoader.getSystemResourceAsStream("Titanic/train.csv");

        if (trainingDataStream == null)
            throw new RuntimeException("Could not load data");

        try {
            return new AnchorTabularBuilderByName()
                    .addTargetColumn(IntegerColumn.fromStringInput("Survived"))
                    .addColumn(IntegerColumn.fromStringInput("Pclass"))
                    .build(trainingDataStream, false, false);
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException("Could not read data");
        }
    }

    @Test
    public void randomizeParameters() {

        // Given
        AnchorTabular tabular = createTabular();
        DiscretizationSpace discretizationSpace = new DiscretizationSpace(tabular, new Discretizer[] {new PercentileMedianDiscretizer(5), new UniqueValueDiscretizer()});

        // Then
        for (int i = 0; i < 20; i++){
            discretizationSpace.randomizeParameters();
        }
    }
}