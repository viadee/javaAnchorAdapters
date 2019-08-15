package RandomSearch;

import Parameter.DiscretizerInstantiation.AmevaDiscretizerInstantiation;
import Parameter.DiscretizerInstantiation.EqualSizeDiscretizerInstantiation;
import Parameter.DiscretizerInstantiation.PercentileMedianDiscretizerInstantiation;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.builder.AnchorTabularBuilderByName;
import de.viadee.xai.anchor.adapter.tabular.column.DoubleColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IntegerColumn;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;

public class DiscretizationSpaceTest {

    private AnchorTabular setTabular() {
        try {
            return new AnchorTabularBuilderByName()
                    .addTargetColumn(IntegerColumn.fromStringInput("Survived"))
                    .addColumn(DoubleColumn.fromStringInput("Age", -1, 5))
                    .build(ClassLoader.getSystemResourceAsStream("train.csv"));
        } catch (IOException e) {
            throw new IllegalArgumentException("No file found with such name");
        }
    }

    @Test
    public void randomizeParameters() {

        // Given
        DiscretizationSpace discretizationSpace = new DiscretizationSpace();

        // Then
        for (int i = 0; i < 20; i++) {
            discretizationSpace.getRandomDiscretizer();
        }
    }

    @Test
    public void testTransferToConfigurationSpace() {
        // Given
        AnchorTabular anchorTabular = setTabular();
        DiscretizationSpace discretizationSpace = new DiscretizationSpace(anchorTabular,
                new PercentileMedianDiscretizerInstantiation(),
                new AmevaDiscretizerInstantiation(),
                new EqualSizeDiscretizerInstantiation()
        );

        // When
        String test = discretizationSpace.transferToConfigurationSpace();

        // Then
        Assert.assertTrue(!test.isEmpty());
    }
}