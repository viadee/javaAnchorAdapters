package RandomSearch;

import Parameter.DiscretizerInstantiation.AmevaDiscretizerInstantiation;
import Parameter.DiscretizerInstantiation.DiscretizerInstantiation;
import Parameter.DiscretizerInstantiation.EqualSizeDiscretizerInstantiation;
import Parameter.DiscretizerInstantiation.PercentileMedianDiscretizerInstantiation;
import DataInitialization.impl.TitanicInitializer;
import org.junit.Assert;
import org.junit.Test;

public class DiscretizationSpaceTest {

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
        DiscretizationSpace discretizationSpace = new DiscretizationSpace(
                (DiscretizerInstantiation) new TitanicInitializer(),
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