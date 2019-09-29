package randomSearch;

import configurationSpace.DiscretizationSpace;
import configurationSpace.DiscretizerInstantiation.AmevaDiscretizerInstantiation;
import configurationSpace.DiscretizerInstantiation.DiscretizerInstantiation;
import configurationSpace.DiscretizerInstantiation.EqualSizeDiscretizerInstantiation;
import configurationSpace.DiscretizerInstantiation.PercentileMedianDiscretizerInstantiation;
import dataInitialization.impl.TitanicInitializer;
import org.junit.Assert;
import org.junit.Test;

public class DiscretizationSpaceTest {

    @Test
    public void randomizeParameters() {

        // Given
        configurationSpace.DiscretizationSpace discretizationSpace = new configurationSpace.DiscretizationSpace();

        // Then
        for (int i = 0; i < 20; i++) {
            discretizationSpace.getRandomDiscretizer();
        }
    }

    @Test
    public void testTransferToConfigurationSpace() {
        // Given
        configurationSpace.DiscretizationSpace discretizationSpace = new DiscretizationSpace(
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