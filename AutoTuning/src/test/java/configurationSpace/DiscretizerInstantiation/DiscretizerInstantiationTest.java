package configurationSpace.DiscretizerInstantiation;

import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.AmevaDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.EqualSizeDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.FUSINTERDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.PercentileMedianDiscretizer;
import org.junit.Assert;
import org.junit.Test;

public class DiscretizerInstantiationTest {

    @Test
    public void testGetAllDiscretizersPercentileMedian() {
        // Given
        DiscretizerInstantiation discretizerInstantiation = new PercentileMedianDiscretizerInstantiation();

        // When
        Discretizer result = discretizerInstantiation.getRandomDiscretizer();

        // Then
        Assert.assertTrue(result.getClass().equals(PercentileMedianDiscretizer.class));
    }

    @Test
    public void testGetAllDiscretizersFUSINTER() {
        // Given
        DiscretizerInstantiation discretizerInstantiation = new FUSINTERDiscretizerInstantiation();

        // When
        Discretizer result = discretizerInstantiation.getRandomDiscretizer();

        // Then
        Assert.assertTrue(result.getClass().equals(FUSINTERDiscretizer.class));
    }


    @Test
    public void testGetAllDiscretizersEqualSize() {
        // Given
        DiscretizerInstantiation discretizerInstantiation = new EqualSizeDiscretizerInstantiation();

        // When
        Discretizer result = discretizerInstantiation.getRandomDiscretizer();

        // Then
        Assert.assertTrue(result.getClass().equals(EqualSizeDiscretizer.class));
    }

    @Test
    public void testGetAllDiscretizersAmeva() {
        // Given
        DiscretizerInstantiation discretizerInstantiation = new AmevaDiscretizerInstantiation();

        // When
        Discretizer result = discretizerInstantiation.getRandomDiscretizer();

        // Then
        Assert.assertTrue(result.getClass().equals(AmevaDiscretizer.class));
    }

    @Test
    public void selectRandom() {
    }

    @Test
    public void testGetClassName() {
        // Given
        DiscretizerInstantiation median = new PercentileMedianDiscretizerInstantiation();
        DiscretizerInstantiation ameva = new AmevaDiscretizerInstantiation();
        DiscretizerInstantiation equalSize = new EqualSizeDiscretizerInstantiation();
        DiscretizerInstantiation fusinter = new FUSINTERDiscretizerInstantiation();

        // When
        String medianClass = median.getClassName();
        String amevaClass = ameva.getClassName();
        String equalSizeClass = equalSize.getClassName();
        String fusinterClass = fusinter.getClassName();

        // Then
        Assert.assertEquals(PercentileMedianDiscretizer.class.getSimpleName(), medianClass);
        Assert.assertEquals(AmevaDiscretizer.class.getSimpleName(), amevaClass);
        Assert.assertEquals(EqualSizeDiscretizer.class.getSimpleName(), equalSizeClass);
        Assert.assertEquals(FUSINTERDiscretizer.class.getSimpleName(), fusinterClass);
    }

    @Test
    public void testGetChildParameterConfigPercentileMedian() {
        // Given
        DiscretizerInstantiation discretizerInstantiation = new PercentileMedianDiscretizerInstantiation();

        // When
        String result = discretizerInstantiation.getChildParameterConfig("test");

        // Then
        Assert.assertTrue(result.equals("test_PercentileMedianDiscretizer integer [1,100][10]\ntest_PercentileMedianDiscretizer|test == PercentileMedianDiscretizer\n"));
    }

    @Test
    public void testGetChildParameterConfigAmeva() {
        // Given
        DiscretizerInstantiation discretizerInstantiation = new AmevaDiscretizerInstantiation();

        // When
        String result = discretizerInstantiation.getChildParameterConfig("test");

        // Then
        Assert.assertTrue(result.equals(""));
    }

    @Test
    public void testGetChildParameterConfigEqualSize() {
        // Given
        DiscretizerInstantiation discretizerInstantiation = new EqualSizeDiscretizerInstantiation();

        // When
        String result = discretizerInstantiation.getChildParameterConfig("test");

        // Then
        Assert.assertTrue(result.equals("test_EqualSizeDiscretizer integer [1,100][10]\ntest_EqualSizeDiscretizer|test == EqualSizeDiscretizer\n"));
    }

    @Test
    public void fromParameterConfig() {
    }
}