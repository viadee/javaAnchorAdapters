package Parameter.DiscretizerInstantiation;

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
        Discretizer[] result = discretizerInstantiation.getAllDiscretizers();

        // Then
        Assert.assertTrue(result.length == 99);
    }

    @Test
    public void testGetAllDiscretizersFUSINTER() {
        // Given
        DiscretizerInstantiation discretizerInstantiation = new FUSINTERDiscretizerInstantiation();

        // When
        Discretizer[] result = discretizerInstantiation.getAllDiscretizers();

        // Then
        Assert.assertTrue(result.length == 100 * 100);
    }


    @Test
    public void testGetAllDiscretizersEqualSize() {
        // Given
        DiscretizerInstantiation discretizerInstantiation = new EqualSizeDiscretizerInstantiation();

        // When
        Discretizer[] result = discretizerInstantiation.getAllDiscretizers();

        // Then
        Assert.assertTrue(result.length == 99);
    }

    @Test
    public void testGetAllDiscretizersAmeva() {
        // Given
        DiscretizerInstantiation discretizerInstantiation = new AmevaDiscretizerInstantiation();

        // When
        Discretizer[] result = discretizerInstantiation.getAllDiscretizers();

        // Then
        Assert.assertTrue(result.length == 1);
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
        Assert.assertTrue(result.equals("test_PercentileMedianDiscretizer_classCount integer [1,100][10]\ntest_PercentileMedianDiscretizer_classCount|test == PercentileMedianDiscretizer\n"));
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
        Assert.assertTrue(result.equals("test_EqualSizeDiscretizer_classSize integer [1,100][10]\ntest_EqualSizeDiscretizer_classSize|test == EqualSizeDiscretizer\n"));
    }

    @Test
    public void fromParameterConfig() {
    }
}