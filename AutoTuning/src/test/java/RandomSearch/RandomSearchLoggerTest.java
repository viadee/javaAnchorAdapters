package RandomSearch;

import LossFunctions.PerformanceMeasures;
import org.junit.Test;

import static org.junit.Assert.*;


public class RandomSearchLoggerTest {

    @Test
    public void testFileCreation(){
        //Given
        ConfigurationSpace configurationSpace = new ConfigurationSpace(HyperparameterSpace.createDefaultHyperparameterSpace());
        RandomSearchLogger logger = new RandomSearchLogger("Titanic", configurationSpace, PerformanceMeasures.Measure.ACCURACY);

        logger.endLogging();
    }

}