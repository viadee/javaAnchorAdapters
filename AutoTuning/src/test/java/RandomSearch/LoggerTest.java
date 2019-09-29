package RandomSearch;

import LossFunctions.PerformanceMeasures;
import org.junit.Test;


public class LoggerTest {

    @Test
    public void testFileCreation(){
        //Given
        ConfigurationSpace configurationSpace = new ConfigurationSpace(HyperparameterSpace.createDefaultHyperparameterSpace());
        Logger logger = new Logger("Titanic", configurationSpace, PerformanceMeasures.Measure.ACCURACY);

        logger.endLogging();
    }

}