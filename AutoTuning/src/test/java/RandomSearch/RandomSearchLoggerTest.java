package RandomSearch;

import org.junit.Test;

import static org.junit.Assert.*;


public class RandomSearchLoggerTest {

    @Test
    public void testFileCreation(){
        //Given
        HyperparameterSpace hyperparameterSpace = new HyperparameterSpace();
        RandomSearchLogger logger = new RandomSearchLogger("Titanic", hyperparameterSpace);

        logger.endLogging();
    }

}