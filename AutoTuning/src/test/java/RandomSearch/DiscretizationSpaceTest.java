package RandomSearch;

import org.junit.Test;

import static org.junit.Assert.*;

public class DiscretizationSpaceTest {

    @Test
    public void randomizeParameters() {

        // Given
        DiscretizationSpace discretizationSpace = new DiscretizationSpace();

        // Then
        for (int i = 0; i < 20; i++){
            discretizationSpace.getRandomDiscretizer();
        }
    }
}