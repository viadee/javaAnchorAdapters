package de.viadee.xai.anchor.adapter.tabular.discretizer.impl;

import org.junit.jupiter.api.Test;

import java.io.Serializable;

import static org.junit.jupiter.api.Assertions.*;

class AmevaDiscretizerTest {

    @Test
    void AmevaStepOneTest(){
        Number[][] values = new Number[][]{
                {1.0, 0},
                {2.0, 0},
                {3.0, 0},
                {4.0, 0},
                {5.0, 0},
                {6.0, 1},
                {7.0, 1},
                {8.0, 1},
                {9.0, 1},
                {10.0, 1}
        };
        Serializable[] serializables = new Serializable[values.length];
        Double[] doubles = new Double[values.length];
        for (int i = 0; i < values.length; i++) {
            serializables[i] = values[i][0];
            doubles[i] = values[i][1].doubleValue();
        }
        AmevaDiscretizer amevaDiscretizer = new AmevaDiscretizer();


        amevaDiscretizer.fit(serializables, doubles);
        assertEquals(1, 1);
    }
}