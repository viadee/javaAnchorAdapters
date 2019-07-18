package de.viadee.xai.anchor.adapter.tabular.discretizer;

import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;


/**
 * Tests the PercentileMedianDiscretizer Class with Metaanalysis using Field. Because Attributes of Discretizer are
 * private without getters
 */
@SuppressWarnings("unchecked")
class PercentileMedianDiscretizerTest {

    @Test
    void testPercentileMedianDiscretizerConstructorSCV()  throws Exception  {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(5, -1);

        Field singleClassValuesField = PercentileMedianDiscretizer.class.getDeclaredField("singleClassValues");
        Field singleClassValueRelationsField = PercentileMedianDiscretizer.class.getDeclaredField("singleClassValueRelations");
        singleClassValuesField.setAccessible(true);
        singleClassValueRelationsField.setAccessible(true);
        ArrayList<Number> scvList = (ArrayList<Number>) singleClassValuesField.get(disc);
        ArrayList<DiscretizerRelation> scvrList = (ArrayList<DiscretizerRelation>) singleClassValueRelationsField.get(disc);

        assertEquals(-1, scvList.get(0), "tests if singleClassValues are set correctly");
        assertEquals(
                new DiscretizerRelation(-1, -1.0, -1.0),
                scvrList.get(0), "tests if scvr is set to scv"
        );
    }


    @Test
    void testPercentileMedianDiscretizerConstructorNOSCV()  throws Exception {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(5);

        Field singleClassValuesField = PercentileMedianDiscretizer.class.getDeclaredField("singleClassValues");
        singleClassValuesField.setAccessible(true);
        List<Number> scvList = (List<Number>) singleClassValuesField.get(disc);
        assertTrue(scvList.isEmpty(), "tests if singleClassValues are set to emptyList if scv == null");

    }

    @Test
    void testMedianIndexValueOddAndEven()  throws Exception {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(2, -1);
        Number[] numbers = {0, 1, 1, 2, 2, 3};
        disc.fit(numbers);

        Field discretizerRelationsField = PercentileMedianDiscretizer.class.getDeclaredField("discretizerRelations");
        discretizerRelationsField.setAccessible(true);
        ArrayList<DiscretizerRelation> scvrList = (ArrayList<DiscretizerRelation>) discretizerRelationsField.get(disc);
        assertEquals(
                new DiscretizerRelation(100, 0.0, 1.0),
                scvrList.get(0), "tests if Methods gets correct median with odd Arraylength"
        );

        Number[] numbersOdd = {0, 1, 1, 1, 2, 2, 2, 3};
        disc.fit(numbersOdd);

        assertEquals(
                new DiscretizerRelation(200, 2.0, 3.0),
                scvrList.get(1), "tests if Methods gets correct median with even Arraylength"
        );
    }

    @Test
    void testRemoveDuplicateDiscretizedValues()  throws Exception {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(5, -1);
        Number[] numbersNotDistinct = {0, 0, 0, 0, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
        disc.fit(numbersNotDistinct);

        Field discretizerRelationsField = PercentileMedianDiscretizer.class.getDeclaredField("discretizerRelations");
        discretizerRelationsField.setAccessible(true);
        ArrayList<DiscretizerRelation> scvrList = (ArrayList<DiscretizerRelation>) discretizerRelationsField.get(disc);

        assertEquals(4, scvrList.size(), "tests if Relations are Merged if DiscValue is identical");

        Number[] numbersDistinct = {0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
        disc.fit(numbersDistinct);

        assertEquals(5, scvrList.size(), "tests if Relations are unmerged if DiscValue are Distinct");
    }

    @Test
    void testDistinctMinAndMaxValues() throws Exception {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(5, -1);
        Number[] numbersNotDistinct = {0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
        disc.fit(numbersNotDistinct);


        Field discretizerRelationsField = PercentileMedianDiscretizer.class.getDeclaredField("discretizerRelations");
        discretizerRelationsField.setAccessible(true);
        ArrayList<DiscretizerRelation> scvrList = (ArrayList<DiscretizerRelation>) discretizerRelationsField.get(disc);

        assertEquals(
                new DiscretizerRelation(100, 1.0, 2.0)
                , scvrList.get(1), "tests if new min is set if Min is Max of other Relation"
        );
    }

    @Test
    void testApply() {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(5, -1);
        Number[] numbers = {0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
        disc.fit(numbers);

        int discValue = disc.apply(3);
        assertEquals(2 * 100, discValue, "tests if number is applied to Relation");

        assertThrows(IllegalArgumentException.class, () ->
                disc.apply(13)
        );
    }

    @Test
    void testUnApply() {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(5, -1);
        Number[] numbers = {0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
        disc.fit(numbers);

        DiscretizerRelation discretizerRelation = disc.unApply(2 * 100);
        assertEquals(
                new DiscretizerRelation(2 * 100, 1.0, 3.0),
                discretizerRelation, "tests if unApply returns correct Relation"
        );

        assertThrows(IllegalArgumentException.class, () ->
                disc.unApply(13)
        );
    }

    @Test
    void testFitNonNumeric() {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(2, -1);
        String[] noNumbers = {"0", "0", "0", "1", "2", "3"};

        assertThrows(ClassCastException.class, () ->
                disc.fit(noNumbers)
        );
    }

    @Test
    void testFitSmallNrOfValues() throws Exception {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(10, -1);
        Number[] numbers = {0, 1, 2, 3};
        disc.fit(numbers);

        Field discretizerRelationsField = PercentileMedianDiscretizer.class.getDeclaredField("discretizerRelations");
        discretizerRelationsField.setAccessible(true);
        ArrayList<DiscretizerRelation> scvrList = (ArrayList<DiscretizerRelation>) discretizerRelationsField.get(disc);

        assertEquals(4, scvrList.size(), "tests if number of Relations is set correctly if Values > classCount");
    }

    @Test
    void testFitWithBackloggedNumbers() throws Exception {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(3, -1);
        Number[] numbers = {1, 2, 3, 4, 5, 6, 7};
        disc.fit(numbers);

        Field discretizerRelationsField = PercentileMedianDiscretizer.class.getDeclaredField("discretizerRelations");
        discretizerRelationsField.setAccessible(true);
        ArrayList<DiscretizerRelation> scvrList = (ArrayList<DiscretizerRelation>) discretizerRelationsField.get(disc);

        assertEquals(
                new DiscretizerRelation(2 * 100, 1.0, 3.0),
                scvrList.get(0), "tests if backlog is accounted for"
        );
    }

    @Test
    void testFitWithEmptyNumberArray() throws Exception {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(3, -1);
        Number[] numbers = {};

        disc.fit(numbers);

        Field discretizerRelationsField = PercentileMedianDiscretizer.class.getDeclaredField("discretizerRelations");
        discretizerRelationsField.setAccessible(true);
        ArrayList<DiscretizerRelation> scvrList = (ArrayList<DiscretizerRelation>) discretizerRelationsField.get(disc);

        assertEquals(Collections.EMPTY_LIST, scvrList, "tests if empty RelationArray is returned");
    }

    @Test
    void testFitWithSingleClass() throws Exception {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(3, -1);
        Number[] numbers = {-1, -1, -1};
        disc.fit(numbers);

        Field discretizerRelationsField = PercentileMedianDiscretizer.class.getDeclaredField("discretizerRelations");
        discretizerRelationsField.setAccessible(true);
        ArrayList<DiscretizerRelation> scvrList = (ArrayList<DiscretizerRelation>) discretizerRelationsField.get(disc);

        assertEquals(Collections.EMPTY_LIST, scvrList, "tests if empty RelationArray is returned");
    }

    @Test
    void testIfRelationMergeFalseErrorIsThrown() {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(3, false, -1);
        Number[] numbers = {0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4};

        assertThrows(IllegalArgumentException.class, () ->
                disc.fit(numbers)
        );
    }

    @Test
    void testIfRelationMergeFalseErrorIsNotThrown()  throws Exception  {
        PercentileMedianDiscretizer disc = new PercentileMedianDiscretizer(3, false, -1);
        Number[] workingNumbers = {0, 1, 2, 3, 4, 5, 6, 7, 8};

        disc.fit(workingNumbers);
        Field discretizerRelationsField = PercentileMedianDiscretizer.class.getDeclaredField("discretizerRelations");
        discretizerRelationsField.setAccessible(true);
        ArrayList<DiscretizerRelation> scvrList = (ArrayList<DiscretizerRelation>) discretizerRelationsField.get(disc);

        assertEquals(3, scvrList.size(), "tests if Exception is not thrown");

    }
}
