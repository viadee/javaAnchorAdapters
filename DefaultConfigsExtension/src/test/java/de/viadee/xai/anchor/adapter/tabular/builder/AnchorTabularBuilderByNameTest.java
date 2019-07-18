package de.viadee.xai.anchor.adapter.tabular.builder;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.column.StringColumn;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;

import static org.junit.jupiter.api.Assertions.*;

class AnchorTabularBuilderByNameTest {


    private InputStream getTestCSV() {
        return getClass().getClassLoader().getResourceAsStream("AnchorTabularTest.csv");
    }

    @Test
    public void testFailsOnNoColumnsAdded() throws IOException {
        Assertions.assertThrows(IllegalArgumentException.class, () ->
                new AnchorTabularBuilderByName().build(getTestCSV()));
    }

    @Test
    public void testAddOneColumn() throws IOException {
        AnchorTabular tabular = new AnchorTabularBuilderByName()
                .addColumn(new StringColumn("attr3"))
                .build(getTestCSV());

        assertEquals(tabular.getTabularInstances().length, 2);
        assertNull(tabular.getTargetColumn());
        assertEquals(tabular.getColumns().size(), 1);
        assertEquals(tabular.getColumns().get(0).getName(), "attr3");
        assertEquals(tabular.getTabularInstances()[0].getTransformedValue(0), "thirdattr1");
        assertEquals(tabular.getTabularInstances()[1].getTransformedValue(0), "thirdattr2");
    }

    @Test
    public void testAddInvalidColumnName() {
        Assertions.assertThrows(IllegalArgumentException.class, () ->
                new AnchorTabularBuilderByName()
                        .addColumn(new StringColumn("notavailable"))
                        .build(getTestCSV()));
    }

    @Test
    public void testAddDuplicateColumnName() {
        Assertions.assertThrows(IllegalArgumentException.class, () ->
                new AnchorTabularBuilderByName()
                        .addColumn(new StringColumn("attr3"))
                        .addColumn(new StringColumn("attr3"))
                        .build(getTestCSV()));
    }

    @Test
    public void testAddAllDifferentOrdering() throws IOException {
        AnchorTabular tabular = new AnchorTabularBuilderByName()
                .addColumn(new StringColumn("attr3"))
                .addColumn(new StringColumn("attr1"))
                .addTargetColumn(new StringColumn("name"))
                .addColumn(new StringColumn("attr2"))
                .build(getTestCSV());

        assertEquals(tabular.getTabularInstances().length, 2);
        assertNotNull(tabular.getTargetColumn());
        assertEquals(tabular.getColumns().size(), 3);
        assertEquals(tabular.getColumns().get(0).getName(), "attr1");
        assertEquals(tabular.getColumns().get(1).getName(), "attr2");
        assertEquals(tabular.getColumns().get(2).getName(), "attr3");
        assertEquals(tabular.getTargetColumn().getName(), "name");

        assertEquals(tabular.getTabularInstances()[1].getTransformedValue(0), "someattr2");
        assertEquals(tabular.getTabularInstances()[1].getTransformedValue(1), "anotherattr2");
        assertEquals(tabular.getTabularInstances()[1].getTransformedValue(2), "thirdattr2");
        assertEquals(tabular.getTabularInstances()[1].getTransformedLabel(), "testrow2");
    }

}