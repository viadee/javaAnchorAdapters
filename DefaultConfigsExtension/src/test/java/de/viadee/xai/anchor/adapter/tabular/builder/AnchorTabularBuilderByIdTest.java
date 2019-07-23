package de.viadee.xai.anchor.adapter.tabular.builder;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.column.StringColumn;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;

import static org.junit.jupiter.api.Assertions.*;

class AnchorTabularBuilderByIdTest {

    private InputStream getTestCSV() {
        return getClass().getClassLoader().getResourceAsStream("AnchorTabularTest.csv");
    }

    @Test
    public void testFailsOnNoColumnsAdded() {
        Assertions.assertThrows(IllegalArgumentException.class, () ->
                new AnchorTabularBuilderById().build(getTestCSV()));
    }

    @Test
    public void testAddsIgnoredColumnsAutomatically() throws IOException {
        AnchorTabular tabular = new AnchorTabularBuilderById()
                .addColumn(0, new StringColumn("name"))
                .build(getTestCSV(), true);
        assertEquals(tabular.getTabularInstances().length, 2);
        assertNull(tabular.getTargetColumn());
        assertEquals(tabular.getColumns().size(), 1);
        assertEquals(tabular.getColumns().get(0).getName(), "name");
        assertEquals(tabular.getTabularInstances()[1].getTransformedValue(0), "testrow2");
    }

    @Test
    public void testAddTwoColumns() throws IOException {
        AnchorTabular tabular = new AnchorTabularBuilderById()
                .addColumn(0, new StringColumn("name"))
                .addColumn(2, new StringColumn("attr2"))
                .build(getTestCSV(), true);
        assertEquals(tabular.getTabularInstances().length, 2);
        assertNull(tabular.getTargetColumn());
        assertEquals(tabular.getColumns().size(), 2);
        assertEquals(tabular.getColumns().get(0).getName(), "name");
        assertEquals(tabular.getColumns().get(1).getName(), "attr2");
        assertEquals(tabular.getTabularInstances()[1].getTransformedValue(0), "testrow2");
        assertEquals(tabular.getTabularInstances()[1].getTransformedValue(1), "anotherattr2");
    }

    @Test
    public void testAddTwoColumnsAndTarget() throws IOException {
        AnchorTabular tabular = new AnchorTabularBuilderById()
                .addColumn(0, new StringColumn("name"))
                .addColumn(2, new StringColumn("attr2"))
                .addTargetColumn(1, new StringColumn("attr1"))
                .build(getTestCSV(), true);
        assertEquals(tabular.getTabularInstances().length, 2);
        assertNotNull(tabular.getTargetColumn());
        assertEquals(tabular.getColumns().size(), 2);
        assertEquals(tabular.getColumns().get(0).getName(), "name");
        assertEquals(tabular.getColumns().get(1).getName(), "attr2");
        assertEquals(tabular.getTabularInstances()[1].getTransformedValue(0), "testrow2");
        assertEquals(tabular.getTabularInstances()[1].getTransformedValue(1), "anotherattr2");
        assertEquals(tabular.getTargetColumn().getName(), "attr1");
        assertEquals(tabular.getTabularInstances()[1].getTransformedLabel(), "someattr2");
    }



}