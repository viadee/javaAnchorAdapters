package de.viadee.xai.anchor.adapter.tabular.builder;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.column.StringColumn;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;

import static org.junit.jupiter.api.Assertions.*;

class AnchorTabularBuilderSequentialTest {

    private InputStream getTestCSV() {
        return getClass().getClassLoader().getResourceAsStream("AnchorTabularTest.csv");
    }

    @Test
    public void testInsufficientAddColumns() {
        Assertions.assertThrows(IllegalArgumentException.class, () ->
                new AnchorTabularBuilderSequential().build(getTestCSV()));
    }

    @Test
    public void testAllIgnoredFails() {
        Assertions.assertThrows(IllegalArgumentException.class, () ->
                new AnchorTabularBuilderSequential()
                        .addIgnoredColumn()
                        .addIgnoredColumn()
                        .addIgnoredColumn()
                        .addIgnoredColumn()
                        .build(getTestCSV()));
    }

    @Test
    public void testAddOne() throws IOException {
        AnchorTabular tabular = new AnchorTabularBuilderSequential()
                .addIgnoredColumn()
                .addIgnoredColumn()
                .addColumn(new StringColumn("attr2"))
                .addIgnoredColumn()
                .build(getTestCSV(), true);

        assertEquals(tabular.getTabularInstances().length, 2);
        assertEquals(tabular.getColumns().size(), 1);
        assertNull(tabular.getTargetColumn());
        assertEquals(tabular.getColumns().get(0).getName(), "attr2");
        assertEquals(tabular.getTabularInstances()[0].getTransformedValue(0), "anotherattr1");
        assertEquals(tabular.getTabularInstances()[1].getTransformedValue(0), "anotherattr2");
    }

}