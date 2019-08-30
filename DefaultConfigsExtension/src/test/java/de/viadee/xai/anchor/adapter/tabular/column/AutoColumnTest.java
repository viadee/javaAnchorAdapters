package de.viadee.xai.anchor.adapter.tabular.column;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.builder.AnchorTabularBuilderByName;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AutoColumnTest {

    private InputStream getTestCSV() {
        return getClass().getClassLoader().getResourceAsStream("AnchorAutoTabularTest.csv");
    }


    @Test
    public void testIntColumns() throws IOException {
        final String intAttr = "IntAttr";
        AnchorTabular tabular = new AnchorTabularBuilderByName()
                .addColumn(new AutoColumn(intAttr))
                .build(getTestCSV());

        assertEquals(1, tabular.getTabularInstances()[0].getTransformedValue(intAttr));
        assertEquals(2, tabular.getTabularInstances()[1].getTransformedValue(intAttr));
        assertEquals(IntegerColumn.class, ((AutoColumn) tabular.getColumns().get(0)).getInternalColumn().getClass());
    }

    @Test
    public void testDoubleColumns() throws IOException {
        final String doubleAttr = "DoubleAttr";
        AnchorTabular tabular = new AnchorTabularBuilderByName()
                .addColumn(new AutoColumn(doubleAttr))
                .build(getTestCSV());

        assertEquals(1.1, tabular.getTabularInstances()[0].getTransformedValue(doubleAttr));
        assertEquals(2.1, tabular.getTabularInstances()[1].getTransformedValue(doubleAttr));
        assertEquals(DoubleColumn.class, ((AutoColumn) tabular.getColumns().get(0)).getInternalColumn().getClass());
    }

    @Test
    @Disabled
    public void testBooleanColumn() throws IOException {
        final String booleanAttr = "BooleanAttr";
        AnchorTabular tabular = new AnchorTabularBuilderByName()
                .addColumn(new AutoColumn(booleanAttr))
                .build(getTestCSV());

        assertEquals(true, tabular.getTabularInstances()[0].getTransformedValue(booleanAttr));
        assertEquals(false, tabular.getTabularInstances()[1].getTransformedValue(booleanAttr));
        assertEquals(BooleanColumn.class, ((AutoColumn) tabular.getColumns().get(0)).getInternalColumn().getClass());
    }

    @Test
    @Disabled
    public void testStringColumn() throws IOException {
        // IdAttr,StringAttr,EnumAttr,BooleanAttr,IntAttr,DoubleAttr,MixedNumAttr,MissingValNumberAttr,MissingValString
        final String strAttr = "StringAttr";
        AnchorTabular tabular = new AnchorTabularBuilderByName()
                .addColumn(new AutoColumn(strAttr))
                .build(getTestCSV());

        assertEquals("This is row 1", tabular.getTabularInstances()[0].getTransformedValue(strAttr));
        assertEquals("This is another row", tabular.getTabularInstances()[1].getTransformedValue(strAttr));
        assertEquals(StringColumn.class, ((AutoColumn) tabular.getColumns().get(0)).getInternalColumn().getClass());
    }

}