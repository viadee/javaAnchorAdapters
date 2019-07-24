package de.viadee.xai.anchor.adapter.tabular.column;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.builder.AnchorTabularBuilderByName;
import de.viadee.xai.anchor.adapter.tabular.builder.AnchorTabularBuilderSequential;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;

import static org.junit.jupiter.api.Assertions.*;

class AutoColumnTest {

    private InputStream getTestCSV() {
        return getClass().getClassLoader().getResourceAsStream("AnchorAutoTabularTest.csv");
    }


    @Test
    public void testIntColumns() throws IOException {
        // IdAttr,StringAttr,EnumAttr,BooleanAttr,IntAttr,DoubleAttr,MixedNumAttr,MissingValNumberAttr,MissingValString
        AnchorTabular tabular = new AnchorTabularBuilderByName()
                .addColumn(new AutoColumn("IntAttr"))
                .build(getTestCSV());

        assertEquals(1, tabular.getTabularInstances()[0].getTransformedValue("IntAttr"));
        assertEquals(2, tabular.getTabularInstances()[1].getTransformedValue("IntAttr"));
    }

}