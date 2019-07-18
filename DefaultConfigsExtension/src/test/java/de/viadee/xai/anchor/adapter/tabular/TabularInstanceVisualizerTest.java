package de.viadee.xai.anchor.adapter.tabular;

import de.viadee.xai.anchor.adapter.tabular.builder.AnchorTabularBuilderByName;
import de.viadee.xai.anchor.adapter.tabular.column.StringColumn;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class TabularInstanceVisualizerTest {

    private InputStream getTestCSV() {
        return getClass().getClassLoader().getResourceAsStream("AnchorTabularTest.csv");
    }

    @Test
    public void testVisualizeInstance() throws IOException {
        TabularInstanceVisualizer tabularInstanceVisualizer = new TabularInstanceVisualizer();

        AnchorTabular tabular = new AnchorTabularBuilderByName()
                .addColumn(new StringColumn("name"))
                .addTargetColumn(new StringColumn("attr1"))
                .addColumn(new StringColumn("attr2"))
                .addColumn(new StringColumn("attr3"))
                .build(getTestCSV());

        String instanceOutput = tabularInstanceVisualizer.visualizeInstance(tabular.getTabularInstances()[0]);
        String expectedOutput =
                "name='testrow1'" + System.getProperty("line.separator") +
                        "attr2='anotherattr1'" + System.getProperty("line.separator") +
                        "attr3='thirdattr1'" + System.getProperty("line.separator") +
                        "WITH LABEL attr1='someattr1'";
        assertEquals(expectedOutput, instanceOutput);
    }


    // TODO test remaining code once discretization is finished

}