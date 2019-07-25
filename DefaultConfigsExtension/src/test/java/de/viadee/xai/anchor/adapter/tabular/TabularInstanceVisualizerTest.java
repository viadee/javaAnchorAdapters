package de.viadee.xai.anchor.adapter.tabular;

import de.viadee.xai.anchor.adapter.tabular.builder.AnchorTabularBuilderByName;
import de.viadee.xai.anchor.adapter.tabular.column.IntegerColumn;
import de.viadee.xai.anchor.adapter.tabular.column.StringColumn;
import de.viadee.xai.anchor.algorithm.AnchorCandidate;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collections;

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

    @Test
    public void testVisualizeResult() throws IOException {
        TabularInstanceVisualizer tabularInstanceVisualizer = new TabularInstanceVisualizer();
        AnchorTabular tabular = new AnchorTabularBuilderByName()
                .addColumn(new StringColumn("name"))
                .addTargetColumn(new StringColumn("attr1"))
                .addColumn(new StringColumn("attr2"))
                .addColumn(new StringColumn("attr3"))
                .addColumn(IntegerColumn.fromStringInput("numAttr", 1))
                .build(getTestCSV());

        AnchorCandidate candidateName = new AnchorCandidate(Collections.singletonList(0));
        candidateName.registerSamples(100, 10);
        candidateName.setCoverage(0.8);
        AnchorCandidate candidateNum = new AnchorCandidate(Arrays.asList(0,3), candidateName);
        candidateNum.registerSamples(100, 30);
        candidateNum.setCoverage(0.6);
        AnchorCandidate anchorCandidateAttr3 = new AnchorCandidate(Arrays.asList(0,3,2), candidateNum);
        anchorCandidateAttr3.registerSamples(100, 50);
        anchorCandidateAttr3.setCoverage(0.4);
        AnchorCandidate anchorCandidateAttr2 = new AnchorCandidate(Arrays.asList(0,3,2,1), anchorCandidateAttr3);
        anchorCandidateAttr2.registerSamples(100, 100);
        anchorCandidateAttr2.setCoverage(0.2);

        AnchorResult<TabularInstance> result = new AnchorResult<>(anchorCandidateAttr2,
                tabular.getTabularInstances()[1], tabular.getTabularInstances()[1].getDiscretizedLabel().intValue(),
                true, 10, 10);

        String visualization = tabularInstanceVisualizer.visualizeResult(result);
        
        String expected = "IF name = 'testrow2' {0.1, -0.19} AND " + System.lineSeparator() +
                "numAttr IN [10, 20) {0.2, -0.2} AND " + System.lineSeparator() +
                "attr3 = 'thirdattr2' {0.2, -0.19} AND " + System.lineSeparator() +
                "attr2 = 'anotherattr2' {0.5, -0.2}" + System.lineSeparator() +
                "THEN PREDICT [someattr2] (1)" + System.lineSeparator() +
                "WITH PRECISION 1 AND COVERAGE 0.2";

        assertEquals(expected, visualization);
    }

}