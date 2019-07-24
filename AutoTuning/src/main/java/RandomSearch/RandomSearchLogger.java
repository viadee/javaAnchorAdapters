package RandomSearch;

import LossFunctions.PerformanceMeasures;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.algorithm.AnchorResult;

import java.io.*;
import java.util.Date;
import java.util.List;

public class RandomSearchLogger {

    private final FileWriter fileWriterCSV;
    private String directory = "./rs-output/";

    public RandomSearchLogger(String scenario, HyperparameterSpace hyperparameterSpace, PerformanceMeasures.Measure measure) {

        try {
            // create directory for random search output if not yet created
            new File(directory + scenario).mkdirs();
            File f = new File("rundata_" + new Date().getTime());
            fileWriterCSV = new FileWriter(directory + scenario + File.separator + f.getName() + ".csv");
            StringBuilder header = new StringBuilder();
            for (Parameter p : hyperparameterSpace.getHyperParameters()) {
                header.append(p.getName() + ";");
            }
            fileWriterCSV.write(header.toString() + "runtime,coverage," + measure + ",best rule(s)" + "\n");

        } catch (IOException e) {
            throw new IllegalStateException("Error occurred creating file", e);
        }

    }

    public void addValuesToLogging(HyperparameterSpace hyperparameterSpace) {

        try {
            StringBuilder values = new StringBuilder();
            for (Parameter p : hyperparameterSpace.getHyperParameters()) {
                values.append(p.getCurrentValue() + ";");
            }
            values.append(hyperparameterSpace.getRuntime() + ";");
            values.append(hyperparameterSpace.getCoverage() + ";");
            values.append(hyperparameterSpace.getPerformance() + ";");

            fileWriterCSV.write(values.toString());
        } catch (IOException e) {
            throw new IllegalStateException("Error occurred writing in the file", e);
        }

    }

    public void addRulesToLogging(String bestExplanations) {
        try {
            fileWriterCSV.write(bestExplanations.replace(System.getProperty("line.separator"), " - "));
        } catch (IOException e) {
            throw new IllegalArgumentException("Error occurred writing the best rules in the file", e);
        }
    }

    public void endLine() {
        try {
            fileWriterCSV.write("\n");
        } catch (IOException e) {
            throw new IllegalStateException("Error occurred writing in the file", e);
        }
    }

    public void endLogging() {
        try {
            fileWriterCSV.close();
        } catch (IOException e) {
            throw new IllegalStateException("Error occurred closing the file", e);
        }
    }
}
