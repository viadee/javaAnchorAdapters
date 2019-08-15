package RandomSearch;

import LossFunctions.PerformanceMeasures;
import Parameter.Parameter;

import java.io.*;
import java.util.Date;

public class RandomSearchLogger {

    private final FileWriter fileWriterCSV;
    private String directory = "./rs-output/";

    public RandomSearchLogger(String scenario, ConfigurationSpace configurationSpace, PerformanceMeasures.Measure measure) {

        try {
            // create directory for random search output if not yet created
            new File(directory + scenario).mkdirs();
            File f = new File("rundata_" + new Date().getTime());
            fileWriterCSV = new FileWriter(directory + scenario + File.separator + f.getName() + ".csv");
            StringBuilder header = new StringBuilder();
            for (Parameter p : configurationSpace.getHyperparameterSpace().getHyperParameters()) {
                header.append(p.getName() + ";");
            }
            fileWriterCSV.write(header.toString() + "runtime,coverage," + measure.toString().toLowerCase() + ",best rule(s)" + "\n");

        } catch (IOException e) {
            throw new IllegalStateException("Error occurred creating file", e);
        }

    }

    public void addValuesToLogging(ConfigurationSpace configurationSpace) {

        try {
            StringBuilder values = new StringBuilder();
//            for (Parameter p : configurationSpace.getHyperparameterSpace().getHyperParameters()) {
//                values.append(p.getCurrentValue() + ";");
//            }
            values.append(configurationSpace.getRuntime() + ";");
            values.append(configurationSpace.getCoverage() + ";");
            values.append(configurationSpace.getPerformance() + ";");

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
