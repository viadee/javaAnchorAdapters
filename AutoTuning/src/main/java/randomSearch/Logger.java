package randomSearch;

import evaluationMetrics.PerformanceMeasures;
import configurationSpace.Parameter;
import configurationSpace.ConfigurationSpace;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;

import java.io.*;
import java.util.Date;
import java.util.Map;

/**
 * Logging the results of the each iteration in a .csv file
 */
public class Logger {

    private final FileWriter fileWriterCSV;

    public Logger(String fileName) {
        this("./output/", fileName, null, PerformanceMeasures.Measure.PRECISION);
    }

    public Logger(String directory, String fileName) {
        this(directory, fileName, null, PerformanceMeasures.Measure.PRECISION);
    }

    public Logger(String fileName, ConfigurationSpace configurationSpace, PerformanceMeasures.Measure measure) {
        this("./output/", fileName, configurationSpace, measure);
    }

    public Logger(String directory, String fileName, ConfigurationSpace configurationSpace, PerformanceMeasures.Measure measure) {

        directory = "./" + directory + "/";

        try {
            // create directory for random search output if not yet created
            new File(directory + fileName).mkdirs();
            File f = new File("rundata_" + new Date().getTime());
            fileWriterCSV = new FileWriter(directory + fileName + File.separator + f.getName() + ".csv");
            StringBuilder header = new StringBuilder();
            header.append(";;");
            if (configurationSpace != null) {
                for (Parameter p : configurationSpace.getHyperparameterSpace().getHyperParameters()) {
                    header.append(p.getName() + ";");
                }
            }
            fileWriterCSV.write(header.toString() + "runtime;coverage;" + measure.toString().toLowerCase() + ";best rule(s)" + "\n");

        } catch (IOException e) {
            throw new IllegalStateException("Error occurred creating file", e);
        }

    }

    /**
     * Add performance values to log file
     *
     * @param runtime the runtime of the execution
     * @param coverage the coverage of the execution
     * @param performance the quality value of the execution
     */
    public void addValuesToLogging(double runtime, double coverage, double performance) {

        try {
            String values = runtime + ";" + coverage + ";" + performance + ";";
            fileWriterCSV.write(values);
        } catch (IOException e) {
            throw new IllegalStateException("Error occurred writing in the file", e);
        }
    }

    /**
     * Add the parameters values picket out of the configuration space to log file
     *
     * @param parameters a map of selected parameters with their selected value
     */
    public void addParameterValues(Map<String, String> parameters) {
        try {
            StringBuilder values = new StringBuilder();
            for (String key : parameters.keySet()) {
                values.append(parameters.get(key) + ";");
            }
            fileWriterCSV.write(values.toString());
        } catch (IOException e) {
            throw new IllegalStateException("Error occurred writing in the file", e);
        }
    }

    /**
     * Add parameters from SMAC to the log file separated with "="
     *
     * @param parameters the map of parameters picked by SMAC
     */
    public void addSMACValues(Map<String, String> parameters) {
        try {
            StringBuilder values = new StringBuilder();

            for (String key : parameters.keySet()) {
                values.append(key +  "=" + parameters.get(key) + ";");
            }
            fileWriterCSV.write(values.toString());
        } catch (IOException e) {
            throw new IllegalStateException("Error occurred writing in the file", e);
        }
    }


    /**
     * Add the explanations created by Anchors to the log file
     *
     * @param bestExplanations the best explanation found
     */
    public void addRulesToLogging(String bestExplanations) {
        try {
            fileWriterCSV.write(bestExplanations.replace(System.getProperty("line.separator"), " - ") + ";");
        } catch (IOException e) {
            throw new IllegalArgumentException("Error occurred writing the best rules in the file", e);
        }
    }

    /**
     * Add the selected discretizers of each column to the log file
     *
     * @param discretizer the discretizers that were chosen
     */
    public void addDiscretizerValue(Map<String, Discretizer> discretizer) {
        try {
            for (String key : discretizer.keySet()) {
                fileWriterCSV.write(key + "=" + discretizer.get(key).getClass().getSimpleName() + ";");
            }
        } catch (IOException e) {
            throw new IllegalArgumentException("Error occurred writing the best rules in the file", e);
        }
    }

    /**
     * end line of logging
     */
    public void endLine() {
        try {
            fileWriterCSV.write("\n");
        } catch (IOException e) {
            throw new IllegalStateException("Error occurred writing in the file", e);
        }
    }

    /**
     * end logging by closing the file
     */
    public void endLogging() {
        try {
            fileWriterCSV.close();
        } catch (IOException e) {
            throw new IllegalStateException("Error occurred closing the file", e);
        }
    }
}
