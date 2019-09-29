package RandomSearch;

import LossFunctions.PerformanceMeasures;
import Parameter.Parameter;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;

import java.io.*;
import java.util.Date;
import java.util.Map;

public class Logger {

    private final FileWriter fileWriterCSV;

    public Logger(String fileName) {
        this("./rs-output/", fileName, null, PerformanceMeasures.Measure.PRECISION);
    }

    public Logger(String directory, String fileName) {
        this(directory, fileName, null, PerformanceMeasures.Measure.PRECISION);
    }

    public Logger(String fileName, ConfigurationSpace configurationSpace, PerformanceMeasures.Measure measure) {
        this("./rs-output/", fileName, configurationSpace, measure);
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

    public void addValuesToLogging(double runtime, double coverage, double performance) {

        try {
            String values = runtime + ";" + coverage + ";" + performance + ";";
            fileWriterCSV.write(values);
        } catch (IOException e) {
            throw new IllegalStateException("Error occurred writing in the file", e);
        }
    }

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


    public void addRulesToLogging(String bestExplanations) {
        try {
            fileWriterCSV.write(bestExplanations.replace(System.getProperty("line.separator"), " - ") + ";");
        } catch (IOException e) {
            throw new IllegalArgumentException("Error occurred writing the best rules in the file", e);
        }
    }

    public void addDiscretizerValue(Map<String, Discretizer> discretizer) {
        try {
            for (String key : discretizer.keySet()) {
                fileWriterCSV.write(key + "=" + discretizer.get(key).getClass().getSimpleName() + ";");
            }
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
