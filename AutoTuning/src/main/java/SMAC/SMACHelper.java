package SMAC;

import RandomSearch.ConfigurationSpace;
import ca.ubc.cs.beta.aeatk.algorithmexecutionconfiguration.AlgorithmExecutionConfiguration;
import ca.ubc.cs.beta.aeatk.misc.watch.AutoStartStopWatch;
import ca.ubc.cs.beta.aeatk.misc.watch.StopWatch;
import ca.ubc.cs.beta.aeatk.options.AbstractOptions;
import ca.ubc.cs.beta.aeatk.parameterconfigurationspace.ParameterConfiguration;
import ca.ubc.cs.beta.aeatk.parameterconfigurationspace.ParameterConfigurationSpace;
import ca.ubc.cs.beta.aeatk.probleminstance.InstanceListWithSeeds;
import ca.ubc.cs.beta.aeatk.probleminstance.ProblemInstanceOptions;
import ca.ubc.cs.beta.aeatk.random.SeedableRandomPool;
import ca.ubc.cs.beta.aeatk.runhistory.RunHistory;
import ca.ubc.cs.beta.aeatk.smac.SMACOptions;
import ca.ubc.cs.beta.aeatk.targetalgorithmevaluator.TargetAlgorithmEvaluator;
import ca.ubc.cs.beta.aeatk.termination.TerminationCondition;
import ca.ubc.cs.beta.aeatk.trajectoryfile.TrajectoryFileEntry;
import ca.ubc.cs.beta.smac.builder.SMACBuilder;
import ca.ubc.cs.beta.smac.configurator.AbstractAlgorithmFramework;
import de.viadee.xai.anchor.algorithm.util.ParameterValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.text.DecimalFormat;
import java.util.List;
import java.util.Map;

public class SMACHelper {
    private static final Logger LOGGER = LoggerFactory.getLogger(SMACHelper.class);

    private static InstanceListWithSeeds trainingILWS;
    private static InstanceListWithSeeds testingILWS;

    private Map<String, AbstractOptions> taeOptions;
    private SeedableRandomPool pool;
    private SMACBuilder smacBuilder;

    // Variables from the Builder
    private ConfigurationSpace configurationSpace;
//     private String outputDir = "C:\\Users\\B96\\IdeaProjects\\javaAnchorAdapters\\AutoTuning\\smac-output\\";
    private String scenario;
    private SMACOptions options;
    private String outputDir;
    private TargetAlgorithmEvaluator tae;
    private double cutOffTime;

    public SMACHelper(String scenario,
                      ConfigurationSpace configurationSpace,
                      TargetAlgorithmEvaluator tae,
                      String outputDir,
                      SMACOptions smacOptions,
                      double cutOffTime) {

        if (tae == null)
            throw new IllegalArgumentException("The Target Algorithm Evaluator " + ParameterValidation.NULL_MESSAGE);
        if (configurationSpace == null)
            throw new IllegalArgumentException("Configuration space " + ParameterValidation.NULL_MESSAGE);

        this.scenario = scenario;
        this.configurationSpace = configurationSpace;
        this.outputDir = outputDir;
        this.tae = tae;
        this.options = smacOptions;
        this.cutOffTime = cutOffTime;
    }

    public void run() {

        setDefaultOptions();

        this.smacBuilder = new SMACBuilder();

        ParameterConfigurationSpace parameterConfigurationSpace = configurationSpace.toParameterConfigurationSpace();

        AlgorithmExecutionConfiguration execConfig = new AlgorithmExecutionConfiguration("", "", parameterConfigurationSpace, false, false, this.cutOffTime);

        AbstractAlgorithmFramework smac = smacBuilder.getAutomaticConfigurator(
                execConfig,
                trainingILWS,
                options,
                taeOptions,
                outputDir + scenario,
                pool,
                tae,
                null);

        StopWatch watch = new AutoStartStopWatch();

        smac.run();

        watch.stop();
        smacBuilder.getLogRuntimeStatistics().logLastRuntimeStatistics();

        ParameterConfiguration incumbent = smac.getIncumbent();
        RunHistory runHistory = smac.runHistory();
        TerminationCondition tc = smac.getTerminationCondition();

        final DecimalFormat df0 = new DecimalFormat("0");

        if (LOGGER != null) {
            LOGGER.info("\n=======================================================================================\n"
                            + "SMAC has finished. Reason: {}\n"
                            + "Total number of runs performed: {}, total configurations tried: {}.\n"
                            + "Total CPU time used: {} s, total wallclock time used: {} s.\n"
                            + "SMAC's final incumbent: config {} (internal ID: {}), with estimated {}: {}, based on {} run(s) on {} training instance(s).\n"
                            //+ "Total number of runs performed: {}, total CPU time used: {} s, total wallclock time used: {} s, total configurations tried: {}.\n"
                            + "=======================================================================================",
                    smac.getTerminationReason(),
                    runHistory.getAlgorithmRunsIncludingRedundant().size(),
                    runHistory.getAllParameterConfigurationsRan().size(),
                    df0.format(tc.getTunerTime()),
                    df0.format(tc.getWallTime()),
                    runHistory.getThetaIdx(incumbent), incumbent,
                    smac.getObjectiveToReport(),
                    smac.getEmpericalPerformance(incumbent),
                    runHistory.getAlgorithmRunsExcludingRedundant(incumbent).size(),
                    runHistory.getProblemInstancesRan(incumbent).size());
        }
//        List<TrajectoryFileEntry> tfes = smacBuilder.getTrajectoryFileLogger().getTrajectoryFileEntries();

    }

    private void setDefaultOptions() {
        // stuff so the builder does not fail
        try {
            pool = options.seedOptions.getSeedableRandomPool();
            ProblemInstanceOptions.TrainTestInstances tti =
                    options.getTrainingAndTestProblemInstances(pool, new SeedableRandomPool(options.validationSeed + options.seedOptions.seedOffset, pool.getInitialSeeds()));
            trainingILWS = tti.getTrainingInstances();
            testingILWS = tti.getTestInstances();
            taeOptions = options.scenarioConfig.algoExecOptions.taeOpts.getAvailableTargetAlgorithmEvaluators();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static InstanceListWithSeeds getTrainingILWS() {
        return trainingILWS;
    }

    public static InstanceListWithSeeds getTestingILWS() {
        return testingILWS;
    }

    public Map<String, AbstractOptions> getTaeOptions() {
        return taeOptions;
    }

    public SeedableRandomPool getPool() {
        return pool;
    }

    public SMACBuilder getSmacBuilder() {
        return smacBuilder;
    }

    public ConfigurationSpace getConfigurationSpace() {
        return configurationSpace;
    }

    public String getScenario() {
        return scenario;
    }

    public SMACOptions getOptions() {
        return options;
    }

    public String getOutputDir() {
        return outputDir;
    }

    public TargetAlgorithmEvaluator getTae() {
        return tae;
    }

    public double getCutOffTime() {
        return cutOffTime;
    }
}



