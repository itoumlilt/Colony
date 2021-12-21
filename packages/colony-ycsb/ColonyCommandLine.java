package adbm.main;

import adbm.colony.IcolonyClientWrapper;
import adbm.settings.ISettingsManager;
import adbm.util.AdbmConstants;
import adbm.util.EverythingIsNonnullByDefault;
import adbm.ycsb.colonyYCSBConfiguration;
import eu.colonydb.colonypb.colonyPB;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.Appender;
import org.apache.logging.log4j.core.Layout;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.appender.RandomAccessFileAppender;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.Configurator;
import org.apache.logging.log4j.core.config.LoggerConfig;
import org.apache.logging.log4j.core.config.NullConfiguration;
import org.apache.logging.log4j.core.layout.PatternLayout;
import picocli.CommandLine;

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;

/**
 * @author Alka Scaria
 * Reviewed by Kevin Bartik
 */
@EverythingIsNonnullByDefault
@CommandLine.Command(name = "colony Benchmark", footer = "Copyright(c) 2018", sortOptions = false,
        description = "Run the colony Benchmark from the Console. Benchmarks are performed five times for each commit and the last 3 benchmark results of each commit are averaged for comparison (Compensates a little bit for JVM and colony warm up time). Can take a few minutes for each commit. If logging is active progress will be shown in the console.")
public class colonyCommandLine
{
    private static final Logger log = LogManager.getLogger(colonyCommandLine.class);

    @CommandLine.Option(names = {"-help", "-version"}, description = "Shows version and help. Exits afterwards.")
    private boolean help;

    @CommandLine.Option(names = {"-info", "-currentsettings"}, description = "Shows information of the current settings. Shows set file paths and current benchmark settings. Exits afterwards.")
    private boolean info;

    @CommandLine.Option(names = {"-resetsettings"}, description = "Resets all settings stored in the persistent store of this application. (Git folder path, Commits, Custom Configurations...) Exits afterwards.")
    private boolean resetSettings;

    @CommandLine.Option(names = {"-gui"}, description = "Starts the GUI of the colony Benchmark which allows more configuration. Ignores further command line options.")
    private boolean gui;

    @CommandLine.Option(names = {"-norerun"}, description = "Resets benchmark settings from previous benchmark to default settings. Only applies to benchmark settings that require a value. Settings are overwritten if they are specified in the command line.")
    private boolean norerun;

    @CommandLine.Option(names = {"-runagain"}, description = "Runs the exact same benchmark as before. All command line options besides -commits are ignored.")
    private boolean runagain;

    @CommandLine.Option(names = {"-nologging"}, description = "Disables all logging. Not recommended.")
    private boolean noLogging;

    @Nullable
    @CommandLine.Option(names = {"-logfile"}, description = "Sets the log file path. Logs are always created in the folder of this application but an additional log file can be set. Default: None")
    private File logfile;

    @Nullable
    @CommandLine.Option(names = {"-loglevel"}, description = "Defines the log level (ERROR, WARN, INFO, DEBUG, TRACE) of the logs that are written to the specified log file. Default: DEBUG")
    private String logLevel = "DEBUG";

    @CommandLine.Option(names = {"-checkcommits", "-usegit"}, description = "Checks that the provided commits exist. Requires a local git repository of colony to be cloned. The location of the git repository can be set with another option.")
    private boolean checkCommits;

    @Nullable
    @CommandLine.Option(names = {"-gitfolder"}, description = "Sets the git folder where the colony repository is cloned. Can be an existing colony repository but it must be in a clean state for this application to work properly. The folder path is stored in a persistent database and only needs to be set once. Default: The application directory.")
    private File gitfolder;

    @CommandLine.Option(names = {"-autofetch"}, description = "Fetches the newest updates of the colony repository. The git repository must be in a clean state. It might be necessary to fetch the newest updates if new commits are benchmarked.")
    private boolean autoFetch;


    @CommandLine.Option(names = {"-fastbench"}, description = "Performs a fast benchmark that is less reliable. Performs three benchmarks for each commit a takes the result of the last benchmark for each commit.")
    private boolean fastBenchmark;

    @CommandLine.Option(names = {"-superfastbench"}, description = "Like -fastbench with a reduced set of data.")
    private boolean superFastBenchmark;

    @CommandLine.Option(names = {"-onlyreads"}, description = "Performs a benchmark that only performs reads.")
    private boolean onlyReads;

    @CommandLine.Option(names = {"-onlyupdates"}, description = "Performs a benchmark that only performs updates.")
    private boolean onlyUpdates;

    @Nullable
    @CommandLine.Option(names = {"-keytype"}, description = "Choose the key type that should be benchmarked. Options are\nRegister, MVRegister, Counter, FatCounter, Integer, Set.\nDefaults to Counter.")
    private String keyType;

    @Nullable
    @CommandLine.Option(names = {"-operation"}, description = "Choose the operation that should be benchmarked. Options are\n reset for all key types;\nincrement, decrement for Counter, FatCounter and Integer;\nassign for Register, MVRegister;\nset for Integer;\nadd for Set.\nDefaults to the default operation (usually the first one e.g. increment).")
    private String operation;

    @Nullable
    @CommandLine.Option(names = {"-transactiontype"}, description = "Choose the transaction type that should be benchmarked. Options are Interactive, Static, NoTransaction. Default is Interactive.")
    private String transactionType;

    @CommandLine.Option(names = {"-threads"}, description = "The number of threads that should be used in the benchmark. Default is 1 Thread. If a previous benchmark is rerun (default) then the previous number of threads is used.")
    private int threads; //Just for parsing

    @Nullable
    @CommandLine.Option(names = "-commits", arity = "*", description = "Define the commits you want to benchmark. The defined commits are stored persistently for re-runs and are overwritten if new commits are provided. Default: Commits from the previous benchmark or none if no benchmark where performed.")
    private String[] commits;

    public int parseInput()
    {
        ISettingsManager settings = Main.getSettingsManager();
        colonyYCSBConfiguration config = Main.getcolonyYCSBConfiguration();
        if (help) {
            log.info("{} Version: {}", AdbmConstants.APP_NAME, AdbmConstants.APP_VERSION);
            CommandLine.usage(new colonyCommandLine(), System.out);
            return 0;
        }
        if (info) {
            //TODO show settings
        }
        if (resetSettings) {
            Main.getSettingsManager().resetAllSettings();
            return 0;
        }
        if (gui) {
            return 1;
        }
        if (noLogging) {
            Configurator.initialize(new NullConfiguration());
        }
        else {
            /*String actualLogFile = null;
            //TODO create file
            if (logfile != null) {
                try {
                    if (logfile.exists()) {

                        actualLogFile = logfile.getCanonicalPath();
                    }else {
                        if (!logfile.getParentFile().exists()) {
                            if (!logfile.getParentFile().mkdirs()) {

                            }
                        }
                    }
                } catch (IOException e) {
                    log.error("Could not log to the specified log file.", e);
                    actualLogFile = null;
                }
            }
            if (actualLogFile == null) {
                actualLogFile = settings.getAppSetting(ISettingsManager.LOG_FOLDER_PATH_SETTING);
            }
            if (!actualLogFile.isEmpty() && new File(actualLogFile).exists()) {
                addFileAppender("colonyBenchmarkAppender", actualLogFile, Level.toLevel(logLevel, Level.DEBUG),
                                false);
                settings.setAppSetting(ISettingsManager.LOG_FOLDER_PATH_SETTING, actualLogFile);
            }*/
        }
        if (checkCommits) {
            //TODO
            String folder = null;
            if (gitfolder != null) {
                try {
                    folder = gitfolder.getCanonicalPath();
                } catch (IOException e) {
                    log.error("Could not use the specified folder for git!");
                }
            }
            Main.getGitManager().start(folder, String.valueOf(autoFetch));
        }
        if (runagain) {
            if (commits != null && commits.length != 0) {
                config.runBenchmark(commits);
            }
            else {
                config.runBenchmark((String[]) settings.getBenchmarkCommits().toArray());
            }
        }
        else {
            if (norerun) {
                config = new colonyYCSBConfiguration();
            }
            config.setOnlyReads(onlyReads);
            config.setOnlyUpdates(onlyUpdates); //TODO overwritten
            if (threads > 0) config.setNumberOfThreads(threads);
            if (keyType != null) config.setUsedKeyType(getKeyType());
            if (operation != null) config.setUsedOperation(operation);
            if (transactionType != null) config.setUsedTransactionType(getTransactionType());
            if (superFastBenchmark) {
                fastBenchmark = true;
                //TODO
            }
            if (fastBenchmark) {
                //TODO
            }
            if (commits != null && commits.length != 0) {
                config.runBenchmark(commits);
            }
            else {
                config.runBenchmark(settings.getBenchmarkCommits());
            }
        }
        return 2;
    }

    private IcolonyClientWrapper.TransactionType getTransactionType()
    {
        if (transactionType == null) {
            return IcolonyClientWrapper.TransactionType.InteractiveTransaction;
        }
        switch (transactionType) {
            case "Interactive":
                return IcolonyClientWrapper.TransactionType.InteractiveTransaction;
            case "Static":
                return IcolonyClientWrapper.TransactionType.StaticTransaction;
            case "NoTransaction":
                return IcolonyClientWrapper.TransactionType.NoTransaction;
            default:
                return IcolonyClientWrapper.TransactionType.InteractiveTransaction;
        }
    }

    private colonyPB.CRDT_type getKeyType()
    {
        if (keyType == null) {
            return colonyPB.CRDT_type.COUNTER;
        }
        switch (keyType) {
            case "FatCounter":
                return colonyPB.CRDT_type.FATCOUNTER;
            case "Counter":
                return colonyPB.CRDT_type.COUNTER;
            case "Register":
                return colonyPB.CRDT_type.LWWREG;
            case "MVRegister":
                return colonyPB.CRDT_type.MVREG;
            case "Integer":
                return colonyPB.CRDT_type.INTEGER;
            case "Set":
                return colonyPB.CRDT_type.ORSET;
            default:
                return colonyPB.CRDT_type.COUNTER;
        }
    }

    private static void addFileAppender(String appenderName, String fileName, Level logLevel, boolean onlyTime)
    {
        final LoggerContext ctx = (LoggerContext) LogManager.getContext(false);
        final Configuration config = ctx.getConfiguration();
        String datePattern = onlyTime ? "%d{ABSOLUTE}" : "%d{ISO8601}";
        Layout layout = PatternLayout.newBuilder().withPattern("[%-5level] " + datePattern + " [%t] %c{1} - %msg%n")
                                     .build();
        Appender appender = RandomAccessFileAppender.newBuilder().setFileName(fileName).setAppend(true)
                                                    .withName(appenderName).withImmediateFlush(false).withLayout(layout)
                                                    .build();
        appender.start();
        config.addAppender(appender);
        LoggerConfig loggerConfig = config.getRootLogger();
        loggerConfig.addAppender(appender, logLevel, null);
        ctx.updateLoggers();
    }
}
