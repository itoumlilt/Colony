package adbm.ycsb;

import adbm.colony.IcolonyClientWrapper;
import adbm.colony.util.colonyUtil;
import adbm.main.Main;
import adbm.resultsVisualization.ResultsDialog;
import adbm.settings.ISettingsManager;
import adbm.util.AdbmConstants;
import adbm.util.EverythingIsNonnullByDefault;
import adbm.util.helpers.FileUtil;
import adbm.util.helpers.GeneralUtil;
import com.yahoo.ycsb.Client;
import eu.colonydb.colonypb.colonyPB;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.*;

import static adbm.util.helpers.FormatUtil.format;

/**
 * @author Kevin Bartik
 * Updated by Vishnu Vardhan Sundarrajan
 * Reviewed by Kevin Bartik
 */
@EverythingIsNonnullByDefault
public class colonyYCSBConfiguration
{
    private static final Logger log = LogManager.getLogger(colonyYCSBConfiguration.class);

    private static final String usedKeyTypeName = "usedKeyType";

    private colonyPB.CRDT_type usedKeyType = colonyPB.CRDT_type.COUNTER;

    public colonyPB.CRDT_type getUsedKeyType()
    {
        return usedKeyType;
    }

    public void setUsedKeyType(colonyPB.CRDT_type type)
    {
        usedKeyType = type;
        if (!Arrays.asList(colonyUtil.typeOperationMap.get(usedKeyType)).contains(usedOperation)) {
            usedOperation = colonyUtil.typeOperationMap.get(usedKeyType)[0];
        }
    }

    private static final String usedOperationName = "usedOperation";

    private String usedOperation = "increment";

    public String getUsedOperation()
    {
        return usedOperation;
    }

    public void setUsedOperation(String operation)
    {
        if (Arrays.asList(colonyUtil.typeOperationMap.get(usedKeyType)).contains(operation)) {
            usedOperation = operation;
        }
    }

    private static final String usedTransactionTypeName = "usedTransactionType";

    private IcolonyClientWrapper.TransactionType usedTransactionType = IcolonyClientWrapper.TransactionType.InteractiveTransaction;

    public IcolonyClientWrapper.TransactionType getUsedTransactionType()
    {
        return usedTransactionType;
    }

    public void setUsedTransactionType(IcolonyClientWrapper.TransactionType txType)
    {
        usedTransactionType = txType;
    }

    private static final String useTransactionsName = "useTransactions";

    private boolean useTransactions = true;

    public boolean getUseTransactions()
    {
        return useTransactions;
    }

    public void setUseTransactions(boolean useTransactions)
    {
        this.useTransactions = useTransactions;
    }

    private static final String usedWorkloadName = "usedWorkload";

    private String usedWorkload = AdbmConstants.YCSB_SAMPLE_WORKLOAD_NAME;

    public String getUsedWorkLoad()
    {
        return usedWorkload;
    }

    public void setUsedWorkload(String workload)
    {
        if (FileUtil.getAllFileNamesInFolder(AdbmConstants.YCSB_WORKLOADS_FOLDER_PATH).contains(workload))
        usedWorkload = workload;
    }

    private static final String numberOfThreadsName = "numberOfThreads";

    private int numberOfThreads = 1;

    public int getNumberOfThreads()
    {
        return numberOfThreads;
    }

    public void setNumberOfThreads(int number)
    {
        if (number > 0) numberOfThreads = number;
    }

    private static final String targetNumberName = "targetNumber";

    private int targetNumber = 0;

    public int getTargetNumber()
    {
        return targetNumber;
    }

    public void setTargetNumber(int number)
    {
        if (number >= 0) targetNumber = number;
    }

    private static final String showStatusName = "showStatus";

    private boolean showStatus = true;

    public boolean getShowStatus()
    {
        return showStatus;
    }

    public void setShowStatus(boolean status)
    {
        showStatus = status;
    }

    //TODO one time or stored?

    private boolean onlyReads = false;

    public boolean isOnlyReads()
    {
        return onlyReads;
    }

    public void setOnlyReads(boolean onlyReads)
    {
        this.onlyReads = onlyReads;
    }

    private boolean onlyUpdates = false;

    public boolean isOnlyUpdates()
    {
        return onlyUpdates;
    }

    public void setOnlyUpdates(boolean onlyUpdates)
    {
        this.onlyUpdates = onlyUpdates;
    }

    private boolean rebuildcolony(String commit)
    {
        String currentCommit = Main.getDockerManager().getCommitOfContainer(AdbmConstants.ADBM_CONTAINER_NAME).trim();
        log.trace("Current Commit: {}", currentCommit);
        log.trace("New Commit: {}", commit);
        if (commit.trim().equals(currentCommit)) {
            return true;
        }
        return Main.getDockerManager().rebuildcolonyInContainer(AdbmConstants.ADBM_CONTAINER_NAME, commit);
    }

    public static colonyYCSBConfiguration loadFromSettings()
    {
        colonyYCSBConfiguration config = new colonyYCSBConfiguration();
        colonyYCSBConfiguration defaultConfig = new colonyYCSBConfiguration();
        ISettingsManager settings = Main.getSettingsManager();
        try {
            config.usedKeyType = colonyPB.CRDT_type.valueOf(settings.getYCSBSetting(usedKeyTypeName));
        } catch (IllegalArgumentException e) {
            config.usedKeyType = defaultConfig.usedKeyType;
            settings.setYCSBSetting(usedKeyTypeName, config.usedKeyType.name());
        }
        //TODO extra check that operation exists
        config.usedOperation = settings.getYCSBSetting(usedOperationName);
        if (config.usedOperation.isEmpty() || !Arrays.asList(colonyUtil.typeOperationMap.get(config.usedKeyType))
                                                     .contains(config.usedOperation))
        {
            config.usedOperation = defaultConfig.usedOperation;
            settings.setYCSBSetting(usedOperationName, config.usedOperation);
        }
        try {
            config.usedTransactionType = IcolonyClientWrapper.TransactionType
                    .valueOf(settings.getYCSBSetting(usedTransactionTypeName));
        } catch (IllegalArgumentException e) {
            config.usedTransactionType = defaultConfig.usedTransactionType;
            settings.setYCSBSetting(usedTransactionTypeName, config.usedTransactionType.name());
        }
        String ut = settings.getYCSBSetting(useTransactionsName);
        if (!ut.equals("true") && !ut.equals("false")) {
            config.useTransactions = defaultConfig.useTransactions;
            settings.setYCSBSetting(useTransactionsName, String.valueOf(config.useTransactions));
        }
        else {
            config.useTransactions = Boolean.valueOf(ut);
        }
        config.usedWorkload = settings.getYCSBSetting(usedWorkloadName);
        //TODO extra check that workload exists
        if (config.usedWorkload.isEmpty() || !FileUtil.getAllFileNamesInFolder(AdbmConstants.YCSB_WORKLOADS_FOLDER_PATH)
                                                      .contains(config.usedWorkload))
        {
            config.usedWorkload = defaultConfig.usedWorkload;
            settings.setYCSBSetting(usedWorkloadName, config.usedWorkload);
        }
        try {
            config.numberOfThreads = Integer.valueOf(settings.getYCSBSetting(numberOfThreadsName));
        } catch (NumberFormatException e) {
            config.numberOfThreads = defaultConfig.numberOfThreads;
            settings.setYCSBSetting(numberOfThreadsName, String.valueOf(config.numberOfThreads));
        }
        try {
            config.targetNumber = Integer.valueOf(settings.getYCSBSetting(targetNumberName));
        } catch (NumberFormatException e) {
            config.targetNumber = defaultConfig.targetNumber;
            settings.setYCSBSetting(targetNumberName, String.valueOf(config.targetNumber));
        }
        String ss = settings.getYCSBSetting(showStatusName);
        if (!ss.equals("true") && !ss.equals("false")) {
            config.showStatus = defaultConfig.showStatus;
            settings.setYCSBSetting(showStatusName, String.valueOf(config.showStatus));
        }
        else {
            config.showStatus = Boolean.valueOf(ss);
        }
        return config;
    }

    public void saveInSettings()
    {
        ISettingsManager settings = Main.getSettingsManager();
        settings.setYCSBSetting(usedKeyTypeName, usedKeyType.name());
        settings.setYCSBSetting(usedOperationName, usedOperation);
        settings.setYCSBSetting(usedTransactionTypeName, usedTransactionType.name());
        settings.setYCSBSetting(useTransactionsName, String.valueOf(useTransactions));
        settings.setYCSBSetting(usedWorkloadName, usedWorkload);
        settings.setYCSBSetting(numberOfThreadsName, String.valueOf(numberOfThreads));
        settings.setYCSBSetting(targetNumberName, String.valueOf(targetNumber));
        settings.setYCSBSetting(showStatusName, String.valueOf(showStatus));
    }

    public boolean runBenchmark(Collection<String> commits)
    {
        return runBenchmark(commits.toArray(new String[0]));
    }

    public boolean runBenchmark(String... commits)
    {
        commits = new String[0]; //TODO for testing now because container cannot be rebuilt
        if (!Main.isDockerRunning) return false;
        String currentDate = new SimpleDateFormat(AdbmConstants.DATE_FORMAT_DATE).format(new Date());
        String currentTime = new SimpleDateFormat(AdbmConstants.DATE_FORMAT_TIME).format(new Date());
        String usedDB = AdbmConstants.YCSB_DB_CLASS_NAME;
        String[] threadsArg = numberOfThreads <= 1 ? new String[0] : new String[]{"-threads", format("{}",
                                                                                                     numberOfThreads)};
        String[] targetArg = targetNumber <= 0 ? new String[0] : new String[]{"-target", format("{}",
                                                                                                targetNumber)};
        String[] transactionArg = useTransactions ? new String[]{"-t"} : new String[0];
        String[] dbArg = {"-db", format("{}", usedDB)};
        String[] workloadArg = {"-P", AdbmConstants.getWorkloadPath(usedWorkload)};
        String[] statusArg = showStatus ? new String[]{"-s"} : new String[0];

        List<String> argList = new ArrayList<>();
        GeneralUtil.addIfNotEmpty(argList, threadsArg, targetArg, transactionArg, dbArg, workloadArg, statusArg);

        if (commits.length == 0) {
            commits = new String[]{null};
        }
        String resultsFolder = format("{}/{}", AdbmConstants.YCSB_RESULT_FOLDER_PATH, currentDate);
        File folder = new File(resultsFolder);
        if (!folder.exists()) {
            if (!folder.mkdirs()) {
                return false;
            }
        }
        String fileNameStart = format("{}/{}_{}", resultsFolder, AdbmConstants.YCSB_RESULT_FILE_NAME_START, currentTime);
        String fileEnd = ".csv";
        List<String> resultFiles = new ArrayList<>();
        for (String commit : commits) {
            if (commit != null) if (!rebuildcolony(commit)) return false;
            commit = Main.getDockerManager().getCommitOfContainer(AdbmConstants.ADBM_CONTAINER_NAME);
            String shortCommit = commit
                    .substring(0, Math.min(commit.length(), AdbmConstants.NUMBER_COMMIT_ABBREVIATION));
            String resultFileName = format("{}_{}{}", fileNameStart, shortCommit, fileEnd);
            resultFiles.add(resultFileName);
            String[] resultFileArg = new String[]{"-p", format("exportfile=\"{}\"", resultFileName)};
            List<String> newArgList = new ArrayList<>(argList);
            GeneralUtil.addIfNotEmpty(newArgList, resultFileArg);
            String[] ycsbArgs = newArgList.toArray(new String[0]);
            ycsbArgs[4] = AdbmConstants.getWorkloadPath("10R90U.txt");
            Client.main(ycsbArgs);

            ycsbArgs[4] = AdbmConstants.getWorkloadPath("30R70U.txt");
            Client.main(ycsbArgs);

            ycsbArgs[4] = AdbmConstants.getWorkloadPath("50R50U.txt");
            Client.main(ycsbArgs);

            ycsbArgs[4] = AdbmConstants.getWorkloadPath("70R30U.txt");
            Client.main(ycsbArgs);

            ycsbArgs[4] = AdbmConstants.getWorkloadPath("90R10U.txt");
            Client.main(ycsbArgs);

            ycsbArgs[4] = AdbmConstants.getWorkloadPath("100R.txt");
            Client.main(ycsbArgs);

            ycsbArgs[4] = AdbmConstants.getWorkloadPath("100U.txt");
            Client.main(ycsbArgs);

        }
        ResultsDialog.showResultsWindow(false, resultFiles.toArray(new String[0]));
        return true;
    }


}
