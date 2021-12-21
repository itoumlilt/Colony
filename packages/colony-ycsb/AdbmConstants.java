package adbm.util;

import adbm.main.Main;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static adbm.util.helpers.FormatUtil.format;

/**
 * Holds all constants that are used by the application.
 * They are mostly file paths and names.
 * Some of the constants may change in time.
 */
@EverythingIsNonnullByDefault
public class AdbmConstants
{

    private static final Logger log = LogManager.getLogger(AdbmConstants.class);

    // colony = AD
    // Benchmark = BM
    // colony Benchmark = ADBM

    private static File appPath = new File("");

    private static String canonicalAppPath = "";

    static {
        try {
            appPath = new File(Main.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath());
            canonicalAppPath = appPath.getParentFile().getCanonicalPath();
        } catch (URISyntaxException | IOException e) {
            log.error("", e); //TODO exit!
        }
    }

    private static String IDEPath() {
        String path = Main.class.getResource("Main.class").toString();
        if (path.startsWith("jar")) {
            return "";
        }
        else {
            return "classes/";
        }
    }

    public static String getJarPath(String... paths)
    {
        StringBuilder res = new StringBuilder();
        for (String path : paths) {
            res.append("/");
            res.append(path);
        }
        return res.toString();
    }

    public static File getAppPath() {
        return appPath;
    }

    private static String getAppPath(String path) {
        log.trace("AppPath: {}/{}{}", canonicalAppPath, IDEPath(), path);
        return format("{}/{}{}", canonicalAppPath, IDEPath(), path);
    }

    private static URL getResource(String path) {
        return Main.class.getResource(getJarPath(path));
    }

    /**
     * The name of the application. Used for logging and gui.
     */
    public static final String APP_NAME = "colony-bench";

    public static final String APP_VERSION = "0.0.1";

    public static final String APP_DATA_FOLDER_NAME = format("colony-bench-{}-data", APP_VERSION);

    public static final String ADBM_CONTAINER_NAME = "colonyBenchmarkContainer";

    public static final String YCSB_DB_CLASS_NAME = "adbm.ycsb.colonyYCSBClient";

    public static final String DATE_FORMAT_TIME = "HHmmssXX";

    public static final String DATE_FORMAT_DATE = "yyyyMMdd";

    public static final String DATE_FORMAT_ISO = "yyyyMMdd'T'HHmmssXX";

    public static final String YCSB_RESULT_FILE_NAME_START = "YCSB_Result";

    public static final String YCSB_RESULT_FOLDER_NAME = "YCSB_Results";

    public static final String YCSB_RESULT_FOLDER_PATH = getAppPath(YCSB_RESULT_FOLDER_NAME);

    public static final String YCSB_SAMPLE_RESULT_FILE_NAME = "SampleResult.csv";

    public static final String YCSB_SAMPLE_RESULT_PATH = format("{}/{}", YCSB_RESULT_FOLDER_PATH, YCSB_SAMPLE_RESULT_FILE_NAME);

    public static final int NUMBER_COMMIT_ABBREVIATION = 10; //Should be plenty

    public static final String YCSB_WORKLOADS_FOLDER_NAME = "YCSB_Workloads";

    /**
     * This is the path to the workloads that can be used for benchmarking with YCSB.
     */
    public static final String YCSB_WORKLOADS_FOLDER_PATH = getAppPath(YCSB_WORKLOADS_FOLDER_NAME);

    public static final String YCSB_SAMPLE_WORKLOAD_NAME = "SampleWorkload.txt";

    public static String getWorkloadPath(String workloadName) {
        return format("{}/{}", YCSB_WORKLOADS_FOLDER_PATH, workloadName);
    }

    private static final String SETTINGS_FOLDER_NAME = "Settings";

    public static final String SETTINGS_FOLDER_PATH = getAppPath(SETTINGS_FOLDER_NAME);

    /**
     * This is the path to the application settings.
     */
    public static final String APP_SETTINGS_PATH = format("{}/AppSettings", SETTINGS_FOLDER_PATH);

    /**
     * This is the path to the log4j2.xml configuration file.
     */
    public static final String LOG_SETTINGS_PATH = getAppPath("log4j2.xml"); //TODO

    public static final String DOCKER_FOLDER_NAME = "Dockerfolder";

    public static final String DOCKER_FOLDER_PATH = getAppPath(DOCKER_FOLDER_NAME);

    public static final String DOCKERFILE_PATH = format("{}/Dockerfile", DOCKER_FOLDER_PATH);

    public static final String ADBM_README_PATH = getAppPath("ADBM_README.txt");

    public static final String ADBM_README_TEXT =
            format("{}\nVersion: {}\n\nReadme\n\n" +
                           "This application is packaged as a jar file and creates several folders which store user preferences and results.\n" +
                           "The following folders belong to this application:\n\n" +
                           "{}\n" +
                           "{}\n" +
                           "{}\n" +
                        //Add new folders here!
                           "\n" +
                           "If you wish to keep the stored data when moving the application then also move these folders to the new location of the application.\n" +
                           "Other folders and files including this readme can be deleted if the application is not currently running.\n"

                    , APP_NAME, APP_VERSION, SETTINGS_FOLDER_NAME, YCSB_WORKLOADS_FOLDER_NAME, YCSB_RESULT_FOLDER_NAME);

    /**
     * This is the path to the images that are used in the gui.
     */
    private static final String IMAGES_FOLDER_NAME = "Images";

    public static final URL AD_ICON_URL = getResource(format("{}/colonyIcon.PNG", IMAGES_FOLDER_NAME));

    private static final String DEFAULT_LOG_FOLDER_NAME = "Logs";

    /**
     * This is the default path to the folder of the application logs.
     */
    public static final String DEFAULT_LOG_FOLDER_PATH = getAppPath(DEFAULT_LOG_FOLDER_NAME);

    private static final String DEFAULT_AD_GIT_REPO_FOLDER_NAME = "colonyGitRepo";

    /**
     * This is the default path to the folder of the colony git repository.
     */
    public static final String DEFAULT_AD_GIT_REPO_PATH = getAppPath(DEFAULT_AD_GIT_REPO_FOLDER_NAME);

    /**
     * This is the git url to the colony git repository.
     */
    public static final String AD_GIT_REPO_URL = "https://github.com/SyncFree/colony.git";

    /**
     * This image is required to build the colony benchmark image.
     */
    public static final String REQUIRED_IMAGE = "erlang:19";

    /**
     * This is the network that is used for the colony containers.
     */
    public static final String ADBM_DOCKER_NETWORK_NAME = "colony_ntwk";

    /**
     * This is the name of the colony benchmark image.
     * It is similar to the regular colony image.
     * However it keeps the colony git repository intact so that other commits can be checked out.
     */
    public static final String ADBM_DOCKER_IMAGE_NAME = "colonydb/colony";//"colonydb/benchmark";

    public static final String AD_DEFAULT_IMAGE_NAME = "colonydb/colony";

    /**
     * The standart client port used by colony containers.
     */
    public static final int STANDARD_ADBM_CLIENT_PORT = 8087;

    /**
     * The time to wait before killing a container that is being stopped.
     */
    public static final int SECONDS_TO_WAIT_BEFORE_KILLING = 10;

    //TODO unlimited amount (find available ports automatically)
    /**
     * The list of available host ports that can be used by colony containers.
     */
    public static final List<Integer> ADBM_HOST_PORT_LIST = new ArrayList<>(
            Arrays.asList(8087, 8088, 8089, 8090, 8091, 8092));

    //TODO change the way this works!
    /**
     * The standard colony host that is used to connect to the colony container.
     */
    public static final String ADBM_CLIENT_HOST = "localhost";

    /**
     * The number of attempts to start the git repository before cancelling the start.
     */
    public static final int NUMBER_OF_ATTEMPTS_TO_START_GIT = 3;

}
