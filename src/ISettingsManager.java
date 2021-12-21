package adbm.settings;

import adbm.util.AdbmConstants;
import adbm.util.EverythingIsNonnullByDefault;
import adbm.util.IStartStop;

import java.util.HashSet;

/**
 *
 */
@EverythingIsNonnullByDefault
public interface ISettingsManager extends IStartStop
{

    /**
     *
     */
    String GIT_REPO_PATH_SETTING = "GIT_REPO_PATH";

    String GIT_REPO_DEFAULT_PATH = AdbmConstants.DEFAULT_AD_GIT_REPO_PATH;

    String LOG_FOLDER_PATH_SETTING = "LOG_FOLDER_PATH";

    String LOG_FOLDER_DEFAULT_PATH = AdbmConstants.DEFAULT_LOG_FOLDER_PATH;

    default String checkDefaultSetting(String settingName, String result)
    {
        if (!result.isEmpty()) return result;
        switch (settingName) {
            case GIT_REPO_PATH_SETTING:
                return GIT_REPO_DEFAULT_PATH;
            case LOG_FOLDER_PATH_SETTING:
                return LOG_FOLDER_DEFAULT_PATH;
            default:
                return result;
        }
    }

    String getAllSettings();

    boolean resetAllSettings();

    /**
     * @param settingName
     * @return
     */
    String getYCSBSetting(String settingName);

    /**
     * @param settingName
     * @param value
     * @return
     */
    boolean setYCSBSetting(String settingName, String value);

    /**
     * @return
     */
    boolean resetYCSBSettings();

    /**
     * @param settingName
     * @return
     */
    String getAppSetting(String settingName);

    /**
     * @param settingName
     * @param value
     * @return
     */
    boolean setAppSetting(String settingName, String value);

    /**
     * @return
     */
    boolean resetAppSettings();

    /**
     * @return
     */
    HashSet<String> getBenchmarkCommits();

    /**
     * @param commitId
     * @return
     */
    boolean addBenchmarkCommit(String commitId);

    /**
     * @param commitId
     * @return
     */
    boolean removeBenchmarkCommit(String commitId);

    /**
     * @return
     */
    boolean resetBenchmarkCommits();

}
