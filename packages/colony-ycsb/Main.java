package adbm.main;

import adbm.colony.IcolonyClientWrapper;
import adbm.colony.wrappers.colonyClientWrapperGui;
import adbm.docker.IDockerManager;
import adbm.docker.managers.DockerManager;
import adbm.git.IGitManager;
import adbm.git.managers.GitManager;
import adbm.main.ui.MainWindow;
import adbm.settings.IcolonyKeyStoreManager;
import adbm.settings.ISettingsManager;
import adbm.settings.managers.MapDBManager;
import adbm.util.AdbmConstants;
import adbm.util.EverythingIsNonnullByDefault;
import adbm.ycsb.colonyYCSBConfiguration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import picocli.CommandLine;

import javax.annotation.Nullable;
import javax.swing.*;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
@EverythingIsNonnullByDefault
public class Main
{

    private static final Logger log = LogManager.getLogger(Main.class);

    private static final Map<String, colonyClientWrapperGui> clientList = new HashMap<>();

    public static Map<String, colonyClientWrapperGui> getClientList()
    {
        return clientList;
    }

    private static boolean guiMode = false;

    public static boolean isGuiMode()
    {
        return guiMode;
    }

    public static void setGuiMode(boolean setGuiMode)
    {
        guiMode = setGuiMode;
    }

    private static IDockerManager dockerManager = DockerManager.getInstance();

    public static IDockerManager getDockerManager()
    {
        return dockerManager;
    }

    private static ISettingsManager settingsManager = MapDBManager.getInstance();

    public static ISettingsManager getSettingsManager()
    {
        return settingsManager;
    }

    private static IcolonyKeyStoreManager keyManager = MapDBManager.getInstance();

    public static IcolonyKeyStoreManager getKeyManager()
    {
        return keyManager;
    }

    private static IGitManager gitManager = GitManager.getInstance();

    public static IGitManager getGitManager()
    {
        return gitManager;
    }

    static {

        if (!FileSetup.setupFoldersAndFiles() || !settingsManager.start())
            closeApp();
    }

    private static colonyYCSBConfiguration colonyYCSBConfiguration = colonyYCSBConfiguration.loadFromSettings();

    public static colonyYCSBConfiguration getcolonyYCSBConfiguration()
    {
        return colonyYCSBConfiguration;
    }

    //public static File AppFolder = new File(Main.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath());

    public static boolean stopContainers = false;

    public static void closeApp()
    {
        if (stopContainers) dockerManager.stopAllContainers();
        dockerManager.stop();
        gitManager.stop();
        settingsManager.stop();
        keyManager.stop();
        try {
            Runtime.getRuntime().removeShutdownHook(shutdownHook);
        } catch (Exception e) {
            //Ignore
        }
        //TODO maybe delete folder
        //FileUtil.deleteDirectory(new File(AdbmConstants.DOCKER_FOLDER_PATH));
        System.exit(0);
    }

    @Nullable
    public static colonyClientWrapperGui startcolonyClient(String name, String containerName)
    {
        if (clientList.containsKey(name)) {
            colonyClientWrapperGui wrapper = clientList.get(name);
            wrapper.start();
            return wrapper;
        }
        if (dockerManager.runContainer(containerName)) {
            colonyClientWrapperGui clientWrapper = new colonyClientWrapperGui(name, containerName);
            clientList.put(name, clientWrapper);
            return clientWrapper;
        }
        return null;
    }

    public static void stopcolonyClient(String name)
    {
        if (clientList.containsKey(name)) {
            IcolonyClientWrapper wrapper = clientList.get(name);
            wrapper.stop();
        }

    }

    public static void removecolonyClient(String name)
    {
        dockerManager.removeContainer(name);
        clientList.remove(name);
    }

    public static boolean startBenchmarkContainer()
    {
        if (!dockerManager.isReady()) {
            log.trace("Starting Docker and running Benchmark Container.");
            if (!dockerManager.start()) {
                log.error("Docker could not be started!");
                return false;
            }
            if (!dockerManager.runContainer(AdbmConstants.ADBM_CONTAINER_NAME)) {
                log.error("Docker is a bad state! Please restart Docker before using this application!");
                return false;
            }
        }
        return true;
    }

    public static boolean isDockerRunning;

    private static Thread shutdownHook = new Thread(Main::closeApp);

    public static void main(String[] args)
    {
        Handler handler = new Handler();
        Thread.setDefaultUncaughtExceptionHandler(handler);
        Runtime.getRuntime().addShutdownHook(shutdownHook);
        try {
            // Set System L&F
            UIManager.setLookAndFeel(
                    UIManager.getSystemLookAndFeelClassName());
        } catch (UnsupportedLookAndFeelException | ClassNotFoundException | InstantiationException | IllegalAccessException e) {
            log.error("An error occurred while setting the LookAndFeel!", e);
        }
        //TODO check state of folder and settings
        isDockerRunning = startBenchmarkContainer();
        if (!isDockerRunning) closeApp();
        //guiMode = true;
        //MainWindow.showMainWindow();
        //if (1 == 1) return;
        //System.exit(0);
        if (args.length > 0) {
            log.info("Arguments: {}", Arrays.toString(args));
            colonyCommandLine parser = null;
            try {
                parser = CommandLine.populateCommand(new colonyCommandLine(), args);
            } catch (Exception e) {
                log.error("Parsing the arguments caused an exception!", e);
                closeApp();
            }
            if (parser == null) {
                log.error("Parsing the arguments failed!");
                closeApp();
            }
            else {
                int result = parser.parseInput();
                switch (result) {
                    case 0:
                        closeApp();
                        break;
                    case 1:
                        guiMode = true;
                        MainWindow.showMainWindow();
                        break;
                    default:
                        break;
                }
            }
        } else {
            guiMode = true;
            MainWindow.showMainWindow();
        }
    }

    private static class Handler implements Thread.UncaughtExceptionHandler
    {
        public void uncaughtException(Thread t, Throwable e)
        {
            log.error("An uncaught exception has occurred in the Thread: " + t, e);
            log.warn("Such exceptions are usually caused by coding mistakes!");
        }
    }
}
