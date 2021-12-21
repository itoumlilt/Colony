package adbm.main;

import adbm.util.AdbmConstants;
import adbm.util.EverythingIsNonnullByDefault;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Iterator;
import java.util.stream.Stream;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
@EverythingIsNonnullByDefault
public class FileSetup
{

    private static final Logger log = LogManager.getLogger(Main.class);

    public static boolean setupFoldersAndFiles()
    {
        return updateFolderContentFromResources();
    }

    private static boolean updateFolderContentFromResources()
    {
        if (!initializeFolders()) return false;
        if (!copyFileFromResources(
                AdbmConstants
                        .getJarPath(AdbmConstants.YCSB_RESULT_FOLDER_NAME, AdbmConstants.YCSB_SAMPLE_RESULT_FILE_NAME),
                AdbmConstants.YCSB_SAMPLE_RESULT_PATH))
        {
            log.error("The sample result could not be copied to the results folder!");
            return false;
        }
        if (!copyFolderFromResources(AdbmConstants.getJarPath(AdbmConstants.YCSB_WORKLOADS_FOLDER_NAME),
                                     AdbmConstants.YCSB_WORKLOADS_FOLDER_PATH))
        {
            log.error("The YCSB workloads could not be copied to the workloads folder!");
            return false;
        }
        if (!copyFolderFromResources(AdbmConstants.getJarPath(AdbmConstants.DOCKER_FOLDER_NAME),
                                     AdbmConstants.DOCKER_FOLDER_PATH))
        {
            log.error("The Dockerfile and other required files could not be copied to the Docker folder!");
            return false;
        }
        return true;
    }

    private static boolean initializeFolders()
    {
        File settingsFolder = new File(AdbmConstants.SETTINGS_FOLDER_PATH);
        if (!settingsFolder.exists() && !settingsFolder.mkdirs()) {
            log.error("The folder {} did not exist and could not be created.", AdbmConstants.SETTINGS_FOLDER_PATH);
            return false;
        }
        File ycsbResultFolder = new File(AdbmConstants.YCSB_RESULT_FOLDER_PATH);
        if (!ycsbResultFolder.exists() && !ycsbResultFolder.mkdirs()) {
            log.error("The folder {} did not exist and could not be created.", AdbmConstants.YCSB_RESULT_FOLDER_PATH);
            return false;
        }
        File ycsbWorkloadFolder = new File(AdbmConstants.YCSB_WORKLOADS_FOLDER_PATH);
        if (!ycsbWorkloadFolder.exists() && !ycsbWorkloadFolder.mkdirs()) {
            log.error("The folder {} did not exist and could not be created.",
                      AdbmConstants.YCSB_WORKLOADS_FOLDER_PATH);
            return false;
            //TODO copy sample result
            //TODO delete folder maybe
        }
        File dockerFolder = new File(AdbmConstants.DOCKER_FOLDER_PATH);
        if (!dockerFolder.exists() && !dockerFolder.mkdirs()) {
            log.error("The folder {} did not exist and could not be created.", AdbmConstants.DOCKER_FOLDER_PATH);
            return false;
        }
        File readme = new File(AdbmConstants.ADBM_README_PATH);
        if (!readme.exists()) {
            try {
                boolean success = readme.createNewFile();
                if (!success) {
                    log.error("The Readme file with the Path {} could not be created!", AdbmConstants.ADBM_README_PATH);
                    return false;
                }
                try (PrintWriter out = new PrintWriter(AdbmConstants.ADBM_README_PATH)) {
                    out.println(AdbmConstants.ADBM_README_TEXT);
                }
            } catch (IOException e) {
                log.error("The Readme file with the Path {} could not be created!", AdbmConstants.ADBM_README_PATH);
                return false;
            }
        }
        //TODO make sure all
        return true;
    }

    private static boolean copyFolderFromResources(String folderJarPath, String path)
    {
        try {
            URI uri = Main.class.getResource(folderJarPath).toURI();
            Path myPath;
            log.info("URI: {}", uri);
            FileSystem fileSystem;
            if (uri.getScheme().equals("jar")) {
                log.info("jar file");
                fileSystem = FileSystems.newFileSystem(uri, Collections.emptyMap());
                myPath = fileSystem.getPath(folderJarPath);
            }
            else {
                log.info("no jar file");
                return true; //TODO copy also -> already existing I think
            }

            Stream<Path> walk = Files.walk(myPath, 1);
            for (Iterator<Path> it = walk.iterator(); it.hasNext(); ) {
                Path source = it.next();
                Path target = new File(path).toPath();
                log.info("Filename: {}", source.getFileName().toString());
                if (Files.isDirectory(source)) continue;
                log.info("Copy from {}", source);
                log.info("Copy to {}", target);
                Files.copy(source, target.resolve(myPath.relativize(source).toString()), REPLACE_EXISTING);
            }
            fileSystem.close();
            return true;
        } catch (URISyntaxException | IOException e) {
            log.error("", e);
            return false;
        }
    }

    private static boolean copyFileFromResources(String fileJarPath, String path)
    {
        URL inputUrl = Main.class.getResource(fileJarPath);
        File dest = new File(path);
        try {
            FileUtils.copyURLToFile(inputUrl, dest);
            return true;
        } catch (IOException e) {
            log.error("", e);
            return false;
        }
    }

}
