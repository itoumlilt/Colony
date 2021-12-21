package adbm.util.helpers;

import adbm.util.AdbmConstants;
import adbm.util.EverythingIsNonnullByDefault;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import static adbm.util.helpers.FormatUtil.format;

@EverythingIsNonnullByDefault
public class FileUtil
{
    private static final Logger log = LogManager.getLogger(FileUtil.class);

    public static List<String> getAllFileNamesInFolder(String path)
    {
        List<String> fileNames = new ArrayList<>();
        File folder = new File(path);
        File[] listOfFiles = folder.listFiles();
        if (listOfFiles == null) return fileNames;
        for (File listOfFile : listOfFiles) {
            if (listOfFile.isFile()) {
                fileNames.add(listOfFile.getName());
            }
        }
        return fileNames;
    }

    public static boolean deleteDirectory(File file) {
        File[] contents = file.listFiles();
        if (contents != null) {
            for (File f : contents) {
                if (!Files.isSymbolicLink(f.toPath())) {
                    deleteDirectory(f);
                }
            }
        }
        return file.delete();
    }

    /**
     * Works with folders that don't exist yet.
     *
     * @param folder
     * @return
     */
    public static String getAbsolutePath(String folder)
    {
        return format("{}{}{}", Paths.get(".").toAbsolutePath().normalize().toString(),
                      File.separator, folder);
    }

    public static void changeLogFilePath(String path)
    {
        modifyLogSettings(path, true, false);
    }

    public static void changeConsoleLogLevel(String level)
    {
        modifyLogSettings(level, false, true);
    }

    private static void modifyLogSettings(String replacementValue, boolean location, boolean level)
    {
        if (location == level) return;
        ArrayList<String> lines = new ArrayList<>();
        String line;
        File file;
        FileReader fileReader = null;
        BufferedReader bufferedReader = null;
        FileWriter fileWriter;
        BufferedWriter bufferedWriter = null;
        try {
            file = new File(AdbmConstants.LOG_SETTINGS_PATH);
            fileReader = new FileReader(file);
            bufferedReader = new BufferedReader(fileReader);
            while ((line = bufferedReader.readLine()) != null) {
                //TODO dangerous because of regex
                if (location) {
                    if (line.contains("Log.log")) {
                        if (line.contains("ErrorLog.log")) {
                            line = line.replaceAll("fileName=.*/ErrorLog.log",
                                                   format("fileName=\"{}/ErrorLog.log", replacementValue));
                        }
                        if (line.contains("InfoLog.log")) {
                            line = line.replaceAll("fileName=.*/InfoLog.log",
                                                   format("fileName=\"{}/InfoLog.log", replacementValue));
                        }
                        if (line.contains("TraceLog.log")) {
                            line = line.replaceAll("fileName=.*/TraceLog.log",
                                                   format("fileName=\"{}/TraceLog.log", replacementValue));
                        }
                    }
                }
                if (level) {
                    if (line.contains("<AppenderRef ref=\"ConsoleAppender\" level=")) {
                        line = line.replaceAll("<AppenderRef ref=\"ConsoleAppender\" level=.*/>",
                                               format("<AppenderRef ref=\"ConsoleAppender\" level=\"{}\"/>",
                                                      replacementValue));
                    }
                    if (line.contains("<AppenderRef ref=\"VisualAppender\" level=")) {
                        line = line.replaceAll("<AppenderRef ref=\"VisualAppender\" level=.*/>",
                                               format("<AppenderRef ref=\"VisualAppender\" level=\"{}\"/>",
                                                      replacementValue));
                    }
                }
                lines.add(line);
            }
            fileWriter = new FileWriter(file);
            bufferedWriter = new BufferedWriter(fileWriter);
            for (String s : lines) {
                bufferedWriter.write(s);
            }
            bufferedWriter.flush();

        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                if (fileReader != null) fileReader.close();
                if (bufferedReader != null) bufferedReader.close();
                if (bufferedWriter != null) bufferedWriter.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
