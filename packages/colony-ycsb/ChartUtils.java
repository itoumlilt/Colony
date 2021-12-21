package adbm.resultsVisualization;

import adbm.util.AdbmConstants;
import adbm.util.EverythingIsNonnullByDefault;
import au.com.bytecode.opencsv.CSVReader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.ApplicationFrame;
import org.jfree.ui.RefineryUtilities;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import static adbm.util.helpers.FormatUtil.format;

@EverythingIsNonnullByDefault
public class ChartUtils extends ApplicationFrame {

    private static final Logger log = LogManager.getLogger(ChartUtils.class);

    public ChartUtils(String applicationTitle , String chartTitle ) {
        super(applicationTitle);
        JFreeChart barChart = ChartFactory.createBarChart(
                chartTitle,
                "Category",
                "Latency (µs)",
                createDataset(),
                PlotOrientation.VERTICAL,
                true, true, false);

        ChartPanel chartPanel = new ChartPanel(barChart);
        chartPanel.setPreferredSize(new java.awt.Dimension(560 , 367));
        setContentPane(chartPanel);
    }

    public static CategoryDataset createDataset(String... fileNames) {
        final String read = "[READ]";
        final String cleanup = "[CLEANUP]";
        final String update = "[UPDATE]";
        final String throughput = "Throughput";
        final String averageLatency = "AverageLatency";
        final String minLatency = "MinLatency";
        final String maxLatency = "MaxLatency";
        final DefaultCategoryDataset dataset = new DefaultCategoryDataset();


        log.trace("Updating {} Dataset with Files {}!"/*, name*/, fileNames);
        if (fileNames.length == 0) {
            fileNames = new String[]{AdbmConstants.YCSB_SAMPLE_RESULT_PATH};
        }
        try {
            for (String fileName : fileNames) {
                if(fileName != null && new File(fileName).isFile()) {
                    //CSVReader reader = new CSVReader(new FileReader(format("{}/Result_master_20180302T230249+0100.csv", AdbmConstants.YCSB_RESULTS_PATH)), ',');
                    CSVReader reader = new CSVReader(new FileReader(fileName), ',');
                    String[] readNextLine;

                    while ((readNextLine = reader.readNext()) != null) {
                        if (readNextLine.length < 3) {
                            log.debug("CSV Line could not be read! {}", readNextLine);
                            continue;
                        }

                        String type = readNextLine[0].trim();
                        String name = readNextLine[1].trim();
                        String value = readNextLine[2].trim();
                        log.trace("Read CSV Line: {}, {}, {}", type, name, value);
                        double doubleValue = 0;
                        try {
                            doubleValue = Double.parseDouble(value);
                        } catch (NumberFormatException e) {
                            log.debug("Conversion to double failed! Value: " + value, e);
                        }
                        if (type.equals("[OVERALL]") && name.equals("Throughput(ops/sec)")) {
                            //ToDo
                            dataset.addValue(doubleValue, read, throughput);
                            dataset.addValue(doubleValue, cleanup, throughput);
                            dataset.addValue(doubleValue, update, throughput);
                        }
                        if (!type.isEmpty() && name.equals("AverageLatency(us)")) {
                            //ToDo
                            switch (type) {
                                case "[READ]":
                                    dataset.addValue(doubleValue, read, averageLatency);
                                    break;
                                case "[CLEANUP]":
                                    dataset.addValue(doubleValue, cleanup, averageLatency);
                                    break;
                                case "[UPDATE]":
                                    dataset.addValue(doubleValue, update, averageLatency);
                                    break;
                            }
                        }
                        if (!type.isEmpty() && name.equals("MinLatency(us)")) {
                            //ToDo
                            switch (type) {
                                case "[READ]":
                                    dataset.addValue(doubleValue, read, minLatency);
                                    break;
                                case "[CLEANUP]":
                                    dataset.addValue(doubleValue, cleanup, minLatency);
                                    break;
                                case "[UPDATE]":
                                    dataset.addValue(doubleValue, update, minLatency);
                                    break;
                            }
                        }
                        if (!type.isEmpty() && name.equals("MaxLatency(us)")) {
                            //ToDo
                            switch (type) {
                                case "[READ]":
                                    dataset.addValue(doubleValue, read, maxLatency);
                                    break;
                                case "[CLEANUP]":
                                    dataset.addValue(doubleValue, cleanup, maxLatency);
                                    break;
                                case "[UPDATE]":
                                    dataset.addValue(doubleValue, update, maxLatency);
                                    break;
                            }
                        }
                    }
                }
            }
        } catch(IOException e) {
            System.out.println("File could not be read!");
        }

        return dataset;
    }

    public static XYSeriesCollection createDataset(String name, String operation, String... fileNames)
    {
        log.trace("Updating {} Dataset with Files {}!", name, fileNames);
        int i = 1;
        if (fileNames.length == 0) {
            fileNames = new String[]{AdbmConstants.YCSB_SAMPLE_RESULT_PATH};
        }
        XYSeriesCollection dataset = new XYSeriesCollection();
        try {
            for (String fileName : fileNames) {
                if (fileName != null && new File(fileName).isFile()) {
                    CSVReader reader = new CSVReader(new FileReader(fileName), ',');
                    // Read the header and chuck it away
                    String[] readNextLine;

                    // Set up series
                    final XYSeries seriesRead = new XYSeries(format("{} for Commit {}", operation, i));
                    i++;
                    while ((readNextLine = reader.readNext()) != null) {
                        // variables declaration
                        boolean isValid = true;
                        String operationsType = readNextLine[0];
                        double X = 0;
                        double Y = 0;
                        // add values to dataset for an operation READ or UPDATE
                        if (operationsType.equals(operation)) {
                            try {
                                X = Double.parseDouble(readNextLine[1]);
                                Y = Double.parseDouble(readNextLine[2]);
                            } catch (NumberFormatException e) {
                                isValid = false;
                            }
                            if (isValid) {
                                seriesRead.add(X, Y);
                            }
                        }
                    }
                    dataset.addSeries(seriesRead);
                }
                else {
                    log.error("File was null or not a File! File Path: {}", fileName);
                }
            }
        } catch (IOException e) {
            System.out.println("File could not be read!");
        }
        return dataset;
    }

    public static void main(String[] args) {
        ChartUtils chart = new ChartUtils("Results Usage Statistics",
                                          "Which operation is better?");
        chart.pack( );
        RefineryUtilities.centerFrameOnScreen(chart);
        chart.setVisible(true);
    }
}
