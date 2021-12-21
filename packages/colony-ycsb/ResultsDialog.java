package adbm.resultsVisualization;

import adbm.util.EverythingIsNonnullByDefault;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.ui.UIUtils;
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.xy.XYSeriesCollection;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

import static adbm.util.helpers.FormatUtil.format;

@EverythingIsNonnullByDefault
public class ResultsDialog extends JDialog
{

    private static final Logger log = LogManager.getLogger(ResultsDialog.class);

    public static void showResultsWindow(boolean timeSeries, String... _fileNames)
    {
        ResultsDialog resultsWindow = new ResultsDialog(timeSeries, _fileNames);
        resultsWindow.setVisible(true);
    }

    private ResultsDialog(boolean timeSeries, String... fileNames)
    {
        super(null, "Results Visualization", ModalityType.MODELESS);
        //super("Results Visualization");
        this.setMinimumSize(new Dimension(1000, 600));
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        pack();
        UIUtils.centerFrameOnScreen(this);
        log.trace("Starting ResultsDialog!");
        List<ChartPanel> chartPanelList = new ArrayList<>();
        if (timeSeries) {
            chartPanelList.add(createTimeSeriesChart("READ", "[READ]", fileNames));
            chartPanelList.add(createTimeSeriesChart("UPDATE", "[UPDATE]", fileNames));
        }
        else {
            chartPanelList.add(createBarChart("Which operation is better?","",fileNames));
        }
        setContentPane(ResultsPanel.getPanel(chartPanelList.toArray(new ChartPanel[0])));
    }

    public static void main(String[] args)
    {
        showResultsWindow(true);
    }

    private ChartPanel createTimeSeriesChart(String chartName, String operationType, String... fileNames)
    {
        XYSeriesCollection dataset = ChartUtils.createDataset(chartName, operationType, fileNames);
        JFreeChart chart = ChartFactory.createXYLineChart(format("Benchmark Result for {} Latency", chartName),
                                                          "Time (ms)",
                                                          "Latency (µs)",
                                                          dataset,
                                                          PlotOrientation.VERTICAL,
                                                          true,
                                                          true,
                                                          false);

        return new ChartPanel(chart);
    }

    private ChartPanel createBarChart(String chartName, String operationType, String... fileNames) {
        CategoryDataset dataset = ChartUtils.createDataset(fileNames);
        JFreeChart barChart = ChartFactory.createBarChart(
                chartName,
                "Category",
                "Latency (µs)",
                dataset,
                PlotOrientation.VERTICAL,
                true, true, false);
        return new ChartPanel(barChart);
    }


}