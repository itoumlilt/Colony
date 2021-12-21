package adbm.util;

import org.apache.logging.log4j.LogManager;

import javax.annotation.Nullable;

import static adbm.util.helpers.FormatUtil.format;

/**
 * This interface specifies that the implementing class can be started and stopped.
 * This is a utility to define common methods of classes that require a setup before they can be used.
 */
@EverythingIsNonnullByDefault
public interface IStartStop
{

    /**
     * Starts the instance of the class.
     * Returns true if the start was successful.
     * Otherwise false.
     * Has different meaning depending on the implementation.
     * Usually sets up connection or a file stream.
     *
     * @return true if the start was successful. Otherwise false.
     */
    boolean start();

    /**
     * Starts the instance of the class with an address and port.
     * Returns true if the start was successful.
     * Otherwise false.
     * Has different meaning depending on the implementation.
     * Usually sets up connection or a file stream.
     * The default implementation just returns {@link #start()} so that it is not mandatory to implement it explicitly.
     *
     * @param address The address.
     * @param port The port.
     * @return true if the start was successful. Otherwise false.
     */
    default boolean start(@Nullable String address, int port) {
        return start();
    }

    /**
     * Starts the instance of the class with one or multiple string arguments e.g. addresses and file paths.
     * Returns true if the start was successful.
     * Otherwise false.
     * Has different meaning depending on the implementation.
     * Usually sets up connection or a file stream.
     * The default implementation just returns {@link #start()} so that it is not mandatory to implement it explicitly.
     *
     * @param arg The string argument e.g. a path to a file.
     * @param args The string arguments e.g. a path to a file.
     * @return true if the start was successful. Otherwise false.
     */
    default boolean start(@Nullable String arg, String... args) {
        return start();
    }

    /**
     * Stops the instance of the class.
     * Returns true if the stop was successful.
     * Otherwise false (e.g. errors during the stop).
     * Has different meaning depending on the implementation.
     * Usually closes a connection or file stream.
     *
     * @return true if the stop was successful. Otherwise false.
     */
    boolean stop();

    /**
     * Returns true if the instance of the class was successfully started and was not stopped yet.
     * Otherwise false.
     * Is silent.
     *
     * @return true if the instance of the class was successfully started and was not stopped yet. Otherwise false.
     */
    boolean isReady();

    /**
     * Returns true if the instance of the class was successfully started and was not stopped yet.
     * Otherwise false.
     * Logs information about the state of the instance.
     * The default implementation just returns {@link #isReady()} logs (trace) if the instance was ready or not so that it is not mandatory to implement it explicitly.
     *
     * @return true if the instance of the class was successfully started and was not stopped yet. Otherwise false.
     */
    default boolean isReadyInfo()
    {
        boolean ready = isReady();
        String message;
        if (ready) {
            message = format("The instance ({}) was ready!", getClass().getSimpleName());
        }
        else {
            message = format("The instance ({}) was not ready!", getClass().getSimpleName());
        }
        LogManager.getLogger(getClass().getSimpleName()).trace(message);
        return ready;
    }
}
