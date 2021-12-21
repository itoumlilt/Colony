/**
 * Class for benchmarking the colony with YCSB
 */

package adbm.ycsb;

import adbm.colony.IcolonyClientWrapper;
import adbm.colony.operations.UpdateOperation;
import adbm.colony.util.colonyUtil;
import adbm.colony.wrappers.colonyClientWrapper;
import adbm.util.AdbmConstants;
import adbm.util.EverythingIsNullableByDefault;
import com.yahoo.ycsb.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.annotation.Nonnull;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static adbm.util.helpers.FormatUtil.format;

@EverythingIsNullableByDefault
public class colonyYCSBClient extends DB
{

    @Nonnull
    private static final Logger log = LogManager.getLogger(colonyYCSBClient.class);

    private IcolonyClientWrapper colonyClient;

    @Nonnull
    private static AtomicInteger idCounter = new AtomicInteger(1);

    /**
     * Any argument-based initialization should start with init() in YCSB
     *
     * @throws DBException
     */
    @Override
    public void init() throws DBException
    {
        colonyClient = new colonyClientWrapper(format("colonyClient-{}", idCounter.getAndIncrement()),
                                                   AdbmConstants.ADBM_CONTAINER_NAME);
        if (!colonyClient.start()) {
            log.error("The colony Client could not be started!");
            throw new DBException("The colony Client could not be started!");
        }
    }

    @Override
    public void cleanup() throws DBException
    {
        colonyClient.stop();
        colonyClient = null;
    }

    /**
     * Read of YCSB_Client
     *
     * @param table  The name of the table
     * @param key    The record key of the record to read.
     * @param fields The list of fields to read, or null for all of them
     * @param result A HashMap of field/value pairs for the result
     * @return
     */
    @Override
    public Status read(String table, String key, Set<String> fields, Map<String, ByteIterator> result)
    {
        if (fields == null) {
            log.warn("The colony Database does not support reading all fields!");
            return Status.NOT_IMPLEMENTED;
        }
        if (result == null) {
            log.warn("Read operation was not performed because the map of results was null!");
            return Status.BAD_REQUEST;
        }
        if (result.size() != 0) {
            log.warn("The map of results was not empty before the read!");
        }
        int size = fields.size();
        switch (size) {
            case 0:
                log.trace("No field is read!");
                return Status.OK;
            case 1:
                log.trace("Single field is read!");
                String field = fields.iterator().next();
                result.put(field, new ByteArrayByteIterator(colonyClient.readKeyValue(field).toString().getBytes()));
                return Status.OK;
            default:
                log.trace("Multiple fields ({}) are read!", size);
                // Keep order
                List<String> orderedFields = new ArrayList<>(fields);
                List<Object> orderedResults = colonyClient.readKeyValues(orderedFields);
                for (int i = 0; i < orderedResults.size(); i++) {
                    result.put(orderedFields.get(i),
                               new ByteArrayByteIterator(orderedResults.get(i).toString().getBytes()));
                }//TODO think about the result
                return Status.OK;
        }

    }

    /**
     * @param table       The name of the table
     * @param startkey    The record key of the first record to read.
     * @param recordcount The number of records to read
     * @param fields      The list of fields to read, or null for all of them
     * @param result      A Vector of HashMaps, where each HashMap is a set field/value pairs for one record
     * @return
     */
    @Override
    public Status scan(String table, String startkey, int recordcount, Set<String> fields,
                       Vector<HashMap<String, ByteIterator>> result)
    {
        return Status.NOT_IMPLEMENTED;
    }

    /**
     * Update of YCSB
     *
     * @param table  The name of the table
     * @param key    The record key of the record to write.
     * @param values A HashMap of field/value pairs to update in the record
     * @return
     */
    @Override
    public Status update(String table, String key, Map<String, ByteIterator> values)
    {

        //Using the hashmap creating an Operation(keyName,OperationName,value) and passing it as a list of Operation
        if (values == null) {
            log.warn("Update operation was not performed because the map of values was null!");
            return Status.BAD_REQUEST;
        }
        int size = values.size();
        switch (size) {
            case 0:
                log.trace("No update operation is performed.");
                return Status.OK;
            case 1:
                log.trace("Single update operation is performed.");
                Map.Entry<String, ByteIterator> entry = values.entrySet().iterator().next();
                colonyClient.updateKey(new UpdateOperation<>(entry.getKey(), colonyUtil.getOperation(), entry.getValue()));
                return Status.OK;
            default:
                log.trace("Multiple update operations ({}) are performed", size);
                colonyClient
                        .updateKeys(values.entrySet().stream().map((s) -> new UpdateOperation<>(s.getKey(), colonyUtil
                                .getOperation(), s.getValue())).collect(Collectors.toList()));
                return Status.OK;
        }
    }

    /**
     * Insert in YCSB
     *
     * @param table  The name of the table
     * @param key    The record key of the record to insert.
     * @param values A HashMap of field/value pairs to insert in the record
     * @return
     */
    @Override
    public Status insert(String table, String key, Map<String, ByteIterator> values)
    {
        return update(table, key, values);
    }

    /**
     * Delete in YCSB
     *
     * @param table The name of the table
     * @param key   The record key of the record to delete.
     * @return
     */
    @Override
    public Status delete(String table, String key)
    {
        return Status.NOT_IMPLEMENTED;
    }
}
