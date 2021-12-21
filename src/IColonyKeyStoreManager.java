package adbm.settings;

import adbm.util.EverythingIsNonnullByDefault;
import adbm.util.IStartStop;
import eu.colonydb.colonypb.colonyPB;

import java.util.List;
import java.util.Map;

/**
 *
 */
@EverythingIsNonnullByDefault
public interface IcolonyKeyStoreManager extends IStartStop
{
    /**
     *
     * @return
     */
    Map<String, colonyPB.CRDT_type> getMapKeyNameKeyType();

    /**
     *
     * @param keyName
     * @return
     */
    colonyPB.CRDT_type getTypeOfKey(String keyName);

    /**
     *
     * @param keyName
     * @param keyType
     * @return
     */
    boolean addKey(String keyName, colonyPB.CRDT_type keyType);

    /**
     *
     * @param keyName
     * @return
     */
    boolean removeKey(String keyName);

    /**
     *
     * @return
     */
    Map<colonyPB.CRDT_type, List<String>> getKeyTypeKeyNamesMap();

    /**
     *
     * @return
     */
    boolean resetKeyTypeSettings();
}
