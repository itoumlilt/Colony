package adbm.util.helpers;

import adbm.util.EverythingIsNonnullByDefault;
import org.apache.logging.log4j.message.FormattedMessage;

@EverythingIsNonnullByDefault
public class FormatUtil
{
    public static String format(String text)
    {
        return text;
    }

    public static String format(String text, Object... params)
    {
        return new FormattedMessage(text, params).getFormattedMessage();
    }
}
