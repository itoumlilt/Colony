package adbm.util;

import com.spotify.docker.client.ProgressHandler;
import com.spotify.docker.client.messages.ProgressMessage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.annotation.Nonnull;
import java.text.MessageFormat;

@EverythingIsNullableByDefault
public class SimpleProgressHandler implements ProgressHandler
{
    @Nonnull
    private static final Logger log = LogManager.getLogger(SimpleProgressHandler.class);

    @Nonnull
    private String type;

    public SimpleProgressHandler(String type)
    {
        if (type != null) this.type = type;
        else this.type = "DockerProgressHandler";
    }

    @Override
    public void progress(ProgressMessage message)
    {
        if (message == null) return;
        String error = message.error();
        if (error != null) {
            handleError(error);
        }
        else if (message.progressDetail() != null) {
            handleProgress(message.id(), message.status(), message.progress());
        }
    }

    private void handleProgress(String id, String status, String progress)
    {
        log.info(MessageFormat.format(type + " {0}: {1} {2}", id, status, progress));
    }

    private void handleError(@Nonnull String error)
    {
        log.error(error);
    }
}
