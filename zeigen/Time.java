
import java.util.concurrent.TimeUnit;

public class Time {
    private static long offset = 0;

    public static final TimeUnit timeUnit = TimeUnit.MILLISECONDS;
    public static long now() {
        return System.currentTimeMillis() + offset;
    }

    public static void setClock(long serverTime) {
        offset = serverTime - System.currentTimeMillis();
    }
}
