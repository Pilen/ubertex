
public class Time {
    private static long offset = 0;

    public static long now() {
        return System.currentTimeMillis() + offset;
    }

    public static void setClock(long serverTime) {
        offset = serverTime - System.currentTimeMillis();
    }
}
