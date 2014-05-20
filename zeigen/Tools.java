
public class Tools {

    public static float parseFloat(String number) {
        String trimmed = number.trim();
        if (trimmed.isEmpty()) {
            return 0;
        }
        return Float.parseFloat(trimmed);
    }

    public static int parseInt(String number) {
        String trimmed = number.trim();
        if (trimmed.isEmpty()) {
            return 0;
        }
        return Integer.parseInt(trimmed);
    }

    public static long parseTime(String number) {
        String trimmed = number.trim();
        if (trimmed.isEmpty() || trimmed.equals("now")) {
            return 0;
        }
        return Long.parseLong(trimmed);
    }
}
