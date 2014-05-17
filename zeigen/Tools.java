
public class Tools {
    public static int parseInt(String number) {
        String trimmed = number.trim();
        if (trimmed.isEmpty()) {
            return 0;
        }
        return Integer.parseInt(trimmed);
    }
}
