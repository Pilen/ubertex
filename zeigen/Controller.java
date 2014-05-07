import java.util.ArrayList;
import java.util.HashSet;

public class Controller {

    private final HashSet<String> names;
    private ZSketch sketch;

    public Controller(ArrayList<String> names) {
        this.sketch = null;

        this.names = new HashSet<String>(names.size());
        for (String name : names) {
            this.names.add(name.toLowerCase());
        }
    }

    public void message(String message) {

        String[] headers = message.split(";", 4);
        if (headers.length == 4) {
            String targets = headers[0];
            String time = headers[1];
            String command = headers[2];
            String options = headers[3];

            if (this.forMe(targets.split(" "))) {
                System.out.println("RECEIVED: " + message);
            } else {
                System.out.println("IGNORED MESSAGE");
            }
        } else {
            System.out.println("MALFORMED MESSAGE RECEIVED: " + message);
        }
    }

    private boolean forMe(String[] targets) {
        for (String target : targets) {
            if (this.names.contains(target.toLowerCase())) {
                return true;
            }
        }
        return false;
    }
}
