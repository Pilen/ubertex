
public class Controller {

    private ZSketch sketch;

    public Controller() {
        this.sketch = null;

    }

    public void message(String message) {

        String[] headers = message.split(";", 4);
        if (headers.length == 4) {
            String target = headers[0];
            String time = headers[1];
            String command = headers[2];
            String options = headers[3];
            System.out.println("RECEIVED: " + message);
        } else {
            System.out.println("MALFORMED MESSAGE RECEIVED: " + message);
        }
    }
}
