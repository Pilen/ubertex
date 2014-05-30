
public class SlowTyper extends Module {

    private int size = 30;
    private String string = "";
    private String actual = "";
    private int current = 0;
    private long next = 0;

    // private double min = 171.4;
    // private double max = 240;
    private double min = 30;
    private double max = 75;

    public void setup(String message) {
        receive(message);
    }

    public void draw() {
        if (millis() >= next) {
            if (current < string.length()){
                // Print another char
                current++;
                actual = string.substring(0,current);
                next = millis() + (long) (min + noise(current) * (max - min));
            }
        }

        background();
        textAlign(CENTER);
        textSize(size);
        text(actual, width/2, height/2);
    }

    public void receive(String message) {
        String[] parts = message.split(";", 2);

        switch (parts[0].trim().toLowerCase()) {
        case "text":
        case "message":
        case "string":
            string = parts[1];
            current = 0;
            next = 0;
            break;
        default:
            string = message;
            current = 0;
            next = 0;
            break;
        case "size":
            try {
                int number = Tools.parseInt(parts[1]);
                size = number;
            } catch (NumberFormatException e) {
                println("Size not a number");
            }
            break;
        }
    }
}
