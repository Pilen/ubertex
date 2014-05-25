
public class SlowTyper extends ZSketch {

    private int size = 40;
    private String string = "";
    private String actual = "";
    private int current = 0;
    private long next = 0;

    // private double min = 171.4;
    // private double max = 240;
    private double min = 100;
    private double max = 200;

    public void zSetup(String message) {
        zReceive(message);
    }

    public void zDraw() {
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

    public void zReceive(String message) {
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
