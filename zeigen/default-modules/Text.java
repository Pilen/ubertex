
public class Text extends Module {

    private int size = 30;
    private String string = "";

    public void setup(String message) {
        receive(message);
    }

    public void draw() {
        background();
        textAlign(CENTER);
        textSize(size);
        text(string, width/2, height/2);
    }

    public void receive(String message) {
        String[] parts = message.split(";", 2);

        switch (parts[0].trim().toLowerCase()) {
        case "text":
        case "message":
        case "string":
            string = parts[1];
            break;
        case "size":
            try {
                int number = Tools.parseInt(parts[1]);
                size = number;
            } catch (NumberFormatException e) {
                println("Size not a number");
            }
            break;
        default:
            string = message;
            break;
        }
    }
}
