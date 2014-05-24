
public class Text extends ZSketch {

    private int size = 40;
    private String string = "";

    public void zSetup(String message) {
        zReceive(message);
    }

    public void zDraw() {
        background();
        textAlign(CENTER);
        textSize(size);
        text(string, width/2, height/2);
    }

    public void zReceive(String message) {
        String[] parts = message.split(";", 2);

        switch (parts[0].trim().toLowerCase()) {
        case "text":
        case "message":
        case "string":
            string = parts[1];
            println(string);
            if (string.equals("Gå væk")) {
                println("equals");
            } else {
                println("dåes not æqual");
            }
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
