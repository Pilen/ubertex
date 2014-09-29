
public class Test extends Module {
    private int a = 312;
    private int b = 250;
    private int c = 400;
    private int d = 220;

    public void draw() {
        background(color(255,0,255));
        // rectMode(CORNERS);
        rect(a, b, c, d);

        line(0,0,width,height);
        line(0,height,width,0);
    }

    public void receive(String args) {
        String[] parts = args.split(";");
        a = Tools.parseInt(parts[0]);
        b = Tools.parseInt(parts[1]);
        c = Tools.parseInt(parts[2]);
        d = Tools.parseInt(parts[3]);

        println("" + width + "x" + height);
    }
}
