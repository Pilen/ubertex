
import processing.core.PApplet;

public abstract class ZSketch extends PApplet {
    private String args;

    public ZSketch() {
        super();
    }

    public final void zStart(String args) {
        this.args = args;
        this.runSketch();
    }

    public final void setup() {
        this.zSetup(this.args);
    }

    public final void draw() {
        this.zDraw();
    }

    protected final void exitActual() {
        System.out.println("DØ");
        System.out.println(Thread.activeCount());
        System.out.println(Thread.currentThread().toString());
        this.destroy();
        this.frame.setVisible(false);
        // Thread.currentThread().interrupt();
    }

    /*
     * USER METHODS:
     */

    public void zDraw() {

    }

    public void zRecieve(String args) {

    }

    public void zSetup() {

    }

    public void zSetup(String args) {
        this.zSetup();
    }
}
