
import processing.core.PApplet;

public abstract class ZSketch extends PApplet implements TaskPerformer {
    private TaskManager taskManager;

    private String args;

    public ZSketch() {
        super();
    }

    public final void zStart(String args) {
        this.args = args;
        this.taskManager = new TaskManager(this);
        this.runSketch();
    }

    public final void setup() {
        this.zSetup(this.args);
    }

    public final void draw() {
        this.zDraw();
    }

    protected final void exitActual() {
        this.destroy();
        this.frame.setVisible(false);
        // Thread.currentThread().interrupt();
    }


    public final void addTask(String time, String message) {
        this.taskManager.addTask(time, "", message);
    }

    public final void doTask(String ignored, String message) {
        this.zRecieve(message);
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
