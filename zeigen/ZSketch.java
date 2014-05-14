
import processing.core.PApplet;

public abstract class ZSketch extends PApplet implements TaskPerformer {
    private TaskManager taskManager;

    private String args;
    private int zRandomSeed;
    private int zNoiseSeed;

    public ZSketch() {
        super();
    }

    public final void zStart(String args, int zRandomSeed, int zNoiseSeed) {
        this.args = args;
        this.zRandomSeed = zRandomSeed;
        this.zNoiseSeed = zNoiseSeed;
        this.taskManager = new TaskManager(this);
        this.runSketch();
    }

    public final void setup() {
        randomSeed(this.zRandomSeed);
        noiseSeed(this.zNoiseSeed);
        this.zSetup(this.args);
    }

    public final void draw() {
        this.taskManager.process();
        this.zDraw();
    }

    protected final void exitActual() {
        this.destroy();
        this.frame.setVisible(false);
        // Thread.currentThread().interrupt();
    }


    public final void addTask(String time, String message) {
        this.taskManager.addTask(time, message);
    }

    public final void doTask(String message, String ignored) {
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
