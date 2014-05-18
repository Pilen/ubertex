
import processing.core.PApplet;

public abstract class ZSketch extends PApplet implements TaskPerformer {
    private TaskManager taskManager;

    private String args;
    private int zWidth;
    private int zHeight;
    private int zOffsetX;
    private int zOffsetY;
    private int zRandomSeed;
    private int zNoiseSeed;

    public ZSketch() {
        super();
    }

    public final void zStart(String args, int zWidth, int zHeight, int zOffsetX, int zOffsetY, int zRandomSeed, int zNoiseSeed) {
        this.args = args;
        this.zWidth = zWidth;
        this.zHeight = zHeight;
        this.zRandomSeed = zRandomSeed;
        this.zNoiseSeed = zNoiseSeed;
        this.taskManager = new TaskManager(this);
        this.runSketch();
    }

    public final void setup() {
        try {
            randomSeed(this.zRandomSeed);
            noiseSeed(this.zNoiseSeed);
            size(this.zWidth, this.zHeight);
            this.zSetup(this.args);
        } catch (Exception e) {
            System.out.println("======== ERROR ========\n" + "CURRENT SKETCH THREW AN EXCEPTION\n" + e.toString());
            e.printStackTrace();
            System.out.println("=======================");
            this.exit();
        }
    }

    public final void draw() {
        try {
            this.taskManager.process();
            this.zDraw();
        } catch (Exception e) {
            System.out.println("======== ERROR ========\n" + "CURRENT SKETCH THREW AN EXCEPTION\n" + e.toString());
            e.printStackTrace();
            System.out.println("=======================");
            this.exit();
        }
    }

    protected final void exitActual() {
        this.destroy();
        this.frame.setVisible(false);
        // Thread.currentThread().interrupt();
    }


    public final void addTask(long time, String message) {
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
