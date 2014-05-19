
import java.io.File;
import processing.core.PApplet;

public abstract class ZSketch extends PApplet implements TaskPerformer {
    private TaskManager taskManager;

    private String args;
    private File baseDir;
    private int zWidth;
    private int zHeight;
    private int zOffsetX;
    private int zOffsetY;
    private int zRandomSeed;
    private int zNoiseSeed;

    public ZSketch() {
        super();
    }

    public final void zStart(String args, File baseDir, int zWidth, int zHeight, int zOffsetX, int zOffsetY, int zRandomSeed, int zNoiseSeed) {
        this.args = args;
        this.baseDir = baseDir;
        this.zWidth = zWidth;
        this.zHeight = zHeight;
        this.zRandomSeed = zRandomSeed;
        this.zNoiseSeed = zNoiseSeed;
        this.taskManager = new TaskManager(this);
        this.runSketch(new String[] {"--hide-stop"});
    }

    public final void setup() {
        try {
            randomSeed(this.zRandomSeed);
            noiseSeed(this.zNoiseSeed);
            if (this.zWidth < 0) {
                this.zWidth = displayWidth;
            } if (this.zHeight < 0) {
                this.zHeight = displayHeight;
            }
            size(this.zWidth, this.zHeight);
            background(0,0,0);
            frame.setBackground(new java.awt.Color(0, 0, 0));
            //The above does not currently work so we use a workaround, see https://github.com/processing/processing/issues/2071
            ((javax.swing.JFrame) frame).getContentPane().setBackground(new java.awt.Color(0,0,0));
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

    public File dataFile(String where) {
        File why = new File(where);
        if (why.isAbsolute()) {
            return why;
        }
        return new File(this.baseDir, where);
    }

    public final void addTask(long time, String message) {
        this.taskManager.addTask(time, message);
    }

    public final void doTask(String message, String ignored) {
        this.zRecieve(message);
    }

    public final int clearTasks() {
        return this.taskManager.clear();
    }

    public boolean sketchFullScreen() {
        return false;
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
