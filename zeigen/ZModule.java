
import java.io.File;
import processing.core.PApplet;

public abstract class ZModule extends PApplet implements TaskPerformer {
    private TaskManager taskManager;

    private String args;
    private File baseDir;
    private int zWidth;
    private int zHeight;
    private int zOffsetX;
    private int zOffsetY;
    private int zRandomSeed;
    private int zNoiseSeed;

    public int zBackgroundColor;
    public boolean zBlank = false;

    public ZModule() {
        super();
    }

    public final void zStart(String args, File baseDir,
                             int zWidth, int zHeight,
                             int zOffsetX, int zOffsetY,
                             int zRandomSeed, int zNoiseSeed,
                             String backgroundColor) {
        this.args = args;
        this.baseDir = baseDir;
        this.zWidth = zWidth;
        this.zHeight = zHeight;
        this.zRandomSeed = zRandomSeed;
        this.zNoiseSeed = zNoiseSeed;
        this.taskManager = new TaskManager(this);


        try { // Calculate background color:
            String[] colors = backgroundColor.split("[;,\\s]");

            if (backgroundColor.contains(".")) {
                float gray, alpha, r, g, b;
                switch (colors.length) {
                default: System.out.println("WRONG NUMBER OF ARGUMENTS FOR BACKGROUND-COLOR"); break;
                case 0:
                    this.zBackgroundColor = color(0);
                    break;
                case 1:
                    gray = Tools.parseFloat(colors[0]);
                    this.zBackgroundColor = color(gray);
                    break;
                case 2:
                    gray = Tools.parseFloat(colors[0]);
                    alpha = Tools.parseFloat(colors[3]);
                    this.zBackgroundColor = color(gray, alpha);
                    break;
                case 3:
                    r = Tools.parseFloat(colors[0]);
                    g = Tools.parseFloat(colors[1]);
                    b = Tools.parseFloat(colors[2]);
                    this.zBackgroundColor = color(r, g, b);
                    break;
                case 4:
                    r = Tools.parseFloat(colors[0]);
                    g = Tools.parseFloat(colors[1]);
                    b = Tools.parseFloat(colors[2]);
                    alpha = Tools.parseFloat(colors[3]);
                    this.zBackgroundColor = color(r, g, b, alpha);
                    break;
                }
            } else {
                int gray, alpha, r, g, b;
                switch (colors.length) {
                default: System.out.println("WRONG NUMBER OF ARGUMENTS FOR BACKGROUND-COLOR"); break;
                case 0:
                    this.zBackgroundColor = color(0);
                    break;
                case 1:
                    gray = Tools.parseInt(colors[0]);
                    this.zBackgroundColor = color(gray);
                    break;
                case 2:
                    gray = Tools.parseInt(colors[0]);
                    alpha = Tools.parseInt(colors[3]);
                    this.zBackgroundColor = color(gray, alpha);
                    break;
                case 3:
                    r = Tools.parseInt(colors[0]);
                    g = Tools.parseInt(colors[1]);
                    b = Tools.parseInt(colors[2]);
                    this.zBackgroundColor = color(r, g, b);
                    break;
                case 4:
                    r = Tools.parseInt(colors[0]);
                    g = Tools.parseInt(colors[1]);
                    b = Tools.parseInt(colors[2]);
                    alpha = Tools.parseInt(colors[3]);
                    this.zBackgroundColor = color(r, g, b, alpha);
                    break;
                }
            }
        } catch (NumberFormatException e) {
            System.out.println("AN ARGUMENT FOR BACKGROUND-COLOR WAS NOT A LEGAL NUMBER: " + backgroundColor);
        }

        this.runModule(new String[] {"--hide-stop"});
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
            background(zBackgroundColor);
            frame.setBackground(new java.awt.Color(0, 0, 0));
            //The above does not currently work so we use a workaround, see https://github.com/processing/processing/issues/2071
            ((javax.swing.JFrame) frame).getContentPane().setBackground(new java.awt.Color(0,0,0));
            this.zSetup(this.args);
        } catch (Exception e) {
            System.out.println("======== ERROR ========\n" + "CURRENT MODULE THREW AN EXCEPTION\n" + e.toString());
            e.printStackTrace();
            System.out.println("=======================");
            this.exit();
        }
    }

    public final void draw() {
        try {
            this.taskManager.process();
            this.zDraw();
            if (this.zBlank) {
                background(zBackgroundColor);
            }
        } catch (Exception e) {
            System.out.println("======== ERROR ========\n" + "CURRENT MODULE THREW AN EXCEPTION\n" + e.toString());
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
        return new File(this.baseDir, File.separator + "data" + File.separator + where);
    }

    public void background() {
        this.background(zBackgroundColor);
    }

    public final void addTask(long time, String message) {
        this.taskManager.addTask(time, message);
    }

    public final void doTask(String message, String ignored) {
        String[] parts = message.split(";", 2);


        switch (parts[0].trim().toLowerCase()) {
        case "blanked": case "hidden": case "paused":
            message = parts[1];
            break;
        case "unblank": case "unhide": case "unpause": case "continue":
            message = parts[1];
        case "":
        case "default":
        default:
            this.zBlank = false;
            break;
        }
        this.zReceive(message);
    }

    public final int clearTasks() {
        return this.taskManager.clear();
    }

    public boolean moduleFullScreen() {
        return false;
    }

    /*
     * USER METHODS:
     */

    public void zDraw() {

    }

    public void zReceive(String message) {

    }

    public void zSetup() {

    }

    public void zSetup(String args) {
        this.zSetup();
    }
}
