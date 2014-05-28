
public final class ZSketch {

    private Controller controller;

    public ZSketch(Controller controller) {
        super();

        this.controller = controller;
    }

    public void zStart() {
        this.runSketch(new String[] {"--hide-stop"});
    }

    public void setup() {
        controller.reset();
        size(controller.width, controller.height);
        background();
        frame.setBackground(new java.awt.Color(0, 0, 0));
        //The above does not currently work so we use a workaround, see https://github.com/processing/processing/issues/2071
        ((javax.swing.JFrame) frame).getContentPane().setBackground(new java.awt.Color(0,0,0));

    }

    public void draw() {
        try {
            controller.step();
            controller.module.draw();
            if (controller.blank) {
                background()
            }
        } catch (Exception e) {
            System.out.println("======== ERROR ========\n" + "CURRENT MODULE THREW AN EXCEPTION\n" + e.toString());
            e.printStackTrace();
            System.out.println("=======================");
            exit();
        }
    }

    public File dataFile(String filename) {
        File absolute = new File(filename);
        if (absolute.isAbsolute()) {
            return absolute;
        }
        return new File(controller.baseDir, filename);
    }

    public void background() {
        background(controller.backgroundColor);
    }
}
