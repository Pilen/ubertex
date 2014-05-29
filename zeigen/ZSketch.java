
import java.io.File;

import processing.core.PApplet;
import processing.core.PFont;

public final class ZSketch extends PApplet {

    private final Controller controller;

    public ZSketch(Controller controller) {
        super();

        this.controller = controller;
    }

    public void zStart() {
        this.runSketch(new String[] {"--hide-stop"});
    }

    public void setup() {
        controller.reset();
        frame.setBackground(new java.awt.Color(0, 0, 0));
        //The above does not currently work so we use a workaround, see https://github.com/processing/processing/issues/2071
        ((javax.swing.JFrame) frame).getContentPane().setBackground(new java.awt.Color(0,0,0));
    }

    public void draw() {
        controller.step();
    }

    public boolean sketchFullScreen() {
        return false;
    }

    public ZMovie playSound(String filename) {
        ZMovie sound = new ZMovie(this, filename);
        sound.play();
        return sound;

    }

    public void printFontList() {
        String[] fonts = PFont.list();
        for (String font : fonts) {
            println(font);
        }
    }

    /*
     * User functions:
     */

    public void background() {
        background(controller.backgroundColor);
    }

    public int color(String colorString) {
        try {
            String[] colors = colorString.split("[;,\\s]");

            if (colorString.contains(".")) {
                float gray, alpha, r, g, b;
                switch (colors.length) {
                default: System.out.println("WRONG NUMBER OF ARGUMENTS TO COLOR"); break;
                case 0:
                    return color(0);
                case 1:
                    gray = Tools.parseFloat(colors[0]);
                    return color(gray);
                case 2:
                    gray = Tools.parseFloat(colors[0]);
                    alpha = Tools.parseFloat(colors[3]);
                    return color(gray, alpha);
                case 3:
                    r = Tools.parseFloat(colors[0]);
                    g = Tools.parseFloat(colors[1]);
                    b = Tools.parseFloat(colors[2]);
                    return color(r, g, b);
                case 4:
                    r = Tools.parseFloat(colors[0]);
                    g = Tools.parseFloat(colors[1]);
                    b = Tools.parseFloat(colors[2]);
                    alpha = Tools.parseFloat(colors[3]);
                    return color(r, g, b, alpha);
                }
            } else {
                int gray, alpha, r, g, b;
                switch (colors.length) {
                default: System.out.println("WRONG NUMBER OF ARGUMENTS TO COLOR"); break;
                case 0:
                    return color(0);
                case 1:
                    gray = Tools.parseInt(colors[0]);
                    return color(gray);
                case 2:
                    gray = Tools.parseInt(colors[0]);
                    alpha = Tools.parseInt(colors[3]);
                    return color(gray, alpha);
                case 3:
                    r = Tools.parseInt(colors[0]);
                    g = Tools.parseInt(colors[1]);
                    b = Tools.parseInt(colors[2]);
                    return color(r, g, b);
                case 4:
                    r = Tools.parseInt(colors[0]);
                    g = Tools.parseInt(colors[1]);
                    b = Tools.parseInt(colors[2]);
                    alpha = Tools.parseInt(colors[3]);
                    return color(r, g, b, alpha);
                }
            }
        } catch (NumberFormatException e) {
            System.out.println("AN ARGUMENT FOR BACKGROUND-COLOR WAS NOT A LEGAL NUMBER: " + colorString);
        }
        return controller.backgroundColor;
    }

    public File dataFile(String filename) {
        File absolute = new File(filename);
        if (absolute.isAbsolute()) {
            return absolute;
        }
        return new File(controller.baseDir, filename);
    }

    public void movieEvent(ZMovie m) {
        this.controller.module.movieEvent(m);
    }

}
