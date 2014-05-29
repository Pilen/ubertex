
import java.io.File;

import processing.core.PImage;
import processing.core.PConstants;
import processing.video.Movie;

public abstract class Module {

    private ZSketch sketch;

    public final void initModule(ZSketch sketch) {
        this.sketch = sketch;
    }


    public void receive(String message) {}

    public void setup(String arguments) {
        this.setup();
    }

    public void setup() {}

    public void draw() {}

    public void onDeath() {}

    /*
     * Processing fields:
     */
    public int width;
    public int height;

    public int CENTER = PConstants.CENTER;
    public int CORNERS = PConstants.CORNERS;

    /*
     * Processing functions:
     */


    public final PImage loadImage(String filename) {return sketch.loadImage(filename);}
    public final void background() {sketch.background();}
    public final void background(int color) {sketch.background(color);}
    public final int color(int r, int g, int b) {return sketch.color(r, g, b);}
    public static final void println(String message) {ZSketch.println(message);}
    public static final void println(long message) {ZSketch.println(message);}
    public final void image(PImage image, float a, float b, float c, float d) {sketch.image(image, a, b, c, d);}
    public final File dataFile(String filename) {return sketch.dataFile(filename);}

    public static final int max(int a, int b) {return ZSketch.max(a, b);}

    public final float random(float high) {return sketch.random(high);}
    public final float random(float low, float high) {return sketch.random(low, high);}

    public final void textAlign(int alignX) {sketch.textAlign(alignX);}
    public final void textSize(float size) {sketch.textSize(size);}
    public final void text(String text, float x, float y) {sketch.text(text, x, y);}
    public final long millis() {return sketch.millis();}
    public final float noise(float a) {return sketch.noise(a);}

    public final Movie loadMovie(String filename) {return new Movie(sketch, filename);}

    public final void line(float a, float b, float c, float d) {sketch.line(a, b, c, d);}

    public final void rectMode(int a) {sketch.rectMode(a);}
    public final void rect(float a, float b, float c, float d) {sketch.rect(a, b, c, d);}

}
