
import java.util.ArrayList;

import processing.core.PImage;

public class WindowsBruger extends ImageRenderer {

    ZMovie movie;
    String mode;
    int current = 0;
    ArrayList<PImage> slides;
    PImage desktop;
    PImage blue;
    boolean started;

    public void setup() {
        mode = "sized/312,250,400,220";
        movie = loadMovie("windowsbruger/windowsbruger_v26.mp4");

        slides = new ArrayList<>();
        slides.add(loadImage("windowsbruger/slideshow1.png"));
        slides.add(loadImage("windowsbruger/slideshow2.png"));
        slides.add(loadImage("windowsbruger/slideshow3.png"));
        slides.add(loadImage("windowsbruger/slideshow4.png"));
        slides.add(loadImage("windowsbruger/slideshow5.png"));
        slides.add(loadImage("windowsbruger/slideshow6.png"));

        desktop = loadImage("windowsbruger/desktop1.gif");

        blue = loadImage("windowsbruger/windows-blue-screen-of-death-pfn_list_corrupt.jpg");
        println("Started and loaded");
    }

    public void draw() {
        background();
        if (movie.available()) {
            movie.read();
        }
        if (movie.isFinished()) {
            println("finished");
            renderImage(blue, "full");
        } else if (movie.isPlaying()) {
            renderImage(movie, mode);
        }

        switch (current) {
        case 0: break;
        case 1: renderImage(slides.get(0), mode); break;
        case 2: renderImage(slides.get(1), mode); break;
        case 3: renderImage(slides.get(2), mode); break;
        case 4: renderImage(slides.get(3), mode); break;
        case 5: renderImage(slides.get(4), mode); break;
        case 6: renderImage(slides.get(5), mode); break;
        case 7: renderImage(desktop, mode); break;
        case 8: if (!movie.isPlaying()) {movie.play();} break;
        case 9: renderImage(blue, "full"); break;
        }
    }

    public void receive(String arg) {
        current = Tools.parseInt(arg);
    }

    public void movieEvent(ZMovie movie) {
        movie.read();
    }

    public void onDeath() {
        movie.stop();
    }
}
