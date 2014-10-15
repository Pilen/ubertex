
import processing.core.PApplet;
import processing.video.Movie;

public class ZMovie extends Movie {

    private boolean started;
    public ZMovie(PApplet parrent, String filename) {
        super(parrent, filename);
        this.started = false;
    }

    public boolean isFinished() {
        return this.started && !this.playing;
    }
    public boolean isPlaying() {
        return this.playing;
    }

    public void play() {
        this.started = true;
        super.play();
    }
}
