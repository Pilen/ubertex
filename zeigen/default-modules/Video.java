
import processing.video.*;

public class Video extends ImageRenderer {

    Movie movie;
    String mode;

    public void setup(String args) {
        String[] parts = args.split(";", 2);
        String movieName;
        if (parts.length == 2) {
            mode = parts[0].trim();
            movieName = parts[1].trim();
        } else {
            movieName = args;
        }
        println("Playing: " + movieName);
        movie = loadMovie(movieName);
        movie.play();
    }

    public void draw() {

        if (movie != null) {
            if (movie.available()) {
                // print("-");
                renderImage(movie, mode);
            // } else {
                // print("v");
            }
        }
    }

    public void receive(String message) {
        String[] parts = message.split(";", 2);
        switch (parts[0].trim().toLowerCase()) {
        case "open":
        case "load":
            movie = loadMovie(parts[1]);
            break;
        case "play":
            movie.play();
            break;

        }
    }

    public void movieEvent(Movie m) {
        m.read();
        // print(".");
    }

}
