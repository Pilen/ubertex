
import processing.video.*;

public class Video extends ImageRenderer {

    Movie movie;
    String mode;

    public void zSetup(String args) {
        String[] parts = args.split(";", 2);
        String movieName;
        if (parts.length == 2) {
            mode = parts[0].trim();
            movieName = parts[1].trim();
        } else {
            movieName = args;
        }
        println("Playing: " + movieName);
        movie = new Movie(this, movieName);
        movie.loop();
    }

    public void zDraw() {

        if (movie != null) {
            // if (movie.available()) {
                // print("-");
                renderImage(movie, mode);
            // } else {
                // print("v");
            // }
        }
    }

    public void zReceive(String message) {
        String[] parts = message.split(";", 2);
        switch (parts[0].trim().toLowerCase()) {
        case "open":
        case "load":
            movie = new Movie(this, parts[1]);
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
