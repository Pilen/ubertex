

public class Video extends ImageRenderer {

    ZMovie movie;
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
                movie.read();
                // print("-");
                renderImage(movie, mode);
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

    // TODO: is not called as this is not the reference given to Movie"
    public void movieEvent(ZMovie m) {
        m.read();
    }

    public void onDeath() {
        movie.stop();
        movie = null;
    }

}
