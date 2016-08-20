
public class Bitcoin extends Module {

    int POINTS = 50;
    float[] array = new float[POINTS];
    int current = 0;
    boolean playing = true;

    public void draw() {
        background();
        stroke(255,255,255);
        line(312,250,312,250+220);
        line(312,250+220,312+400,250+220);

        if (playing && current % 2 == 0) {
            for (int i = 1; i < POINTS; i++) {
                array[i-1] = array[i];
            }
            array[POINTS-1] = number();
        }

        for (int i = 1; i < POINTS; i++) {
            line(312 + (400/POINTS) * (i-1),
                 array[i-1],
                 312 + (400/POINTS) * (i),
                 array[i]);
        }

        textAlign(RIGHT);
        textSize(14);
        text("Meget\nværd", 312-10, 250+20);
        text("Intet\nværd", 312-10, 250+220-20);
        text("Bitcoin kurs", 312 + 400/2+20,250+220+20);
        current++;
    }

    public void receive(String message) {
        switch (message) {
        case "play":
        case "go": playing = true; break;
        case "stop": playing = false; break;
        case "up": playing =false;
            array[POINTS-1] = random(250,250+30);
            break;
        case "down": playing = false;
            array[POINTS-1] = random(250+220-30,250+220);
            break;
        }
    }

    private float number() {
        return random(250, 250+220);
    }
}
