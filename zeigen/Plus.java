
public class Plus extends ZModule {
    // Lots and lots of plusses on the screen!
    private int size = 20;
    private int NUM_PLUS = 500;
    private float[][] positions = new float[NUM_PLUS][3];

    public void zSetup(String message) {
        for (int i = 0; i < NUM_PLUS; i++) {
            // (x,y) coordinates
            positions[i][0] = random(width);
            positions[i][1] = random(height);

            // text size
            positions[i][2] = random(5, 40);
        }
    }

    public void zDraw() {
        background();
        textAlign(CENTER);
        for (int i = 0; i < NUM_PLUS; i++) {
            float x = positions[i][0];
            float y = positions[i][1];
            float size = positions[i][2];
            textSize(size);
            positions[i][1] += 0.05 * size;
            if (positions[i][1] > height) {
                positions[i][0] = random(width);
                positions[i][1] = 0;
            }
            text("+", x, y);
        }
    }
}
