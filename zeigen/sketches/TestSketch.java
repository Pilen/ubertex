import processing.core.PImage;

public class TestSketch extends ZSketch {
    private int t;
    private PImage img;

    public void zSetup() {
        // size(400, 400);
        img = loadImage("mand.png");
        background(140, 140, 200);
        // background(random(255), random(255), random(255));
        this.t = 0;
    }

    public void zDraw() {
        background(140, 140, 200);
        stroke(0,0,0);

        ellipse(200 + (t % 100)*1,200,100,100);
        // rect(200 + (t % 4)*10,200,100,100);
        image(img, 10, 10);

        stroke(0,255,0);
        line(0, 0, 100, 0);
        line(0, 0, 0, 100);

        line(1600, 900, 1500, 900);
        line(1600, 900, 1600, 800);

        int q = 1;
        stroke(255,0,0);
        line(0+q, 0+q, 100+q, 0+q);
        line(0+q, 0+q, 0+q, 100+q);

        line(1600-q, 900-q, 1500-q, 900-q);
        line(1600-q, 900-q, 1600-q, 800-q);

        this.t++;
    }
}
