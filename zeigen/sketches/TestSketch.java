
// package sketches;

import zeigen.ZSketch;

public class TestSketch extends ZSketch {
    private int t;

    public void zSetup() {
        size(400, 400);
        background(140, 140, 200);
        // background(random(255), random(255), random(255));
        this.t = 0;
    }

    public void zDraw() {
        // ellipse(200 + (t % 4)*10,200,100,100);
        rect(200 + (t % 4)*10,200,100,100);
        this.t++;
    }
}
