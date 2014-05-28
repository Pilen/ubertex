
public abstract class Module {

    private ZSketch zSketch;

    public final void initModule(ZSketch sketch) {
        this.zSketch = sketch;
    }


    public void draw() {}

    public void receive(String message) {}

    public void setup(String arguments) {
        this.setup();
    }

    public void setup() {}
}
