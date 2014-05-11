
import java.util.ArrayList;
import java.util.HashSet;

import java.util.concurrent.locks.ReentrantLock;


public class Controller {
    private ReentrantLock lock;

    private final HashSet<String> names;
    private ZSketch sketch;
    private String sketchName;

    private final TaskManager taskManager;
    public Controller(ArrayList<String> names) {
        this.lock = new ReentrantLock(true);
        this.lock.lock();

        this.sketch = null;
        this.sketchName = "";

        this.taskManager = new TaskManager(this);
        Thread taskThread = new Thread(this.taskManager);
        taskThread.start();

        this.names = new HashSet<String>(names.size());
        for (String name : names) {
            this.names.add(name.toLowerCase());
        }

        this.lock.unlock();
    }

    public void message(String message) {

        String[] headers = message.split(";", 4);
        if (headers.length == 4) {
            String targets = headers[0];
            String time = headers[1];
            String command = headers[2];
            String options = headers[3];

            if (this.forMe(targets.split(" "))) {
                System.out.println("RECEIVED: " + message);
                this.taskManager.addTask(time, command, options);
                // this.command(command, time, options);
            } else {
                System.out.println("IGNORED MESSAGE");
            }
        } else {
            System.out.println("MALFORMED MESSAGE RECEIVED: " + message);
        }
    }

    private boolean forMe(String[] targets) {
        for (String target : targets) {
            if (this.names.contains(target.toLowerCase())) {
                return true;
            }
        }
        return false;
    }

    public void command(String command, String options) {
        System.out.println("DOING: " + command + ";" + options);
        switch (command) {
        // case "clearqueue": this.clearqueue(); break;
        case "kill": this.kill(); break;
        // case "quit": this.quit(); break;
        case "restart": this.kill(); this.start(options); break;
        case "sketch": this.sketch(options); break;
        case "start": this.start(options); break;
        // case "sync": this.sync(); break;
        default: System.out.println("UNSUPPORTED COMMAND: " + command);
        }
    }

    /*
     * COMMANDS:
     */

    private void kill() {
        this.lock.lock();

        if (this.sketch != null) {
            this.sketch.exit();
            this.sketch = null;
            this.sketchName = "";
        } else {
            System.out.println("NO SKETCH TO KILL");
        }

        this.lock.unlock();
    }

    private void sketch(String options) {

    }

    private void start(String sketchName) {
        this.lock.lock();

        if (this.sketch != null) {
            this.kill();
        }

        this.sketch = SketchLoader.load(sketchName);

        if (this.sketch != null) {
            this.sketch.zStart(sketchName);
            this.sketchName = sketch
        } else {
            System.out.println("COULD NOT LOAD CLASS: " + sketchName);
        }

        this.lock.unlock();
    }
}
