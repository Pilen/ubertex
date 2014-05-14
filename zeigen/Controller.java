
import java.util.ArrayList;
import java.util.HashSet;

import java.util.concurrent.locks.ReentrantLock;


public class Controller implements TaskPerformer {
    private ReentrantLock lock;

    private final TaskManager taskManager;

    private final HashSet<String> names;
    private ZSketch sketch;
    private String sketchName;

    private int zWidth;
    private int zHeigth;
    private int zNoiseSeed;
    private int zRandomSeed;

    public Controller(ArrayList<String> names) {
        this(names, 0, 0);
    }

    public Controller(ArrayList<String> names, int zRandomSeed, int zNoiseSeed) {
        this.lock = new ReentrantLock(true);
        this.lock.lock();

        this.taskManager = new TaskManager(this);
        Thread taskThread = new Thread(this.taskManager);
        taskThread.start();

        this.names = new HashSet<String>(names.size());
        for (String name : names) {
            this.names.add(name.toLowerCase());
        }

        this.sketch = null;
        this.sketchName = "";


        this.zRandomSeed = zRandomSeed;
        this.zNoiseSeed = zNoiseSeed;

        this.lock.unlock();
    }

    public void message(String message) {

        String[] headers = message.split(";", 4);
        if (headers.length == 4 || headers.length == 3) {
            String targets = headers[0];
            String time = headers[1];
            String command = headers[2];
            String options = headers.length == 4 ? headers[3] : "";

            if (this.forMe(targets.split(" "))) {
                System.out.println("RECEIVED: " + message);
                this.taskManager.addTask(time, command, options);
                this.sketch(time, options);
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

    public void doTask(String command, String options) {
        System.out.println("DOING: " + command + ";" + options);
        switch (command) {
        case "abort" : this.abort(); break;
        case "clearqueue": this.clearqueue(); break;
        case "exit": this.quit(); break;
        case "kill": this.kill(options); break;
        case "offset" : this.offset(options); break;
        case "quit": this.quit(); break;
        case "seed": this.seed(options); break;
        // case "sketch": all ready handled
        case "start": this.start(options); break;
        // case "sync": this.sync(); break;
        case "window" : this.window(options); break;
        default: System.out.println("UNSUPPORTED COMMAND: " + command); break;
        }
    }

    /*
     * COMMANDS:
     */

    private void abort() {
        this.lock.lock();
        this.clearqueue();
        this.kill();
        this.lock.unlock();
    }

    private void clearqueue() {
        this.lock.lock();
        int cleared = this.taskManager.clear();
        System.out.println(cleared);
        this.lock.unlock();
    }

    private void kill(String sketch) {
        this.lock.lock();
        if (sketch.isEmpty() || this.sketchName.toLowerCase().equals(sketch.toLowerCase())) {
            this.kill();
        } else {
            System.out.println("CAN'T KILL: " + sketch);
        }
        this.lock.unlock();
    }

    private void kill() {
        this.lock.lock();

        if (this.sketch != null) {
            this.sketch.exit();
            this.sketch = null;
            System.out.println("TERMINATING: " + this.sketchName);
            this.sketchName = "";
        } else {
            System.out.println("NO SKETCH TO KILL");
        }

        this.lock.unlock();
    }

    private void offset(String options) {

    }

    private void seed(String options) {
        String[] parts = options.split(";", 2);

        if (parts.length == 2) {
            try {
                // Ensures both seeds are integers
                int zRandomSeed = Integer.parseInt(parts[0]);
                int zNoiseSeed = Integer.parseInt(parts[1]);

                this.zRandomSeed = zRandomSeed;
                this.zNoiseSeed = zNoiseSeed;

            } catch (NumberFormatException e) {
                System.out.println("SEEDS MUST BE NUMBERS");
            }

        } else {
            System.out.println("SEED NEEDS BOTH A RANDOMSEED AND A NOISESEED");
        }
    }

    private void sketch(String time, String message) {
        String[] parts = message.split(";", 2);

        if (parts.length == 2) {
            String sketch = parts[0].trim();
            String options = parts[1];

            this.lock.lock();
            if (sketch.isEmpty() || sketch.equals(this.sketchName)) {
                this.sketch.addTask(time, message);
            } else {
                System.out.println("RECEIVED MESSAGE FOR WRONG SKETCH: " + sketch);
            }
            this.lock.unlock();
        } else {
            System.out.println("OPTIONS FOR SKETCH COMMAND MUST CONSIST OF TARGET AND MESSAGE");
        }
    }

    private void start(String sketchName) {
        this.lock.lock();

        if (this.sketch != null) {
            this.kill();
        }

        this.sketch = SketchLoader.load(sketchName);

        if (this.sketch != null) {
            this.sketch.zStart(sketchName, this.zWidth, this.zHeight, this.zRandomSeed, this.zNoiseSeed);
            this.sketchName = sketchName;
        } else {
            System.out.println("COULD NOT LOAD CLASS: " + sketchName);
        }

        this.lock.unlock();
    }

    private void quit() {
        System.exit(0);
    }

    private void window(String options) {
        String[] parts = message.split(";", 2);

        if (parts.length == 2) {
            String height= parts[0].trim();
            String width = parts[1].trim();

            this.lock.lock();
            this.
            this.lock.unlock();
        } else {
            System.out.println("OPTIONS FOR SKETCH COMMAND MUST CONSIST OF TARGET AND MESSAGE");
        }
    }
}
