
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Collection;

import java.util.concurrent.locks.ReentrantLock;


public class Controller implements TaskPerformer {
    private ReentrantLock lock;

    private final TaskManager taskManager;

    private final HashSet<String> names;
    private ZSketch sketch;
    private String sketchName;

    private int zWidth;
    private int zHeight;
    private int zOffsetX;
    private int zOffsetY;
    private int zNoiseSeed;
    private int zRandomSeed;

    public Controller(ArrayList<String> names) {
        this(names, 0, 0);
    }

    public Controller(Collection<String> names, int zRandomSeed, int zNoiseSeed) {
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

        this.zWidth = 100;
        this.zHeight = 100;
        this.zOffsetX = 0;
        this.zOffsetY = 0;
        this.zRandomSeed = zRandomSeed;
        this.zNoiseSeed = zNoiseSeed;

        this.lock.unlock();
    }

    public void message(String message) {

        String[] headers = message.split(";", 4);
        if (headers.length == 4 || headers.length == 3) {
            String targets = headers[0];
            long time = Tools.parseTime(headers[1]);
            String command = headers[2].trim();
            String options = headers.length == 4 ? headers[3] : "";

            if (this.forMe(targets.split(" "))) {
                System.out.println("RECEIVED: " + message);
                switch (command) {
                case "sketch": this.sketch(time, options); break;
                case "sync": this.sync(options); break;
                default: this.taskManager.addTask(time, command, options); break;
                }
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

    public boolean hasName() {
        return !this.names.isEmpty();
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
        int sketchCleared = this.sketch.clearTasks();
        System.out.println("CONTROLLER CLEARED: " + cleared + "    SKETCH CLEARED: " + sketchCleared);
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
        String[] parts = options.split(";", 2);

        if (parts.length == 2) {
            try {
                // Ensures both seeds are integers
                int zOffsetX = Tools.parseInt(parts[0]);
                int zOffsetY = Tools.parseInt(parts[1]);

                this.lock.lock();
                this.zOffsetX = zOffsetX;
                this.zOffsetY = zOffsetY;
                this.lock.unlock();
            } catch (NumberFormatException e) {
                System.out.println("OFFSETS MUST BE INTEGERS");
            }

        } else {
            System.out.println("OFFSET NEEDS BOTH AN X-OFFSET AND AN Y-OFFSET");
        }

    }

    private void seed(String options) {
        String[] parts = options.split(";", 2);

        if (parts.length == 2) {
            try {
                // Ensures both seeds are integers
                int zRandomSeed = Tools.parseInt(parts[0]);
                int zNoiseSeed = Tools.parseInt(parts[1]);

                this.lock.lock();
                this.zRandomSeed = zRandomSeed;
                this.zNoiseSeed = zNoiseSeed;
                this.lock.unlock();
            } catch (NumberFormatException e) {
                System.out.println("SEEDS MUST BE INTEGERS");
            }

        } else {
            System.out.println("SEED NEEDS BOTH A RANDOMSEED AND A NOISESEED");
        }
    }

    private void sketch(long time, String message) {
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

    private void start(String message) {
        String[] parts = message.split(";", 4);

        String sketchName = "";
        int zOffsetX = this.zOffsetX;
        int zOffsetY = this.zOffsetY;
        String arguments = "";

        switch (parts.length) {
        default: System.out.println("WRONG NUMBER OF ARGUMENTS TO START"); return;
        case 2: System.out.println("EITHER BOTH OFFSETS ARE NEEDED OR NONE"); return;
        case 4: arguments = parts[3];
        case 3:
            try {
                // Ensures both seeds are integers
                int tempX = Tools.parseInt(parts[1]);
                int tempY = Tools.parseInt(parts[2]);

                zOffsetX = tempX;
                zOffsetY = tempY;
            } catch (NumberFormatException e) {
                System.out.println("OFFSETS MUST BE INTEGERS");
            }
        case 1: sketchName = parts[0];
        }



        this.lock.lock();

        if (this.sketch != null) {
            this.kill();
        }

        this.sketch = SketchLoader.load(sketchName);

        if (this.sketch != null) {
            this.sketch.zStart(arguments,
                               this.zWidth, this.zHeight,
                               zOffsetX, zOffsetY,
                               this.zRandomSeed, this.zNoiseSeed);
            this.sketchName = sketchName;
        } else {
            System.out.println("COULD NOT LOAD CLASS: " + sketchName);
        }

        this.lock.unlock();
    }

    private void sync(String options) {
        String[] parts = options.split(";", 2);

        if (parts.length == 2) {
            if (parts[0].equals("time") || parts[0].equals("servertime")) {
                try {
                    long serverTime = Tools.parseTime(parts[1]);

                    Time.setClock(serverTime);
                } catch (NumberFormatException e) {
                    System.out.println("SERVERTIME MUST BE A LONG");
                }
            } else {
                System.out.println("MALFORMED ARGUMENTS FOR SYNC");
            }
        } else {
            System.out.println("MISSING ARGUMENTS FOR SYNC");
        }
    }

    private void quit() {
        System.exit(0);
    }

    private void window(String options) {
        String[] parts = options.split(";", 2);

        if (parts.length == 2) {
            try {
                // Ensures both seeds are integers
                int zWidth = Tools.parseInt(parts[0]);
                int zHeight = Tools.parseInt(parts[1]);

                this.lock.lock();
                this.zWidth = zWidth;
                this.zHeight = zHeight;
                this.lock.unlock();
            } catch (NumberFormatException e) {
                System.out.println("SIZES MUST BE INTEGERS");
            }
        } else {
            System.out.println("OPTIONS FOR WINDOW COMMAND MUST CONSIST OF TWO INTEGERS");
        }
    }
}
