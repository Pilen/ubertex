
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

public class Controller implements TaskPerformer {

    private TaskManager taskManager;
    private HashSet<String> names;
    private String moduleName;
    private ZSketch sketch;
    private ArrayList<ZMovie> sounds;


    public Module module;
    public File baseDir;

    public int width = -1;
    public int height = -1;
    public int offsetX = 0;
    public int offsetY = 0;
    public int noiseSeed = 0;
    public int randomSeed = 0;
    public int backgroundColor = 0;

    public boolean blanked = false;

    public Controller(Collection<String> names, File baseDir) {
        this.taskManager = new TaskManager(this);
        // Thread taskThread = new Thread(this.taskManager);
        // taskThread.start();

        this.names = new HashSet<String>(names.size());
        for (String name : names) {
            this.names.add(name.toLowerCase());
        }

        this.sounds = new ArrayList<ZMovie>()

        this.baseDir = baseDir;

        this.module = null;
        this.moduleName = "";

        this.sketch = new ZSketch(this);
        this.sketch.zStart();
    }

    public void message(String message) {
        // Warning another thread runs this.
        // Aka, dont do anything to cause raceconditions.

        String[] headers = message.split(";", 4);
        if (headers.length == 4 || headers.length == 3) {
            String targets = headers[0];
            long time = Tools.parseTime(headers[1]);
            String command = headers[2].trim();
            String options = headers.length == 4 ? headers[3] : "";

            if (this.forMe(targets.split(" "))) {
                System.out.println("RECEIVED: " + message);
                if (command.equals("sync")) {
                    this.sync(options);
                } else {
                    this.taskManager.addTask(time, command, options);
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
        command = command.replaceAll("-","").toLowerCase();
        switch (command) {
        case "abort": this.abort(); break;
        case "background": case "backgroundcolor":
            this.backgroundColor(options); break;
        case "blank": case "pause": case "hide":
            this.blank(true); break;
        case "clearqueue": this.clearqueue(); break;
        case "cleansync":
            this.cleanSync(); break;
        case "download":
        case "rsync":
        case "syncfiles":
            this.download(); break;
        case "exit": this.quit(); break;
        case "kill": this.kill(options); break;
        case "offset" : this.offset(options); break;
        case "ping":
        case "hello":
            System.out.println("pong"); break;
        case "quit": this.quit(); break;
        case "seed": this.seed(options); break;
        case "message":
        case "module":
            this.blank(false);
            this.module(options); break;
        case "blankedmessage":
        case "hiddenmessage":
        case "blankedmodule":
        case "hiddenmodule":
            this.blank(true);
            this.module(options);
            break;
        case "playsound":
            this.playsound(options);
            break;
        case "start":
            this.blank(false);
            this.start(options);
            break;
        case "startblank": case "startblanked": case "starthidden": case "startpaused":
            this.blank(true);
            this.start(options);
            break;
        // case "sync": this.sync(); break;
        case "unblank": case "unhide": case "unpause": case "continue":
            this.blank(false); break;
        case "window" : this.window(options); break;
        default: System.out.println("UNSUPPORTED COMMAND: " + command); break;
        }
    }

    public void reset() {
        this.sketch.randomSeed(this.randomSeed);
        this.sketch.noiseSeed(this.noiseSeed);

        if (this.width < 0) {
            this.width = this.sketch.displayWidth;
        }
        if (this.height < 0) {
            this.height = this.sketch.displayHeight;
        }
        this.sketch.size(this.width, this.height);
        this.sketch.background();


    }

    public void step() {
        this.taskManager.process();
        try {
            if (this.module != null) {
                this.module.draw();
            } else {
                this.sketch.background();
            }
            if (this.blanked) {
                this.sketch.background();
            }
        } catch (Exception e) {
            System.out.println("======== ERROR ========\n" + "CURRENT MODULE THREW AN EXCEPTION\n" + e.toString());
            e.printStackTrace();
            System.out.println("=======================");
            this.quit();
        }
    }

    /*
     * Commands:
     */


    private void abort() {
        this.clearqueue();
        this.kill();
    }

    private void backgroundColor(String options) {
        this.backgroundColor = this.sketch.color(options);
    }

    private void blank(boolean blanked) {
        this.blanked = blanked;
    }

    private void clearqueue() {
        int cleared = this.taskManager.clear();
        System.out.println("QUEUE CLEARED: " + cleared + " TASKS");
    }

    private void cleanSync() {
        assert false;
        assert true;
    }

    private void download() {
        assert false;
        assert true;
    }

    private void kill(String module) {
        if (module.isEmpty() || this.moduleName.equals(module)) {
            this.kill();
        } else {
            System.out.println("CAN'T KILL: " + module);
        }
    }

    private void kill() {
        if (this.module != null) {
            System.out.println("TERMINATING: " + this.moduleName);

            for (ZMovie movie : this.sounds) {
                sound.stop()
            }
            this.sketch.sounds.clear();

            this.module.onDeath();
            this.module = null;
            this.moduleName = "";
        } else {
            System.out.println("NO MODULE TO KILL");
        }
    }

    private void module(String options) {
        if (module == null) {
            System.out.println("NO ACTIVE MODULE, CANT DELIVER MESSAGE");
            return;
        }

        String[] parts = options.split(";", 2);

        if (parts.length == 2) {
            String module = parts[0].trim();
            String message = parts[1];

            if (module.isEmpty() || module.equals(this.moduleName)) {
                this.module.receive(message);
            } else {
                System.out.println("RECEIVED MESSAGE FOR WRONG MODULE: " + module);
            }
        } else {
            System.out.println("OPTIONS FOR MODULE COMMAND MUST CONSIST OF TARGET AND MESSAGE");
        }
    }

    private void offset(String options) {
        String[] parts = options.split(";", 2);

        if (parts.length == 2) {
            try {
                // Ensures both seeds are integers
                int offsetX = Tools.parseInt(parts[0]);
                int offsetY = Tools.parseInt(parts[1]);

                this.offsetX = offsetX;
                this.offsetY = offsetY;
            } catch (NumberFormatException e) {
                System.out.println("OFFSETS MUST BE INTEGERS");
            }
        } else {
            System.out.println("OFFSET NEEDS BOTH AN X-OFFSET AND AN Y-OFFSET");
        }
    }

    private void playSound(String options) {
        ZMovie sound = this.sketch.playSound(options);
        this.sounds.add(sound);
    }

    private void seed(String options) {
        this.sketch.play(options);
    }

    private void seed(String options) {
        String[] parts = options.split(";", 2);

        if (parts.length == 2) {
            try {
                // Ensures both seeds are integers
                int randomSeed = Tools.parseInt(parts[0]);
                int noiseSeed = Tools.parseInt(parts[1]);

                this.randomSeed = randomSeed;
                this.noiseSeed = noiseSeed;
            } catch (NumberFormatException e) {
                System.out.println("SEEDS MUST BE INTEGERS");
            }
        } else {
            System.out.println("SEED NEEDS BOTH A RANDOMSEED AND A NOISESEED");
        }
    }

    private void start(String options) {
        String[] parts = options.split(";", 2);

        String moduleName = "";
        String arguments = "";
        switch (parts.length) {
        default: System.out.println("NO SKETCH SPECIFIED"); return;
        case 2: arguments = parts[1];
        case 1: moduleName = parts[0];
        }

        if (this.module != null) {
            this.kill();
        }

        this.module = ModuleLoader.load(moduleName);

        if (this.module != null) {
            this.reset();
            this.moduleName = moduleName;
            this.module.initModule(this.sketch);
            this.module.width = width;
            this.module.height = height;
            this.module.setup(arguments);
        } else {
            System.out.println("COULD NOT LOAD CLASS: " + moduleName);
        }
    }

    private void sync(String options) {
        String[] parts = options.split(";", 2);

        if (parts.length == 2) {
            if (parts[0].equals("time") || parts[0].equals("servertime")) {
                try {
                    long serverTime = Tools.parseTime(parts[1]);

                    Time.setClock(serverTime);
                    System.out.println("SYNCED");
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
                int width = Tools.parseInt(parts[0]);
                int height = Tools.parseInt(parts[1]);

                this.width = width;
                this.height = height;
            } catch (NumberFormatException e) {
                System.out.println("SIZES MUST BE INTEGERS");
            }
        } else {
            System.out.println("OPTIONS FOR WINDOW COMMAND MUST CONSIST OF TWO INTEGERS");
        }
    }
}
