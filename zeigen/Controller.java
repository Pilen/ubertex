
public class Controller {

    private HashSet<String> names;
    private File baseDir;
    private Module module;
    private String moduleName;
    private ZSketch sketch;

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

        this.baseDir = baseDir;

        this.module = null;
        this.moduleName = "";

        this.sketch = new ZSketch(this);
        this.sketch.zStart();
    }

    public void message(String message) {

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
        case "module": this.module(options); break;
        case "start": this.start(options); break;
        case "startblank": case "startblanked": case "starthidden": case "startpaused":
            this.startBlanked(options); break;
        // case "sync": this.sync(); break;
        case "unblank": case "unhide": case "unpause": case "continue":
            this.blank(false); break;
        case "window" : this.window(options); break;
        default: System.out.println("UNSUPPORTED COMMAND: " + command); break;
        }
    }

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

    private void kill(String module) {
        if (module.isEmpty() || this.moduleName.toLowerCase().equals(module.toLowerCase())) {
            this.kill();
        } else {
            System.out.println("CAN'T KILL: " + module);
        }
    }

    private void kill() {
        if (this.module != null) {
            this.module = null;
            System.out.println("TERMINATING: " + this.moduleName);
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

        String[] parts = message.split(";", 2);

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
        this.start(message, false);
    }

    private void startBlanked(String options) {
        this.start(message, true);
    }

    private void start(String options, boolean blanked) {
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
            this.moduleName = moduleName;
            this.module.initModule(this.sketch);
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
        System.exit();
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
