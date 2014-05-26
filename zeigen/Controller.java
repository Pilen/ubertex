
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Collection;

import java.io.File;

import java.util.concurrent.locks.ReentrantLock;


public class Controller implements TaskPerformer {
    private ReentrantLock lock;

    private final TaskManager taskManager;

    private final HashSet<String> names;
    private final File baseDir;
    private ZModule module;
    private String moduleName;

    private FileManager fileManager;

    private int zWidth;
    private int zHeight;
    private int zOffsetX;
    private int zOffsetY;
    private int zNoiseSeed;
    private int zRandomSeed;
    private String zBackgroundColor;

    public Controller(Collection<String> names, File baseDir) {
        this.lock = new ReentrantLock(true);
        this.lock.lock();

        this.taskManager = new TaskManager(this);
        Thread taskThread = new Thread(this.taskManager);
        taskThread.start();

        this.names = new HashSet<String>(names.size());
        for (String name : names) {
            this.names.add(name.toLowerCase());
        }

        this.baseDir = baseDir;

        this.module = null;
        this.moduleName = "";

        this.fileManager = new FileManager("pilen@192.168.0.10:av/2014/", baseDir);

        this.zWidth = -1; //displayWidth
        this.zHeight = -1; //displayHeigth
        this.zOffsetX = 0;
        this.zOffsetY = 0;
        this.zRandomSeed = zRandomSeed;
        this.zNoiseSeed = zNoiseSeed;
        this.zBackgroundColor = "";

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
                case "module": this.module(time, options); break;
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
        command = command.replaceAll("-","").toLowerCase();
        switch (command) {
        case "abort": this.abort(); break;
        case "background": case "backgroundcolor":
            this.backgroundColor(options); break;
        case "blank": case "pause": case "hide":
            this.blank(true); break;
        case "clearqueue": this.clearqueue(); break;
        case "download":
        case "rsync":
        case "syncfiles":
            this.download(); break;
        case "exit": this.quit(); break;
        case "kill": this.kill(options); break;
        case "offset" : this.offset(options); break;
        case "quit": this.quit(); break;
        case "seed": this.seed(options); break;
        // case "module": all ready handled
        case "start": this.start(options); break;
        case "startblank": case "startblanked": case "starthidden": case "startpaused":
            this.startPaused(options); break;
        // case "sync": this.sync(); break;
        case "unblank": case "unhide": case "unpause": case "continue":
            this.blank(false); break;
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

    private void backgroundColor(String options) {
        this.lock.lock();
        this.zBackgroundColor = options;
        this.lock.unlock();
    }

    private void blank(boolean blanked) {
        this.lock.lock();
        if (this.module != null) {
            this.module.zBlank = blanked;
        }
        this.lock.unlock();
    }

    private void clearqueue() {
        this.lock.lock();
        int cleared = this.taskManager.clear();
        int moduleCleared = 0;
        if (this.module != null) {
            moduleCleared = this.module.clearTasks();
        }
        System.out.println("CONTROLLER CLEARED: " + cleared + "    MODULE CLEARED: " + moduleCleared);
        this.lock.unlock();
    }

    private void download() {
        this.lock.lock();
        this.fileManager.syncFiles();
        this.lock.unlock();
    }

    private void kill(String module) {
        this.lock.lock();
        if (module.isEmpty() || this.moduleName.toLowerCase().equals(module.toLowerCase())) {
            this.kill();
        } else {
            System.out.println("CAN'T KILL: " + module);
        }
        this.lock.unlock();
    }

    private void kill() {
        this.lock.lock();

        if (this.module != null) {
            this.module.exit();
            this.module = null;
            System.out.println("TERMINATING: " + this.moduleName);
            this.moduleName = "";
        } else {
            System.out.println("NO MODULE TO KILL");
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

    private void module(long time, String message) {
        if (module == null) {
            System.out.println("NO ACTIVE MODULE, CANT DELIVER MESSAGE");
            return;
        }

        String[] parts = message.split(";", 2);

        if (parts.length == 2) {
            String module = parts[0].trim();
            String options = parts[1];

            this.lock.lock();
            if (module.isEmpty() || module.equals(this.moduleName)) {
                this.module.addTask(time, options);
            } else {
                System.out.println("RECEIVED MESSAGE FOR WRONG MODULE: " + module);
            }
            this.lock.unlock();
        } else {
            System.out.println("OPTIONS FOR MODULE COMMAND MUST CONSIST OF TARGET AND MESSAGE");
        }
    }

    private void start(String message) {
        this.start(message, false);
    }

    private void startPaused(String message) {
        this.start(message, true);
    }

    private void start(String message, boolean blanked) {
        String[] parts = message.split(";", 4);

        String moduleName = "";
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
        case 1: moduleName = parts[0];
        }



        this.lock.lock();

        if (this.module != null) {
            this.kill();
        }

        this.module = ModuleLoader.load(moduleName);

        if (this.module != null) {
            this.module.zBlank = blanked;
            this.module.zStart(arguments, this.baseDir,
                               this.zWidth, this.zHeight,
                               zOffsetX, zOffsetY,
                               this.zRandomSeed, this.zNoiseSeed,
                               this.zBackgroundColor);
            this.moduleName = moduleName;
        } else {
            System.out.println("COULD NOT LOAD CLASS: " + moduleName);
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
