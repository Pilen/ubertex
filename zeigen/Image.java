
import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.Condition;

import processing.core.PImage;

public class Image extends ZSketch {

    private final ReentrantLock lock = new ReentrantLock(true);
    private Condition condition = lock.newCondition();

    private Map<String, PImage> files = new HashMap<String, PImage>();
    // private int zBackgroundColor = color(0, 0, 0);
    private PImage currentImage = null;

    private String mode = "";

    public void zSetup(String args) {
        String[] parts = args.split(";",2);
        if (parts.length == 2) {
            mode = parts[0].trim();
            setImage(parts[1]);
        } else {
            mode = args.trim();
        }
    }

    public void zDraw() {
        background(zBackgroundColor);

        if (currentImage != null) {
            renderImage(currentImage, mode);
        }
    }

    public void zReceive(String message) {
        String[] parts = message.split(";", 2);
        switch (parts[0].trim().toLowerCase()) {
        case "show":
        case "view":
        case "render":
        case "display":
        case "open":
        case "image": setImage(parts[1]); break;
        case "mode": mode = parts[1].trim(); break;
        case "preload":
        case "load": preload(parts[1]); break;
        case "clearcache":
        case "clear": clearCache(); break;
        case "unblank": case "unhide": case "unpause": case "continue":
        case "nop": break;
        default:
            setImage(message);
            break;
        }
    }

    private void setImage(String file) {
        lock.lock();
        if (files.containsKey(file)) {
            currentImage = files.get(file);
            if (currentImage == null) {
                // await Async loading of files here
            }
        }
        PImage image = loadImage(file);
        files.put(file, image);
        currentImage = image;
        lock.unlock(); // It is fine that we cary the lock until now, this task is the absolute most important!!!
    }

    private void preload(String file) {
        PImage image = loadImage(file);
        lock.lock();
        files.put(file, image);
        lock.unlock();

    }

    private void clearCache() {
        lock.lock();
        files.clear();
        lock.unlock();
    }
}
