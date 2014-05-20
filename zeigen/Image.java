
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
        mode = args.trim();
    }

    public void zDraw() {
        background(zBackgroundColor);

        if (currentImage != null) {
            int x, y, w, h;

            switch(mode) {
            default: println("Unknown mode: " + mode);
            case "":
            case "stretched":
            case "full":
                image(currentImage, 0, 0, width, height);
                break;
            case "plain":
                w = currentImage.width;
                h = currentImage.height;

                x = (width - w)/2;
                y = (height - h)/2;
                image(currentImage, x, y, w, h);
                break;
            case "sized":
                int scale;
                w = currentImage.width;
                h = currentImage.height;

                // FIX ME: This is only true for images with same h/w ratios as window (and smaller?)
                scale = max(width, height) / max(w, h);

                w = w * scale;
                h = h * scale;

                x = (width - w)/2;
                y = (height - h)/2;

                image(currentImage, x, y, w, h);
                break;
            }
        }
    }

    public void zReceive(String message) {
        String[] parts = message.split(";", 2);
        if (parts.length == 2) {
            switch(parts[0]) {
            case "show":
            case "view":
            case "render":
            case "display":
            case "open":
            case "image": setImage(parts[1]); break;
            case "mode": mode = parts[1].trim(); break;
            case "preload":
            case "load": preload(parts[1]); break;
            default: println("Unknown message"); break;
            }
        } else if (parts.length == 1) {
            switch(parts[0]) {
            case "clearcache":
            case "clear": clearCache(); break;
            default: setImage(parts[0]);
            }
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
