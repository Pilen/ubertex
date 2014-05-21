
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import processing.core.PImage;

public class PDF extends ImageRenderer {

    private File pdfFile;
    private long lastModified;
    private ArrayList<PImage> images;
    private int current = 2;
    private String mode = "";

    public void zSetup(String args) {
        String[] parts = args.split(";",2);
        if (parts.length == 2) {
            mode = parts[0].trim();
            load(parts[1]);
            current = 0;
        } else {
            mode = args.trim();
        }
    }

    public void zDraw() {
        background();

        if (images != null && current < images.size()) {
            PImage image = images.get(current);
            renderImage(image, mode);
        }
    }

    public void zReceive(String message) {
        String[] parts = message.split(";",2);
        switch (parts[0].trim().toLowerCase()) {
        case "show":
        case "view":
        case "render":
        case "display":
        case "open":
        case "image":
        case "pdf":
        case "load":
            current = 0;
            load(parts[1]);
            break;
        case "reload":
            load(parts[1]);
            break;
        case "goto":
            goTo(parts[1]);
            break;
        case "openat":
            goTo(parts[1]);
            load(parts[2]);
            break;
        case "next": case "forward":
            goTo(current + 1);
            break;
        case "previous": case "backward": case "back":
            goTo(current - 1);
            break;
        case "mode": mode = parts[1].trim(); break;
        case "unblank": case "unhide": case "unpause": case "continue": case "nop":
        default:
            break;
        }
    }

    private void load(String filename) {
        long start = 0;
        File pdf = dataFile(filename);

        if (!pdf.exists()) {
            println("Could not find file: " + pdf);
            return;
        }

        File tmpDir;
        for (int i = 0;; i++) {
            tmpDir = new File(pdf.getParent(), "pdf2imgtmpdir-"+i);
            if (!tmpDir.exists()) {
                tmpDir.mkdir();
                break;
            }
        }

        File destination = new File(tmpDir, "img%d.png");
        String[] command = {"/usr/bin/mudraw",
                            "-r", "300",
                            "-c", "rgba",
                            "-o", destination.getAbsolutePath(),
                            pdf.getAbsolutePath()};
        try {
            start = Time.now();
            println("mudraw");
            Process mudraw = Runtime.getRuntime().exec(command);
            mudraw.waitFor();
            println(Time.now() - start);
            start = Time.now();
            println("mudraw done");
        } catch (IOException e) {
            println("COULD NOT CALL MUDRAW");
        } catch (InterruptedException e) {
            println("Interrupted");
        }

        images = new ArrayList<PImage>(100); // .pdfs are typically long

        // First file is numbered 1
        for (int i = 1;; i++) {
            File file = new File(tmpDir, "img" + i + ".png");
            if (! file.exists()) {
                break;
            }
            PImage image = loadImage(file.getAbsolutePath());
            images.add(image);
        }

        println(Time.now() - start);
        println("PDF loaded!");
        println(images.size());
    }

    private void goTo(String slidenumber) {
        try {
            int slide = Tools.parseInt(slidenumber);
            goTo(slide);
        } catch (NumberFormatException e) {
            println("Goto destination not an integer");
        }
    }

    private void goTo(int slide) {
        if (images == null || slide < images.size()) {
            current = slide;
        } else {
            println("Cannot go to slide " + slide + "out of bounds");
            }

    }
}
