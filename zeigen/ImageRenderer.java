
import processing.core.PImage;

public abstract class ImageRenderer extends Module {

    public void renderImage(PImage image, String mode) {
        float x = 0, y = 0, w = 0, h = 0;

        float offsetX = 0;
        float offsetY = 0;
        float width = this.width;
        float height = this.height;

        String[] parts = mode.split("/", 2);
        if (parts.length == 2) {
            mode = parts[0];
            parts = parts[1].split(",");
            try {
                offsetX = Tools.parseFloat(parts[0]);
                offsetY = Tools.parseFloat(parts[1]);
                width = Tools.parseFloat(parts[2]);
                height = Tools.parseFloat(parts[3]);
            } catch (NumberFormatException e) {
                println("Wrong mode specifier");
            }
        }

        switch(mode) {
        default: println("Unknown mode: " + mode);
        case "":
        case "stretched":
        case "full":
            image(image, x, y, width, height);
        break;
        case "plain":
            w = image.width;
            h = image.height;

            x = (width - w)/2;
            y = (height - h)/2;
            image(image, x + offsetX, y + offsetY, w, h);
            break;
        case "sized":
            w = image.width;
            h = image.height;

            float widthRatio = width/w;
            float heightRatio = height/h;

            if (widthRatio <= heightRatio) {
                h = width * (h/w);
                w = width;
            } else {
                w = height * (w/h);
                h = height;
            }

            x = (width - w)/2;
            y = (height - h)/2;

            image(image, x + offsetX, y + offsetY, w, h);
            break;
        }
    }
}
