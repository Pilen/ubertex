
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
            if (parts[1].contains("p")) {
                //TODO: change "p" back to "%"
                parts = parts[1].replace("p","").split(",");
                try {
                    // make optional:
                    float tempOffsetX = Tools.parseFloat(parts[0]);
                    float tempOffsetY = Tools.parseFloat(parts[1]);
                    float xScale = Tools.parseFloat(parts[2]);
                    float yScale = Tools.parseFloat(parts[3]);
                    float tempWidth = (width/100) * xScale;
                    float tempHeight = (height/100) * yScale;
                    // offsetX = tempOffsetX;
                    // offsetY = tempOffsetY;
                    offsetX = (width - tempWidth) / 2 + tempOffsetX;
                    offsetY = (height - tempHeight) / 2 + tempOffsetY;

                    width = tempWidth;
                    height = tempHeight;
                    height = (height/100) * yScale;
                } catch (NumberFormatException e) {
                    println("Wrom mode specifier");
                }
            } else {
                parts = parts[1].split(",");
                try {
                    float tempOffsetX = Tools.parseFloat(parts[0]);
                    float tempOffsetY = Tools.parseFloat(parts[1]);
                    float tempWidth = Tools.parseFloat(parts[2]);
                    float tempHeight = Tools.parseFloat(parts[3]);

                    offsetX = tempOffsetX;
                    offsetY = tempOffsetY;
                    width = tempWidth;
                    height = tempHeight;
                } catch (NumberFormatException e) {
                    println("Wrong mode specifier");
                }
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
