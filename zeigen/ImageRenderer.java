
import processing.core.PImage;

public abstract class ImageRenderer extends ZModule {


    public void renderImage(PImage image, String mode) {
        float x, y, w, h;

        switch(mode) {
        default: println("Unknown mode: " + mode);
        case "":
        case "stretched":
        case "full":
            image(image, 0, 0, width, height);
        break;
        case "plain":
            w = image.width;
            h = image.height;

            x = (width - w)/2;
            y = (height - h)/2;
            image(image, x, y, w, h);
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

            image(image, x, y, w, h);
            break;
        }
    }
}
