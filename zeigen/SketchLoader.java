
import java.io.File;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;


public class SketchLoader extends ClassLoader {

    public SketchLoader(ClassLoader parent) {
        super(parent);
    }

    public Class loadClass(String classname) throws ClassNotFoundException {
        String filename = "/home/pilen/code/ubertex/zeigen/sketches/" + classname + ".class";
        File file = new File(filename);
        System.out.println("LOADER: " + filename);
        if (! file.isFile()) {
            System.out.println("GOING SUPER!");
            return super.loadClass(classname);
        }

        System.out.println("LOAD SKETCH");
        try {
            InputStream input = new FileInputStream(filename);
            ByteArrayOutputStream buffer = new ByteArrayOutputStream();

            int data = input.read();
            while (data != -1) {
                buffer.write(data);
                data = input.read();
            }
            input.close();
            System.out.println("DONE READING");
            byte[] classData = buffer.toByteArray();
            return defineClass(classname, classData, 0, classData.length);
        } catch (IOException e) {
            System.out.println("COULD NOT LOAD CLASS: " + classname);
        }
        return null;
    }

    public static ZSketch load(String sketchname) {

        try {
            ClassLoader parentClassLoader = SketchLoader.class.getClassLoader();
            SketchLoader classLoader = new SketchLoader(parentClassLoader);
            Class sketchClass = classLoader.loadClass(sketchname);

            ZSketch sketch = (ZSketch) sketchClass.newInstance();
            return sketch;
        } catch (ClassNotFoundException | InstantiationException | IllegalAccessException e) {
            System.out.println("COULD NOT LOAD CLASS: " + sketchname);
        }

        return null;
    }
}
