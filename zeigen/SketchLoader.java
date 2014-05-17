
import java.io.File;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;


public class SketchLoader extends ClassLoader {

    private static File path;

    public static void setPath(File baseDir) {
        path = new File(baseDir, "sketches");
        path.mkdir();
    }

    public SketchLoader(ClassLoader parent) {
        super(parent);
    }

    public Class loadClass(String classname) throws ClassNotFoundException {
        File file = new File(path, classname + ".class");

        if (! file.isFile()) {
            return super.loadClass(classname);
        }

        try {
            InputStream input = new FileInputStream(file);
            ByteArrayOutputStream buffer = new ByteArrayOutputStream();

            int data = input.read();
            while (data != -1) {
                buffer.write(data);
                data = input.read();
            }
            input.close();

            byte[] classData = buffer.toByteArray();
            return defineClass(classname, classData, 0, classData.length);
        } catch (IOException e) {
            // System.out.println("COULD NOT LOAD CLASS: " + classname);
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
            // System.out.println("COULD NOT LOAD CLASS: " + sketchname);
        }

        return null;
    }
}
