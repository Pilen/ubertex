
import java.io.File;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;


public class ModuleLoader extends ClassLoader {

    private static File path;

    public static void setPath(File baseDir) {
        path = new File(baseDir, "modules");
        path.mkdir();
    }

    public ModuleLoader(ClassLoader parent) {
        super(parent);
    }

    public Class loadClass(String classname) throws ClassNotFoundException {
        File file = new File(path, classname + ".class");

        // System.out.println(file); //nice for debugging
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

    public static ZModule load(String modulename) {

        try {
            ClassLoader parentClassLoader = ModuleLoader.class.getClassLoader();
            ModuleLoader classLoader = new ModuleLoader(parentClassLoader);
            Class moduleClass = classLoader.loadClass(modulename);

            ZModule module = (ZModule) moduleClass.newInstance();
            return module;
        } catch (ClassNotFoundException | InstantiationException | IllegalAccessException e) {
            // System.out.println("COULD NOT LOAD CLASS: " + modulename);
        }

        return null;
    }
}
