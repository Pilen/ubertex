
import java.io.File;
import java.io.IOException;

public class FileManager {
    private String source;
    private File destination;

    public FileManager(String source, String destination) {
        this(source, new File(destination));
    }

    public FileManager(String source, File destination) {
        this.source = source;
        this.destination = destination;
    }

    public void syncFiles() {
        String[] command = {"/usr/bin/rsync",
                            "-r",        // Recursive
                            "-u",        // Only download updated files
                            // We want to keep our temporary files, like pdfimages.
                            // "--delete",  // Delete files no longer present
                            "--partial", // Keep partial files
                            "-P",
                            "-e ssh",    // Use ssh?
                            this.source,
                            this.destination.getAbsolutePath()};
        sync(command);

    }

    public void cleanSync() {
        String[] command = {"/usr/bin/rsync",
                            "-r",        // Recursive
                            "-u",        // Only download updated files
                            "--delete",  // Delete files no longer present
                            "--partial", // Keep partial files
                            "-P",
                            "-e ssh",    // Use ssh?
                            this.source,
                            this.destination.getAbsolutePath()};
        sync(command);
    }

    private void sync(String[] command) {
        try {
            Process rsync = Runtime.getRuntime().exec(command);
            int returnValue = rsync.waitFor();
            System.out.println("RSYNC FINISHED WITH VALUE: " + returnValue);
        } catch (IOException e) {
            System.out.println("COULD NOT START RSYNC");
        } catch (InterruptedException e) {
            System.out.println("INTERRUPTED");
        }
    }
}
