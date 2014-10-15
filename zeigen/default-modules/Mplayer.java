
import java.io.*;

public class Mplayer extends Module {
    private class LineRedirecter extends Thread {
        private InputStream in;
        private OutputStream out;

        LineRedirecter(InputStream in, OutputStream out) {
            this.in = in;
            this.out = out;
        }
        public void run() {
            try {
                BufferedReader reader = new BufferedReader(new InputStreamReader(in));
                PrintStream printStream = new PrintStream(out);
                String line;

                while ((line = reader.readLine()) != null) {
                    printStream.println(line);
                }
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
        }
    }
    public void setup() {
        try {
            Process mplayerProcess = Runtime.getRuntime().exec("mplayer -slave -quiet -idle /home/pilen/satyr-2014/Ken_v07_subs_v01.mp4");

            PipedInputStream readFrom = new PipedInputStream(256 * 1024);
            PipedOutputStream writeTo = new PipedOutputStream(readFrom);
            BufferedReader mplayerOutErr = new BufferedReader(new InputStreamReader(readFrom));
            new LineRedirecter(mplayerProcess.getInputStream(), writeTo).start();
            new LineRedirecter(mplayerProcess.getErrorStream(), writeTo).start();
            PrintStream mplayerIn = new PrintStream(mplayerProcess.getOutputStream());
            mplayerIn.print("loadfile \"/home/pilen/satyr-2014/Ken_v07_subs_v01\" 0\n");
            mplayerIn.flush();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
    }
}
