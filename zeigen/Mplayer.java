
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.PrintStream;

public class Mplayer implements Runnable {


    public Mplayer(String args) {
        String filename = "/home/pilen/youtube/Penguins VS the Rope-A85WpaTHNlw.mp4";
        String command = "/usr/bin/mplayer -slave -quiet \"" + filename + "\"";

        try {
            Process mplayer = Runtime.getRuntime().exec(command);

            PipedInputStream readFrom = new PipedInputStream(256*1024);
            PipedOutputStream writeTo = new PipedOutputStream(readFrom);
            BufferedReader mplayerOutErr = new BufferedReader(new InputStreamReader(readFrom));

            StreamRedirecter s1 = new StreamRedirecter(mplayer.getInputStream(), writeTo);
            Thread thread1 = new Thread(s1);
            thread1.start();

            StreamRedirecter s2 = new StreamRedirecter(mplayer.getErrorStream(), writeTo);
            Thread thread2 = new Thread(s2);
            thread2.start();

            PrintStream mplayerIn = new PrintStream(mplayer.getOutputStream());

            mplayerIn.print("play");
            mplayerIn.print("\n");
            mplayerIn.flush();

        } catch (IOException e) {

        }
    }

    public void run() {

    }

    class StreamRedirecter implements Runnable {
        private InputStream in;
        private OutputStream out;

        public StreamRedirecter(InputStream in, OutputStream out) {
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
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }


}
