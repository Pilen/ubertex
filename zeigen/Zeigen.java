
import java.util.ArrayList;
import java.util.Arrays;

import java.io.File;

import java.net.SocketException;


public class Zeigen {
    public static void main(String[] args) {
        Zeigen z = new Zeigen(args);
    }

    public Zeigen(String[] args) {

        if (args.length < 3) {
            System.out.println("NOT ENOUGH ARGUMENTS FOR ZEIGEN");
            return;
        }

        String[] nameArray = args[0].split("[;\\s]");
        ArrayList<String> names = new ArrayList<String>(Arrays.asList(nameArray));
        names.add("all");
        names.add("");

        int port = -1;
        try {
            port = Tools.parseInt(args[1]);
        } catch (NumberFormatException e) {
            System.out.println("PORT MUST BE AN INTEGER");
            System.exit(1);
        }

        File path = new File(args[2]);
        ModuleLoader.setPath(path);



        Controller controller = new Controller(names, path);

        for (int i = 3; i < args.length; i++) {
            controller.message(args[i]);
        }


        Server server;
        try {
            server = new UDPServer(controller, port);

            Thread thread = new Thread(server);
            thread.start();
        } catch (SocketException e) {
            System.exit(1);
        }

    }
}
