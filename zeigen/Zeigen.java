
import java.util.ArrayList;

import java.net.SocketException;


public class Zeigen {
    public static void main(String[] args) {
        Zeigen z = new Zeigen(args);
    }

    public Zeigen(String[] args) {
        ArrayList<String> names = new ArrayList<String>();
        names.add("brok");
        Controller controller = new Controller(names);

        for (String arg : args) {
            controller.message(arg);
        }

        if (! controller.hasName()) {
            System.out.println("ZEIGEN NEEDS A NAME");
            System.exit(1);
        }

        Server server;
        try {
            server = new UDPServer(controller, 9999);

            Thread thread = new Thread(server);
            thread.start();
        } catch (SocketException e) {
            System.exit(1);
        }

    }
}
