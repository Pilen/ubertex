import java.net.SocketException;

public class Zeigen {
    public static void main(String[] args) {
        Zeigen z = new Zeigen(args);
    }

    public Zeigen(String[] args) {
        Controller controller = new Controller();
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
