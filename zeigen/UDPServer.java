import java.io.IOException;
import java.io.UnsupportedEncodingException;

import java.net.DatagramSocket;
import java.net.DatagramPacket;
import java.net.SocketException;

public class UDPServer implements Server {

    private DatagramSocket socket;
    private Controller controller;

    public UDPServer(Controller controller, int port) throws SocketException {
        this.socket = new DatagramSocket(port);
        this.controller = controller;
    }

    public void run() {
        DatagramPacket packet = new DatagramPacket(new byte[Server.PACKET_SIZE], Server.PACKET_SIZE);

        while (true) {
            try {
                socket.receive(packet);
            } catch (IOException e) {
                // It is not really documented what would cause this exception and what it implies.
                throw new RuntimeException(e.getMessage(), e);
            }
            byte[] data = packet.getData();
            String message;
            try {
                message = new String(data, "UTF-8");
            } catch (UnsupportedEncodingException e) {
                // Should never happen!
                throw new AssertionError(e);
            }
            System.out.println(message.substring(message.length()-2, message.length()));
            System.out.println(message.length());
            if (message.endsWith("\n")) {
                System.out.println("newline!!!");
            }
            System.out.println("Received from: " + packet.getAddress() + ":" + packet.getPort() + ":" + message);
            this.controller.message(message);
        }
    }
}
