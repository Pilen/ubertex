
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;

import java.net.DatagramSocket;
import java.net.DatagramPacket;
import java.net.SocketException;

public class UDPServer implements Server {

    private DatagramSocket socket;
    private Controller controller;

    /**
     * Create a new UDPServer
     *
     * When SocketException is thrown, you should discard this UDPServer object
     */
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
            // Decode buffer of UTF-8 bytes into UTF-8 String
            String message = new String(data, 0, packet.getLength(), StandardCharsets.UTF_8);

            System.out.println(message);
            this.controller.message(message);
        }
    }
}
