
import socket

ip = "127.0.0.1"
port = 1234
message = "Hello, World!"

def udp(ip, port, message):
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)

    sock.sendto(bytes(message, "utf-8"), (ip, int(port)))

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("IP", help="IP of the host")
    parser.add_argument("port", help="port of the host")
    parser.add_argument("message", help="The content of the message to send")
    group = parser.add_mutually_exclusive_group()
    group.add_argument("-u", "--UDP", help="Use the UDP protocol", action="store_const", const="udp", default="udp", dest="protocol")
    group.add_argument("-t", "--TCP", help="Use the TCP protocol", action="store_const", const="tcp", dest="protocol")
    group = parser.add_mutually_exclusive_group()
    group.add_argument("-n", "--newlines", help="turn \\n into newlines", action="store_true", default=True)
    group.add_argument("-N", "--no-newlines", help="do NOT turn \\n into newlines", action="store_false", dest="newlines")
    args = parser.parse_args()

    ip = args.IP
    port = args.port
    message = args.message

    protocol = args.protocol
    newlines = args.newlines

    if newlines:
        # Unescape escaped string by converting it to a bytestream and decoding it back to unicode
        message = bytes(message, "utf-8").decode("unicode-escape")

    if protocol == "udp":
        udp(ip, port, message)
    elif protocol == "tcp":
        print("TCP protocol not yet supported")
