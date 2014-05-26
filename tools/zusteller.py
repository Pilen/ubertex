#!/bin/python3

import socket

host = "127.0.0.1"
port = 1234
message = "Hello, World!"

def udp(host, port, message, size=None, verbose=False):
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)

    bytemessage = bytes(message, "utf-8")

    if size is not None:
        bytemessage = (bytemessage + bytes(max(0, size - len(bytemessage))))[:size]

    if verbose:
        print(bytemessage)

    sock.sendto(bytemessage, (host, int(port)))

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("host", help="IP or hostname of the host.")
    parser.add_argument("port", help="port of the host.")
    parser.add_argument("message", help="The content of the message to send.")
    group = parser.add_mutually_exclusive_group()
    group.add_argument("-u", "--UDP", help="Use the UDP protocol (default).", action="store_const", const="udp", default="udp", dest="protocol")
    group.add_argument("-t", "--TCP", help="Use the TCP protocol.", action="store_const", const="tcp", dest="protocol")
    group = parser.add_mutually_exclusive_group()
    group.add_argument("-n", "--newlines", help="turn \\n into newlines (default).", action="store_true", default=True)
    group.add_argument("-N", "--no-newlines", help="do NOT turn \\n into newlines.", action="store_false", dest="newlines")
    parser.add_argument("-s", "--size", help="Specify a fixed size for the data, truncating message if longer.", type=int)
    parser.add_argument("-v", "--verbose", help="Increase verbosity.", action="store_true")
    args = parser.parse_args()

    host = args.host
    port = args.port
    message = args.message

    protocol = args.protocol
    newlines = args.newlines
    size = args.size
    verbose = args.verbose

    if protocol == "udp":
        udp(host, port, message, size, verbose)
    elif protocol == "tcp":
        print("TCP protocol not yet supported")
