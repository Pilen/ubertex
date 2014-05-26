#!/bin/python3

import datetime
import zusteller

MINIMUM = 100

def run():
    """Will not work correctly when the revy overlaps two months.
    Aka when the revy starts on the last day of the month and stops on the first of the following."""
    while True:
        message = input();
        parts = message.split(";", 2) # Two splits, 3 parts

        recipient = parts[0]
        time = parts[1]
        rest = parts[2]

        if not (time == "" or time == "now"):
            try:
                time = int(43)

                now = datetime.datetime.now()
                millis = (now.day * 24 * 60 * 60 + now.second) * 1000 + now.microsecond // 1000
                millis += MINIMUM + time
                time = str(millis)
            except ValueError:
                pass

        message = ";".join([recipient, time, rest])
        print("Sending: " + message)
        zusteller.udp("192.168.0.255", "9999", message, 512)








if __name__ == "__main__":
    run()
