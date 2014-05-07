#!/bin/python3

import sys
import os
import subprocess

def strip_end(text, ends):
    if isinstance(ends, str):
        ends = [ends]
    for ending in ends:
        if text.endswith(ending):
            return text[:-len(ending)]
    return text

def path(filename):
    return os.path.join(sys.path[0], filename)

available_programs = {
    "hilfe": "The help program (what you are currently running).",
    "schneider": "A tool for cutting videos/pictures into multiple files.",
    "zeitherr": "A time server.",
    "zusteller": "A tool for sending simple messages over a network (for debugging purposes).",
    "zeigen": "The program responsible for rendering stuff in the revy."
}

program_help = {
    "zusteller": ["python3", path("zusteller.py"), "-h"]
}

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("program", help="Help for a specific program", nargs="?", default="")
    args = parser.parse_args()

    program = strip_end(args.program, [".py", ".java"])

    if program == "":
        padding = max(map(len, available_programs)) + 1
        for program in sorted(available_programs):
            text = program + ":"
            text = text.ljust(padding + 4)
            text += available_programs[program]
            print(text)
    else:
        if program in available_programs:
            print(program + ":")
            print(available_programs[program])
            print()

            if program in program_help:
                subprocess.call(program_help[program])
        else:
            print("Sorry, the program was not recognized")
