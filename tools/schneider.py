
import subprocess, re, os, sys, getopt

def delete_created_files(mediapath, slices):
    for i in range(slices):
        try:
            os.remove(name_file(mediapath, i))
        except FileNotFoundError as e:
            continue


def name_file(mediapath, number):
    base, ext = os.path.splitext(mediapath)
    number = str(number).rjust(2, "0") # assumes we wont get numbers bigger than 99
    return base + "-" + number + ext

def get_size(mediapath):
    pattern = re.compile(r"([0-9]{2,})x([0-9]{2,})")
    ffmpeg = subprocess.Popen(["ffmpeg", "-i", mediapath], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = ffmpeg.communicate()
    match = pattern.search(stderr.decode())
    if match:
        x, y = map(int, match.groups())
        return x, y
    else:
        raise LookupError("Could not establish dimensions of requested file")

def crop(mediapath, slices, ffmpegargs=[]):
    width, height = get_size(mediapath)
    w = width//slices
    h = height
    print("Cropping \"{mediapath}\" ({width}x{height}) into {slices} slices of {w}x{h}".format(width=width, height=height, w=w, h=h, slices=slices, mediapath=mediapath))

    for i in range(slices):
        x = w * i
        y = 0
        out = name_file(mediapath, i)
        print("\n\nBuilding \"{0}\"".format(out))
        # subprocess.Popen(["ffmpeg", "-i", mediapath, "-filter:v", "crop={w}:{h}:{x}:{y}".format(w=w, h=h, x=x, y=y), "-c:v", "libx264", "-c:a", "aac", out])
        ffmpeg = subprocess.Popen(["ffmpeg", "-i", mediapath, "-filter:v", "crop={w}:{h}:{x}:{y}".format(w=w, h=h, x=x, y=y), "-strict", "-2"] + ffmpegargs + [out])
        try:
            ffmpeg.communicate()
        except KeyboardInterrupt as e:
            print("Aborting")
            ffmpeg.kill()
            delete_created_files(mediapath, slices)
            sys.exit(2)


helptext = """
Cuts a media file into multiple slices
usage: schneider.py -n slices files...
"""

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hn:" "helptext")
    except getopt.GetoptError as error:
        print("Wrong usage/invalid arguments")
        print(helptext)
        sys.exit(2)

    slices = 3
    ffmpegargs = []

    for opt, value in opts:
        if opt in ("-h", "--helptext"):
            print(helptext)
            sys.exit()
        elif opt in ("-n"):
            slices = int(value)
        elif opt in ("-a", "--ffmpeg", "--ffmpeg-args"):
            ffmpegargs = value.split()
        else:
            print("Wrong usage/invalid arguments")
            print(helptext)
            sys.exit(2)
    if len(args) == 0:
        print("No file specified")
        print(helptext)
        sys.exit(2)

    for file in args:
        if not os.path.exists(file):
            print("Could not find file: \"{0}\"".format(os.path.abspath(file)))
            sys.exit(2)
        else:
            delete_created_files(file, slices)
            crop(file, slices, ffmpegargs)

if __name__ == "__main__":
    main()
