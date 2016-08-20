
public class Bash extends Module {

    int current = 0;
    int x = 312;
    int y = 250;

    String a = "PING datamat.local (127.0.0.1) 56(84) bytes of data.\n";
    String b = "64 bytes from 127.0.0.1: icmp_seq=1 ttl=54 time=4.33 ms\n";
    String c = "64 bytes from 127.0.0.1: icmp_seq=1 ttl=54 time=10.38 ms\n";
    String d = "64 bytes from 127.0.0.1: icmp_seq=1 ttl=54 time=100.36 ms\n";
    String e = "...";

    ZMovie sound = loadMovie("ping.mp3");
    ZMovie flatline = loadMovie("ping.mp3");
    long next = 0;

    public void draw(){
        background();

        // if (mili
        textSize(14);
        switch (current) {
        default:
        case 0: break;
        case 1: text(a, x, y);
            next += 1000*1; break;
        case 2: ping(a+b, x, y);
            next += 1000*1; break;
        case 3: ping(a+b+c, x, y);
            next += 1000*2; break;
        case 4: ping(a+b+c+d, x, y);
            next += 1000*3; break;
        case 5: ping(a+b+c+d+e, x, y);
            next += 100; break;

        }
    }

    void ping(String text, int x, int y) {
        text(text, x, y);
        sound.play();
    }
}
