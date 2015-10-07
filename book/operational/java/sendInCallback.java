import nz.sodium.*;

public class sendInCallback {
    public static void main(String[] args) {
        StreamSink<Integer> sX = new StreamSink<>();
        StreamSink<Integer> sY = new StreamSink<>();
        // Should throw an exception because you're not allowed to use send() inside
        // a callback.
        Listener l = sX.listen(x -> sY.send(x)).append(
                     sY.listen(y -> { System.out.println(y); }));
        sX.send(1);
        sX.send(2);
        sX.send(3);
        l.unlisten();
    }
}
