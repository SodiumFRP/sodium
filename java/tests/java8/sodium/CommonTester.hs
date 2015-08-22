package sodium;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import junit.framework.TestCase;

public class StreamTester extends TestCase {

  public void test_Base_send1() {
    StreamSink<String> s = Transaction.run(() -> {
      StreamSink<String> s_ = new StreamSink();
      return s_;
    });
    List<String> out = new ArrayList();
    Listener l = Transaction.run(() -> {
      Listener l_ = s.listen((String) -> {
        out.add("a");
      });
      return l_;
    });
    Transaction.runVoid(() -> {
      s.send("a");
    });
    Transaction.runVoid(() -> {
      s.send("b");
    });
    l.unlisten();
    assertEquals(Arrays.asList("a","b"),l);
  }
}

