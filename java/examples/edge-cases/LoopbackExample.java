import nz.sodium.*;

/**
 * Created by John Aston on 10/11/2015.
 */
public class LoopbackExample {

    static private <T> void addStdOutListener(Cell<T> cell, String comment) {
        cell.listen(e -> {
            System.out.println("Cell listener: " + comment + " " + e.toString());
        });
    }
    static private <T> void addStdOutListener(Stream<T> stream, String comment) {
        stream.listen(e -> {
            System.out.println("Stream listener: " + comment + " " + e.toString());
        });
    }

    static public void loopbackNegativeFeedbackTest1() {
        CellSink<Double> currentLevel = new CellSink<>(0.0);
        CellSink<Double> anotherCell = new CellSink<>(1.2345);
        double thresholdDifference = 0.01;
        Transaction.runVoid(
                () -> {
                    CellLoop<Double> feedbackLevel = new CellLoop<>();
                    Cell<Double> levelDifference = Cell.lift((a, b) -> a - b, currentLevel, feedbackLevel);
                    // Stabilisation updates to be fed to some external system, so are required to be an Event Stream.
                    Stream<Double> stabilisationUpdates = Operational.updates(levelDifference).filter((d) -> Math.abs(d) > thresholdDifference);
                    feedbackLevel.loop(stabilisationUpdates.coalesce((d, s) -> (d + s))
                                                           .accum(0.0, (d, s) -> (d + s)));

                    addStdOutListener(stabilisationUpdates, "stabilisationUpdates");
                    addStdOutListener(levelDifference, "levelDifference");
                    addStdOutListener(feedbackLevel, "feedbackLevel");

                    Cell<Double> levelDifference_map = levelDifference.map(a -> 2.0 * a);
                    addStdOutListener(levelDifference_map, "levelDifference_map");

                    Cell<Double> levelDifference_lift = Cell.lift((a, b) -> a + b, anotherCell, levelDifference);
                    addStdOutListener(levelDifference_lift, "levelDifference_lift");
                }
        );

        System.out.println("About to send 20.0");
        currentLevel.send(20.0);
        System.out.println("About to send 15.0");
        currentLevel.send(15.0);
    }

    public static void main(String[] args) {
        System.out.println("Running loopbackNegativeFeedbackTest1");
        loopbackNegativeFeedbackTest1();
    }
}
