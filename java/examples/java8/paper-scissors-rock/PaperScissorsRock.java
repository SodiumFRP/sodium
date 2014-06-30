import sodium.*;
import java.util.Optional;
import java.util.Random;
import java.util.Scanner;

enum Play {
    PAPER, ROCK, SCISSORS;
    public final boolean beats(Play other) {
        return (ordinal() + 1) % 3 == other.ordinal();
    }
};

enum Winner { COMPUTER, HUMAN, DRAW };

class Result {
    Result(Play computerPlay, Play humanPlay, Winner winner) {
        this.computerPlay = computerPlay;
        this.humanPlay = humanPlay;
        this.winner = winner;
    }
    final Play computerPlay;
    final Play humanPlay;
    final Winner winner;

    @Override
    public String toString() {
        return "human: "+humanPlay+" computer: "+computerPlay+
                                   " winner: "+winner;
    }
};

public class PaperScissorsRock {

    public static Behavior<Optional<Result>> gameLogic(
            Behavior<Optional<Play>> computerPlay,
            Behavior<Optional<Play>> humanPlay) {
        return Behavior.lift(
            (oComputer, oHuman) -> {
                if (oComputer.isPresent() && oHuman.isPresent()) {
                    Play computer = oComputer.get();
                    Play human    = oHuman.get();
                    return Optional.of(
                        new Result(
                            computer,
                            human,
                            computer.beats(human) ? Winner.COMPUTER :
                            human.beats(computer) ? Winner.HUMAN :
                                                    Winner.DRAW
                        )
                    );
                }
                else
                    return Optional.empty();
            },
            computerPlay,
            humanPlay);
    }

    public static void main(String[] args) throws InterruptedException {
        System.out.println("Paper, Scissors, Rock");
        System.out.println("On 'GO!' please type p, s or r, then <ENTER>");

        BehaviorSink<Optional<Play>> computerPlay =
                                new BehaviorSink<>(Optional.empty());
        BehaviorSink<Optional<Play>> humanPlay =
                                new BehaviorSink<>(Optional.empty());

        Behavior<Optional<Result>> result =
                                gameLogic(computerPlay, humanPlay);

        Listener l = result.value().listen((oResult) -> {
            if (oResult.isPresent())
                System.out.println("Result - "+oResult.get());
        });

        new Thread(() -> {
            Random rng = new Random();
            try {
                while (true) {
                    for (int i = 3; i >= 1; i--) {
                        System.out.println(i+"...");
                        Thread.sleep(2000);
                    }
                    System.out.println("GO!");
                    Play myPlay = Play.values()[rng.nextInt(3)];
                    computerPlay.send(Optional.of(myPlay));
                    Thread.sleep(5000);
                    computerPlay.send(Optional.empty());
                    Thread.sleep(5000);
                }
             } catch (InterruptedException e) {}
        }).start();

        Scanner s = new Scanner(System.in);
        while (s.hasNext()) {
            humanPlay.send(parse(s.nextLine()));
            Thread.sleep(5000);
            humanPlay.send(Optional.empty());
        }
    }

    private static Optional<Play> parse(String l) {
        if (l.length() > 0) {
            char c = l.toLowerCase().charAt(0);
            if (c == 'p') return Optional.of(Play.PAPER); else
            if (c == 's') return Optional.of(Play.SCISSORS); else
            if (c == 'r') return Optional.of(Play.ROCK);
        }
        return Optional.empty();
    }
}

