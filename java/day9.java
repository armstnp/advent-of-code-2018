import java.util.Arrays;
import java.util.Deque;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.ArrayDeque;

class Main {
  private static final int NUM_PLAYERS = 448;
  private static final int NUM_MARBLES = 7162800;
  private static final int SCORING_MARBLE = 23;

  public static void main(String[] args) {
    long[] scores = new long[NUM_PLAYERS];

    Deque<Integer> circle = new ArrayDeque<>();
    circle.push(0);

    IntStream
      .rangeClosed(1, NUM_MARBLES)
      .forEach(marble -> {
        if(isScoringMarble(marble)) scoreMarbles(circle, scores, marble);
        else placeMarble(circle, marble);
      });

    long maxScore = Arrays.stream(scores).max().getAsLong();

    System.out.println(maxScore);
  }

  private static <T> void goCW(Deque<T> deque) {
    deque.addLast(deque.removeFirst());
  }

  private static <T> void goCCW(Deque<T> deque) {
    deque.addFirst(deque.removeLast());
  }

  private static boolean isScoringMarble(int x) {
    return x % SCORING_MARBLE == 0;
  }

  private static void scoreMarbles(Deque<Integer> circle, long[] scores, int marble) {
    int player = marble % NUM_PLAYERS;
    for(int i = 0; i < 7; i++) { goCCW(circle); }
    int removedMarble = circle.removeFirst();
    scores[player] += marble + removedMarble;
  }

  private static void placeMarble(Deque<Integer> circle, int marble) {
    goCW(circle);
    goCW(circle);
    circle.addFirst(marble);
  }
}
