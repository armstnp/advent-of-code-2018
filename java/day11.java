import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.IntStream;

class Main {
  private static final int SERIAL = 8199;

  public static void main(String[] args) {
    Map[] squarePowers = new Map[301];
    squarePowers[1] = new HashMap<Point, Integer>();
    IntStream
      .range(1, 301)
      .forEach(x ->
        IntStream
          .range(1, 301)
          .forEach(y ->
            squarePowers[1].put(
              Point.of(x, y),
              powerLevel(x, y))));

    int maxDim = -1;
    int maxPower = -1;
    Point maxPoint = Point.of(-1, -1);

    for(int dim = 2; dim <= 300; dim++) {
      System.out.println("Processing dim " + dim);

      squarePowers[dim] = new HashMap<>();
      for(int x = 1; x <= 301 - dim; x++) {
        for(int y = 1; y <= 301 - dim; y++) {
          squarePowers[dim].put(Point.of(x, y), combinePowerLevels(x, y, dim, squarePowers));
        }
      }

      Map.Entry<Point, Integer> maxSquare =
        ((Map<Point, Integer>)(squarePowers[dim])).entrySet().stream()
          .max((entryA, entryB) -> entryA.getValue().compareTo(entryB.getValue()))
          .orElseThrow(() -> new RuntimeException("No max??"));

      if(maxSquare.getValue() > maxPower) {
        maxDim = dim;
        maxPower = maxSquare.getValue();
        maxPoint = maxSquare.getKey();
      }
      if(dim % 2 == 1 && dim / 2 > 1) squarePowers[dim / 2] = null;
    }

    System.out.println(maxPoint + "," + maxDim + ": " + maxPower);
  }

  private static int powerLevel(int x, int y) {
    int rackId = x + 10;
    return ((((rackId * y + SERIAL) * rackId) % 1000) / 100) - 5;
  }

  private static int combinePowerLevels(int x, int y, int dim, Map<Point, Integer>[] squarePowers) {
    int half = dim / 2;
    Map<Point, Integer> halfSquares = squarePowers[half];
    if(dim % 2 == 0) {
      return halfSquares.get(Point.of(x, y))
        + halfSquares.get(Point.of(x + half, y))
        + halfSquares.get(Point.of(x, y + half))
        + halfSquares.get(Point.of(x + half, y + half));
    }

    int greaterHalf = half + 1;
    Map<Point, Integer> greaterHalfSquares = squarePowers[greaterHalf];
    Map<Point, Integer> unitSquares = squarePowers[1];
    return greaterHalfSquares.get(Point.of(x, y))
        + halfSquares.get(Point.of(x + greaterHalf, y))
        + halfSquares.get(Point.of(x, y + greaterHalf))
        + greaterHalfSquares.get(Point.of(x + half, y + half))
        - unitSquares.get(Point.of(x + half, y + half));
  }

  private static final class Point {
    final int x;
    final int y;

    static Point of(int x, int y) {
      return new Point(x, y);
    }

    Point(int x, int y) {
      this.x = x;
      this.y = y;
    }

    @Override
    public boolean equals(Object o) {
      if(o == null) return false;
      if(!(o instanceof Point)) return false;
      Point other = (Point) o;
      return x == other.x && y == other.y;
    }

    @Override
    public int hashCode() {
      return x * x + y + 37;
    }

    @Override
    public String toString() {
      return "{" + x + "," + y + "}";
    }
  }
}
