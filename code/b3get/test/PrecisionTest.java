import java.util.SplittableRandom;

import org.nlogo.api.Approximate;

/**
 * Checks b3get's precision against NetLogo's Approximate.approximate, bit for bit,
 * on random numbers like B3GET's costs and on numbers next to rounding ties. Run it
 * with the Java that runs NetLogo, since approximate depends on its Double.toString:
 *   java -cp "code/b3get/b3get.jar:<NetLogo>/lib/app/*:code/b3get/test-classes" PrecisionTest [cases]
 */
public class PrecisionTest {
  static long checked = 0;

  public static void main(String[] args) throws Exception {
    long cases = args.length > 0 ? Long.parseLong(args[0]) : 20_000_000L;
    java.lang.reflect.Method precision = Class.forName("b3get.B3getExtension")
      .getDeclaredMethod("precision", double.class, int.class);
    precision.setAccessible(true);
    SplittableRandom rng = new SplittableRandom(1);
    double[] weights = { 1E-4, 9E-5, 1E-3, 2E-3, 3E-3, 5.1E-3, 1E-2, 0.1, 1, 0 };
    for (long c = 0; c < cases; c++) {
      double n;
      int places = 10;
      switch ((int) (c % 6)) {
        case 0: {  // a weight over a squared distance, as in sta2us-decisions
          double w = weights[rng.nextInt(weights.length)] * (rng.nextBoolean() ? 1 : -1);
          double d = 1 + rng.nextDouble() * 20;
          n = w / (rng.nextInt(4) == 0 ? 1 : d * d);
          break;
        }
        case 1:  // any magnitude
          n = Math.pow(10, -14 + rng.nextDouble() * 20) * (rng.nextBoolean() ? 1 : -1);
          break;
        case 2: {  // next to a tie: (k + 1/2) / 10^places, a few ulps either way
          long k = rng.nextLong(1L << 40);
          places = rng.nextInt(16);
          n = (k + 0.5) / Math.pow(10, places);
          for (int u = rng.nextInt(9) - 4; u != 0; u += u > 0 ? -1 : 1) n = u > 0 ? Math.nextUp(n) : Math.nextDown(n);
          if (rng.nextBoolean()) n = -n;
          break;
        }
        case 3: {  // next to a result: k / 10^places, a few ulps either way
          long k = rng.nextLong(1L << 40);
          places = rng.nextInt(16);
          n = k / Math.pow(10, places);
          for (int u = rng.nextInt(9) - 4; u != 0; u += u > 0 ? -1 : 1) n = u > 0 ? Math.nextUp(n) : Math.nextDown(n);
          if (rng.nextBoolean()) n = -n;
          break;
        }
        case 4:  // already rounded, as precision cost 10 in complete-action sees
          n = Approximate.approximate(Math.pow(10, -8 + rng.nextDouble() * 8) * (rng.nextBoolean() ? 1 : -1), 10);
          break;
        default:  // random bits, any places
          n = Double.longBitsToDouble(rng.nextLong());
          places = rng.nextInt(17);
      }
      check(precision, n, places);
    }
    for (double n : new double[] { 0.0, -0.0, Double.MIN_VALUE, -Double.MIN_VALUE, 5e-11, -5e-11, 4.9999999999e-11,
                                   0.5, 1.5, 2.5, -0.5, 1e15, Double.MAX_VALUE, Double.NaN, Double.POSITIVE_INFINITY })
      for (int places = 0; places <= 16; places++) check(precision, n, places);
    System.out.printf("PrecisionTest: %d cases identical to Approximate.approximate%n", checked);
  }

  static void check(java.lang.reflect.Method precision, double n, int places) throws Exception {
    double expected = Approximate.approximate(n, places);
    double actual = (Double) precision.invoke(null, n, places);
    if (Double.doubleToRawLongBits(expected) != Double.doubleToRawLongBits(actual) && !(Double.isNaN(expected) && Double.isNaN(actual))) {
      System.out.printf("MISMATCH: precision %s %d: approximate %s, b3get %s%n", Double.toString(n), places, expected, actual);
      System.exit(1);
    }
    checked++;
  }
}
