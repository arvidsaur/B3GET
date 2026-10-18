package b3get;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.nlogo.api.Agent;
import org.nlogo.api.AnonymousReporter;
import org.nlogo.api.Approximate;
import org.nlogo.api.Argument;
import org.nlogo.api.Context;
import org.nlogo.api.DefaultClassManager;
import org.nlogo.api.Equality;
import org.nlogo.api.ExtensionException;
import org.nlogo.api.LogoListBuilder;
import org.nlogo.api.PrimitiveManager;
import org.nlogo.api.Reporter;
import org.nlogo.api.Turtle;
import org.nlogo.core.ExtensionObject;
import org.nlogo.core.LogoList;
import org.nlogo.core.Syntax;
import org.nlogo.core.SyntaxJ;

/**
 * Primitives for B3GET's busiest loops. Each one reports exactly what the NetLogo
 * code it replaces reported, using NetLogo's own equality and rounding; the model
 * logic (statuses, action names, costs) stays in NetLogo.
 */
public class B3getExtension extends DefaultClassManager {

  @Override
  public void load(PrimitiveManager primitiveManager) {
    primitiveManager.addPrimitive("sta2us-decisions", new Sta2usDecisions());
    primitiveManager.addPrimitive("position", new Position());
    primitiveManager.addPrimitive("with-item", new WithItem());
    primitiveManager.addPrimitive("sum-item", new SumItem());
    primitiveManager.addPrimitive("matching-alleles", new MatchingAlleles());
    primitiveManager.addPrimitive("precision", new Precision());
    primitiveManager.addPrimitive("lput-completed", new LputCompleted());
    primitiveManager.addPrimitive("completed-cost", new CompletedCost());
  }

  /** Each agent's index of its completed actions, for b3get:completed-cost. */
  private final Map<Agent, CompletedIndex> completedIndexes = new IdentityHashMap<>();

  @Override
  public void clearAll() {
    completedIndexes.clear();
  }

  /**
   * For one list of completed actions, (list self target action cost) each: the costs
   * by target and action name, added in list order as b3get:sum-item adds them. It
   * describes LIST and no other list; NetLogo lists never change.
   */
  private static final class CompletedIndex {
    LogoList list;
    boolean usable = true;  // false when an item is not as complete-action makes them
    final Map<Agent, Map<String, double[]>> costs = new IdentityHashMap<>();

    CompletedIndex(LogoList list) {
      this.list = list;
      for (Object action : list.toJava()) if (!add(action)) { usable = false; break; }
    }

    /**
     * Adds ACTION's cost under its target and name, or reports false when ACTION is
     * not a list of at least 4 items, or its target or name is an extension object.
     * A target that is not a turtle never equals a living turtle, and a name that is
     * not a string never equals a string, so those actions are left out.
     */
    boolean add(Object action) {
      if (!(action instanceof LogoList) || ((LogoList) action).size() < 4) return false;
      LogoList a = (LogoList) action;
      Object target = a.get(1), name = a.get(2), cost = a.get(3);
      if (target instanceof ExtensionObject || name instanceof ExtensionObject) return false;
      if (!(target instanceof Turtle && name instanceof String)) return true;
      double[] sum = costs.computeIfAbsent((Agent) target, t -> new HashMap<>())
                          .computeIfAbsent((String) name, n -> new double[1]);
      if (cost instanceof Double) sum[0] += (Double) cost;
      return true;
    }

    double cost(Turtle target, String name) {
      Map<String, double[]> byName = costs.get(target);
      double[] sum = byName == null ? null : byName.get(name);
      return sum == null ? 0d : sum[0];
    }
  }

  /**
   * b3get:lput-completed action list
   *
   * Same as lput action list. LIST is the calling agent's list of completed actions:
   * when the agent's index (see b3get:completed-cost) describes LIST, it is brought
   * up to date to describe the new list instead.
   */
  class LputCompleted implements Reporter {
    @Override
    public Syntax getSyntax() {
      return SyntaxJ.reporterSyntax(new int[] { Syntax.WildcardType(), Syntax.ListType() }, Syntax.ListType());
    }

    @Override
    public Object report(Argument[] args, Context context) throws ExtensionException {
      Object action = args[0].get();
      LogoList list = args[1].getList();
      LogoList result = list.lput(action);
      CompletedIndex index = completedIndexes.get(context.getAgent());
      if (index != null && index.list == list) {
        if (index.usable && !index.add(action)) index.usable = false;
        index.list = result;
      }
      return result;
    }
  }

  /**
   * b3get:completed-cost list target name
   *
   * Same as b3get:sum-item b3get:with-item b3get:with-item list 1 target 2 name 3:
   * the costs of the completed actions in LIST aimed at TARGET and named NAME, added
   * in list order. LIST is the calling agent's list of completed actions. Instead of
   * scanning it, this looks the sum up in the agent's index of LIST, which it builds
   * when the agent has none for that very list; b3get:lput-completed keeps the index
   * up to date as the agent adds actions. When TARGET is not a living turtle or NAME
   * not a string, or LIST holds other items, it scans LIST as the NetLogo code would.
   */
  class CompletedCost implements Reporter {
    @Override
    public Syntax getSyntax() {
      return SyntaxJ.reporterSyntax(
        new int[] { Syntax.ListType(), Syntax.WildcardType(), Syntax.WildcardType() }, Syntax.NumberType());
    }

    @Override
    public Object report(Argument[] args, Context context) throws ExtensionException {
      LogoList list = args[0].getList();
      Object target = args[1].get(), name = args[2].get();
      double sum;
      // A living turtle equals only itself (Equality.equals compares turtles by id).
      if (target instanceof Turtle && ((Turtle) target).id() >= 0 && name instanceof String) {
        CompletedIndex index = completedIndexes.get(context.getAgent());
        if (index == null || index.list != list) {
          index = new CompletedIndex(list);
          completedIndexes.put(context.getAgent(), index);
        }
        sum = index.usable ? index.cost((Turtle) target, (String) name) : scan(list, target, name);
      } else {
        sum = scan(list, target, name);
      }
      if (Double.isInfinite(sum) || Double.isNaN(sum))
        throw new ExtensionException("math operation produced a number too large for NetLogo");
      return sum;
    }

    /** The NetLogo code's way: the two filters, then the sum. */
    private double scan(LogoList list, Object target, Object name) throws ExtensionException {
      List<Object> aimed = new ArrayList<>();
      for (Object action : list.toJava()) if (Equality.equals(item(action, 1), target)) aimed.add(action);
      List<Object> named = new ArrayList<>();
      for (Object action : aimed) if (Equality.equals(item(action, 2), name)) named.add(action);
      double sum = 0d;
      for (Object action : named) {
        Object x = item(action, 3);
        if (x instanceof Double) sum += (Double) x;
      }
      return sum;
    }
  }

  /**
   * b3get:sta2us-decisions ego-status alleles objects identities statuses distances-squared action-of
   *
   * The decision vectors of sta2us-get-decisions: for each valid allele whose first
   * codon matches EGO-STATUS, in order, and each object whose status matches its
   * second codon, in order,
   *   (list self object (action-of item 3 allele) precision (item 4 allele / distance-squared) 10 false)
   * An object's status is the last of STATUSES given for its identity, as when they
   * are put in a table by identity in order and looked up again. ACTION-OF must be
   * pure, since it is run once per distinct action codon.
   */
  static class Sta2usDecisions implements Reporter {
    @Override
    public Syntax getSyntax() {
      return SyntaxJ.reporterSyntax(
        new int[] { Syntax.WildcardType(), Syntax.ListType(), Syntax.ListType(), Syntax.ListType(),
                    Syntax.ListType(), Syntax.ListType(), Syntax.ReporterType() },
        Syntax.ListType(), "-T--");
    }

    @Override
    public Object report(Argument[] args, Context context) throws ExtensionException {
      Object egoStatus = args[0].get();
      LogoList alleles = args[1].getList();
      LogoList objects = args[2].getList();
      LogoList identities = args[3].getList();
      LogoList statuses = args[4].getList();
      LogoList distancesSquared = args[5].getList();
      AnonymousReporter actionOf = args[6].getReporter();
      Object self = context.getAgent();

      int n = objects.size();
      if (identities.size() != n || statuses.size() != n || distancesSquared.size() != n)
        throw new ExtensionException("objects, identities, statuses and distances must be lists of the same length");
      Map<Object, Object> statusOfIdentity = new HashMap<>();
      for (int i = 0; i < n; i++) statusOfIdentity.put(identities.get(i), statuses.get(i));
      Object[] object = new Object[n];
      Object[] status = new Object[n];
      double[] distanceSquared = new double[n];
      for (int i = 0; i < n; i++) {
        object[i] = objects.get(i);
        status[i] = statusOfIdentity.get(identities.get(i));
        distanceSquared[i] = number(distancesSquared.get(i));
      }

      // An allele's vectors depend only on its last three codons, so repeated alleles
      // (both copies of a homozygous locus) reuse them, as the NetLogo code did.
      Map<AlleleKey, List<LogoList>> vectorsOf = new HashMap<>();
      Map<String, Object> actionNames = new HashMap<>();
      LogoListBuilder decisions = new LogoListBuilder();

      for (Object g : alleles.toJava()) {
        if (!isValidAllele(g)) continue;
        LogoList allele = (LogoList) g;
        if (!firstStringIsPartOfLast(allele.get(1), egoStatus)) continue;
        String geneOther = (String) allele.get(2);
        String geneAction = (String) allele.get(3);
        double geneWeight = (Double) allele.get(4);

        AlleleKey key = new AlleleKey(geneOther, geneAction, Double.doubleToRawLongBits(geneWeight));
        List<LogoList> vectors = vectorsOf.get(key);
        if (vectors == null) {
          Object actionName = actionNames.get(geneAction);
          if (actionName == null) {
            actionName = actionOf.report(context, new Object[] { geneAction });
            actionNames.put(geneAction, actionName);
          }
          vectors = new ArrayList<>();
          for (int i = 0; i < n; i++) {
            if (firstStringIsPartOfLast(geneOther, status[i])) {
              List<Object> vector = new ArrayList<>(5);
              vector.add(self);
              vector.add(object[i]);
              vector.add(actionName);
              vector.add(precision10(geneWeight, distanceSquared[i]));
              vector.add(Boolean.FALSE);
              vectors.add(LogoList.fromJava(vector));
            }
          }
          vectorsOf.put(key, vectors);
        }
        for (LogoList vector : vectors) decisions.add(vector);
      }
      return decisions.toLogoList();
    }

    /** precision ( weight / distance-squared ) 10, with the checks / and precision make. */
    private static Double precision10(double weight, double distanceSquared) throws ExtensionException {
      if (distanceSquared == 0) throw new ExtensionException("Division by zero.");
      double cost = precision(weight / distanceSquared, 10);
      if (Double.isInfinite(cost) || Double.isNaN(cost))
        throw new ExtensionException("math operation produced a number too large for NetLogo");
      return cost;
    }

    /** length g = 5 and is-string? item 1 g and is-string? item 2 g and is-string? item 3 g and is-number? item 4 g */
    private static boolean isValidAllele(Object g) throws ExtensionException {
      if (g instanceof LogoList) {
        LogoList list = (LogoList) g;
        return list.size() == 5 && list.get(1) instanceof String && list.get(2) instanceof String
          && list.get(3) instanceof String && list.get(4) instanceof Double;
      }
      if (g instanceof String) return false;  // item 4 of a string is a string, not a number
      throw new ExtensionException("LENGTH expected input to be a string or list but got " + g + " instead.");
    }

    private static double number(Object x) throws ExtensionException {
      if (x instanceof Double) return (Double) x;
      throw new ExtensionException("expected a number but got " + x);
    }
  }

  private static final double[] POWERS_OF_TEN = {
    1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15 };

  /**
   * NetLogo's precision: Approximate.approximate(n, places), without its slow part
   * in almost all cases.
   *
   * approximate rounds the decimal digits of Double.toString(n) to PLACES places
   * (half up, or half down below zero) and converts the result back. Those digits
   * are some decimal number within half a unit in the last place of n, so when all
   * numbers that close to n round to the same K / 10^places, that is the result,
   * whatever the digits are. Converting K / 10^places back to a double, as
   * BigDecimal.doubleValue does, gives the double nearest to it, which is what
   * dividing K by 10^places gives (both are exact doubles, and division rounds to
   * the nearest). Otherwise, near a tie, this calls approximate.
   * PrecisionTest (code/b3get/test) checks this against approximate itself.
   */
  static double precision(double n, int places) {
    if (!(Math.abs(n) < 0x1p40) || places < 0 || places >= POWERS_OF_TEN.length)
      return Approximate.approximate(n, places);
    double scale = POWERS_OF_TEN[places];
    double magnitude = Math.abs(n);
    double y = magnitude * scale;  // within half an ulp of magnitude * 10^places
    if (!(y < 0x1p50)) return Approximate.approximate(n, places);
    double whole = Math.floor(y);
    double fraction = y - whole;  // exact
    // The digits are within half an ulp of n, so after scaling within
    // ulp(n) * scale / 2 of y, plus y's own rounding error: allow twice both.
    double margin = Math.ulp(magnitude) * scale + 2 * Math.ulp(y);
    if (Math.abs(fraction - 0.5) <= margin) return Approximate.approximate(n, places);
    long k = (long) whole + (fraction > 0.5 ? 1 : 0);
    double result = k / scale;
    // BigDecimal has no negative zero.
    return n < 0 && k != 0 ? -result : result;
  }

  /** first-string-is-part-of-last? in sta2us.nls. */
  static boolean firstStringIsPartOfLast(Object string1, Object string2) {
    if (string1 instanceof String && string2 instanceof String) {
      String s1 = (String) string1, s2 = (String) string2;
      for (int i = 0; i < s1.length(); i++) {
        char c = s1.charAt(i);
        if (s2.indexOf(c) < 0 && c != ' ') return false;
      }
    }
    return true;
  }

  private static final class AlleleKey {
    final String other, action;
    final long weightBits;

    AlleleKey(String other, String action, long weightBits) {
      this.other = other;
      this.action = action;
      this.weightBits = weightBits;
    }

    @Override
    public boolean equals(Object o) {
      if (!(o instanceof AlleleKey)) return false;
      AlleleKey k = (AlleleKey) o;
      return weightBits == k.weightBits && other.equals(k.other) && action.equals(k.action);
    }

    @Override
    public int hashCode() {
      return Objects.hash(other, action, weightBits);
    }
  }

  /**
   * b3get:position item list
   *
   * Same as position on a list. Before the full comparison, it rules out list items
   * with an element that differs from the same element of ITEM as two numbers or
   * two strings, which Equality.equals would find unequal anyway.
   */
  static class Position implements Reporter {
    @Override
    public Syntax getSyntax() {
      return SyntaxJ.reporterSyntax(
        new int[] { Syntax.WildcardType(), Syntax.ListType() },
        Syntax.NumberType() | Syntax.BooleanType());
    }

    @Override
    public Object report(Argument[] args, Context context) throws ExtensionException {
      Object item = args[0].get();
      LogoList list = args[1].getList();
      int i = 0;
      for (Object element : list.toJava()) {
        if (mayBeEqual(item, element) && Equality.equals(item, element)) return Double.valueOf(i);
        i++;
      }
      return Boolean.FALSE;
    }

    private static boolean mayBeEqual(Object a, Object b) {
      if (!(a instanceof LogoList && b instanceof LogoList)) return true;
      LogoList l1 = (LogoList) a, l2 = (LogoList) b;
      int n = l1.size();
      if (l2.size() != n) return false;
      // From the end: in a decision vector the cost (item 3) differs most often.
      for (int k = n - 1; k >= 0; k--) {
        Object x = l1.get(k), y = l2.get(k);
        if (x instanceof Double && y instanceof Double) {
          if (((Double) x).doubleValue() != ((Double) y).doubleValue()) return false;
        } else if ((x instanceof String && y instanceof String) || (x instanceof Boolean && y instanceof Boolean)) {
          if (!x.equals(y)) return false;
        }
      }
      return true;
    }
  }

  /**
   * b3get:with-item list index value
   *
   * Same as filter [ x -> item index x = value ] list, for a list of lists.
   */
  static class WithItem implements Reporter {
    @Override
    public Syntax getSyntax() {
      return SyntaxJ.reporterSyntax(
        new int[] { Syntax.ListType(), Syntax.NumberType(), Syntax.WildcardType() },
        Syntax.ListType());
    }

    @Override
    public Object report(Argument[] args, Context context) throws ExtensionException {
      LogoList list = args[0].getList();
      int index = args[1].getIntValue();
      Object value = args[2].get();
      LogoListBuilder selected = new LogoListBuilder();
      for (Object element : list.toJava()) {
        if (Equality.equals(item(element, index), value)) selected.add(element);
      }
      return selected.toLogoList();
    }
  }

  /**
   * b3get:sum-item list index
   *
   * Same as sum map [ x -> item index x ] list, for a list of lists: the numbers
   * added in order, other values ignored.
   */
  static class SumItem implements Reporter {
    @Override
    public Syntax getSyntax() {
      return SyntaxJ.reporterSyntax(
        new int[] { Syntax.ListType(), Syntax.NumberType() },
        Syntax.NumberType());
    }

    @Override
    public Object report(Argument[] args, Context context) throws ExtensionException {
      LogoList list = args[0].getList();
      int index = args[1].getIntValue();
      double sum = 0d;
      for (Object element : list.toJava()) {
        Object x = item(element, index);
        if (x instanceof Double) sum += (Double) x;
      }
      if (Double.isInfinite(sum) || Double.isNaN(sum))
        throw new ExtensionException("math operation produced a number too large for NetLogo");
      return sum;
    }
  }

  /**
   * b3get:matching-alleles mine-I mine-II theirs-I theirs-II loci
   *
   * How many of the alleles of MINE-I and MINE-II at the first LOCI loci equal the
   * allele of THEIRS-I or THEIRS-II at the same locus: the number of times
   *   item i mine = item i theirs-I or item i mine = item i theirs-II
   * holds for i from 0 to LOCI - 1 and mine being MINE-I, then MINE-II. The
   * chromosomes are strings or lists.
   */
  static class MatchingAlleles implements Reporter {
    @Override
    public Syntax getSyntax() {
      return SyntaxJ.reporterSyntax(
        new int[] { Syntax.StringType() | Syntax.ListType(), Syntax.StringType() | Syntax.ListType(),
                    Syntax.StringType() | Syntax.ListType(), Syntax.StringType() | Syntax.ListType(),
                    Syntax.NumberType() },
        Syntax.NumberType());
    }

    @Override
    public Object report(Argument[] args, Context context) throws ExtensionException {
      Object mineI = args[0].get(), mineII = args[1].get(), theirsI = args[2].get(), theirsII = args[3].get();
      int loci = args[4].getIntValue();
      int matching = 0;
      if (mineI instanceof String && mineII instanceof String && theirsI instanceof String && theirsII instanceof String
          && loci >= 0 && loci <= Math.min(Math.min(((String) mineI).length(), ((String) mineII).length()),
                                           Math.min(((String) theirsI).length(), ((String) theirsII).length()))) {
        // Alleles are one-character strings, equal when their characters are.
        String mI = (String) mineI, mII = (String) mineII, tI = (String) theirsI, tII = (String) theirsII;
        for (int i = 0; i < loci; i++) {
          char a = mI.charAt(i), b = mII.charAt(i), x = tI.charAt(i), y = tII.charAt(i);
          if (a == x || a == y) matching++;
          if (b == x || b == y) matching++;
        }
        return Double.valueOf(matching);
      }
      for (int i = 0; i < loci; i++) {
        Object theirI = allele(theirsI, i), theirII = allele(theirsII, i);
        Object a = allele(mineI, i);
        if (Equality.equals(a, theirI) || Equality.equals(a, theirII)) matching++;
        Object b = allele(mineII, i);
        if (Equality.equals(b, theirI) || Equality.equals(b, theirII)) matching++;
      }
      return Double.valueOf(matching);
    }

    /** item i of a string or list. */
    private static Object allele(Object chromosome, int i) throws ExtensionException {
      if (chromosome instanceof String) {
        String s = (String) chromosome;
        if (i < 0 || i >= s.length())
          throw new ExtensionException("can't find element " + i + " of a string of length " + s.length());
        return s.substring(i, i + 1);
      }
      return item(chromosome, i);
    }
  }

  /** b3get:precision number places: the same as precision number places, faster (see precision). */
  static class Precision implements Reporter {
    @Override
    public Syntax getSyntax() {
      return SyntaxJ.reporterSyntax(new int[] { Syntax.NumberType(), Syntax.NumberType() }, Syntax.NumberType());
    }

    @Override
    public Object report(Argument[] args, Context context) throws ExtensionException {
      double result = precision(args[0].getDoubleValue(), args[1].getIntValue());
      if (Double.isInfinite(result) || Double.isNaN(result))
        throw new ExtensionException("math operation produced a number too large for NetLogo");
      return result;
    }
  }

  /** item index x, for a list x. */
  static Object item(Object x, int index) throws ExtensionException {
    if (!(x instanceof LogoList)) throw new ExtensionException("expected a list but got " + x);
    LogoList list = (LogoList) x;
    if (index < 0 || index >= list.size())
      throw new ExtensionException("can't find element " + index + " of a list of length " + list.size());
    return list.get(index);
  }
}
