package b3get;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

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
      double cost = Approximate.approximate(weight / distanceSquared, 10);
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

  /** item index x, for a list x. */
  static Object item(Object x, int index) throws ExtensionException {
    if (!(x instanceof LogoList)) throw new ExtensionException("expected a list but got " + x);
    LogoList list = (LogoList) x;
    if (index < 0 || index >= list.size())
      throw new ExtensionException("can't find element " + index + " of a list of length " + list.size());
    return list.get(index);
  }
}
