// nlperf harness: drives a NetLogo model headless the way BehaviorSpace's
// lab.Worker does, recording world-export fingerprints and timings.
// Compiled against NetLogo 7.0.4 and invoked by perf/nlperf; see perf/README.md.

import java.io.IOException;
import java.io.Reader;
import java.lang.management.ManagementFactory;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import jdk.jfr.Configuration;
import jdk.jfr.Recording;
import jdk.jfr.consumer.RecordedEvent;
import jdk.jfr.consumer.RecordedFrame;
import jdk.jfr.consumer.RecordingFile;

import org.nlogo.api.Dump;
import org.nlogo.api.MersenneTwisterFast;
import org.nlogo.api.SimpleJobOwner;
import org.nlogo.core.AgentKindJ;
import org.nlogo.headless.HeadlessWorkspace;
import org.nlogo.nvm.Procedure;
import org.nlogo.workspace.Checksummer;

public final class NlPerf {

  static final int OK = 0, DIFFERENT = 1, UNUSABLE = 2, CRASHED = 3;

  public static void main(String[] args) {
    int code;
    try {
      if (args.length == 3 && args[0].equals("compare-traces")) {
        System.out.flush();
        System.exit(compareTraces(Paths.get(args[1]), Paths.get(args[2])));
      }
      if (args.length < 3) throw new UsageError("usage: NlPerf <record|verify|bench|profile|trace> <scenario> <golden-dir> ...");
      Scenario sc = Scenario.load(Paths.get(args[1]));
      Path golden = Paths.get(args[2]);
      code = switch (args[0]) {
        case "record" -> record(sc, golden);
        case "verify" -> verify(sc, golden, Paths.get(arg(args, 3)));
        case "bench" -> bench(sc, golden, Paths.get(arg(args, 3)), args.length > 4 ? args[4] : "");
        case "profile" -> profile(sc, golden, Paths.get(arg(args, 3)), Paths.get(arg(args, 4)));
        // NlPerf ab <scenario> <golden> <results> <reference-model> [label]
        case "ab" -> ab(sc, golden, Paths.get(arg(args, 3)), Paths.get(arg(args, 4)), args.length > 5 ? args[5] : "");
        // NlPerf trace <scenario> <out.tsv>
        case "trace" -> trace(sc, golden);
        // Re-analyze an existing recording: NlPerf procedures <scenario> <file.jfr>
        case "procedures" -> { printProcedureProfile(sc.model, Paths.get(args[2])); yield OK; }
        default -> throw new UsageError("unknown mode " + args[0]);
      };
    } catch (UsageError e) {
      System.err.println("nlperf: " + e.getMessage());
      code = UNUSABLE;
    } catch (Exception e) {
      if (e.getClass().getName().equals("org.nlogo.core.CompilerException"))
        System.err.println("nlperf: NetLogo compile error (model or scenario code): " + e.getMessage());
      else
        e.printStackTrace();
      code = CRASHED;
    }
    System.out.flush();
    // NetLogo leaves non-daemon threads behind, so exit explicitly.
    System.exit(code);
  }

  static String arg(String[] args, int i) {
    if (i >= args.length) throw new UsageError("missing argument " + i + " for mode " + args[0]);
    return args[i];
  }

  // ---------------------------------------------------------------- scenario

  static final class Scenario {
    static final Set<String> KEYS = Set.of(
      "model", "pre-setup", "setup", "go", "steps", "seeds", "checkpoint-every",
      "ignore-globals", "ignore-sections", "unordered-extensions",
      "bench.warmup", "bench.runs", "bench.seed", "profile.runs", "jvm");

    String name;
    Path file, model;
    String modelSha;
    String preSetup, setup, go;
    int steps, checkpointEvery;
    int[] seeds;
    List<String> reporters = new ArrayList<>();
    List<String> ignoreGlobals, ignoreSections, unorderedExtensions;
    int benchWarmup, benchRuns, benchSeed, profileRuns;

    static Scenario load(Path file) throws IOException {
      Properties p = new Properties();
      try (Reader r = Files.newBufferedReader(file)) { p.load(r); }
      for (String k : p.stringPropertyNames())
        if (!KEYS.contains(k) && !k.matches("reporter\\.\\d+"))
          throw new UsageError(file + ": unknown key '" + k + "'");

      Scenario sc = new Scenario();
      sc.file = file;
      sc.name = file.getFileName().toString().replaceFirst("\\.properties$", "");
      String model = p.getProperty("model", "").trim();
      if (model.isEmpty()) throw new UsageError(file + ": 'model' is required");
      sc.model = Paths.get(model).toAbsolutePath(); // relative paths are from the repo root
      if (!Files.isRegularFile(sc.model)) throw new UsageError("model not found: " + sc.model);
      sc.modelSha = sha256(Files.readAllBytes(sc.model));
      sc.preSetup = p.getProperty("pre-setup", "").trim();
      sc.setup = p.getProperty("setup", "setup").trim();
      sc.go = p.getProperty("go", "go").trim();
      sc.steps = positiveInt(p, "steps", null);
      sc.seeds = Arrays.stream(p.getProperty("seeds", "1 2 3").trim().split("[\\s,]+"))
        .mapToInt(Integer::parseInt).toArray();
      sc.checkpointEvery = positiveInt(p, "checkpoint-every", Math.max(1, sc.steps / 10));
      sc.ignoreGlobals = Arrays.stream(p.getProperty("ignore-globals", "").trim().split("[\\s,]+"))
        .filter(s -> !s.isEmpty()).map(String::toLowerCase).toList();
      sc.ignoreSections = Arrays.stream(p.getProperty("ignore-sections", "").trim().split("[\\s,]+"))
        .filter(s -> !s.isEmpty()).map(String::toUpperCase).toList();
      sc.unorderedExtensions = Arrays.stream(p.getProperty("unordered-extensions", "").trim().split("[\\s,]+"))
        .filter(s -> !s.isEmpty()).map(String::toLowerCase).toList();
      p.stringPropertyNames().stream()
        .filter(k -> k.startsWith("reporter."))
        .sorted(Comparator.comparingInt(k -> Integer.parseInt(k.substring("reporter.".length()))))
        .forEach(k -> sc.reporters.add(p.getProperty(k).trim()));
      sc.benchWarmup = nonNegativeInt(p, "bench.warmup", 3);
      sc.benchRuns = positiveInt(p, "bench.runs", 10);
      sc.benchSeed = Integer.parseInt(p.getProperty("bench.seed", String.valueOf(sc.seeds[0])).trim());
      sc.profileRuns = positiveInt(p, "profile.runs", 3);
      return sc;
    }

    /** Everything that determines the fingerprint, apart from the model file and the engine. */
    String configSha() {
      String config = String.join("\u0000", preSetup, setup, go, String.valueOf(steps),
        String.valueOf(checkpointEvery), Arrays.toString(seeds), String.join("\u0000", reporters));
      if (!ignoreGlobals.isEmpty()) config += "\u0000ignore-globals " + String.join(" ", ignoreGlobals);
      if (!ignoreSections.isEmpty()) config += "\u0000ignore-sections " + String.join(" ", ignoreSections);
      if (!unorderedExtensions.isEmpty()) config += "\u0000unordered-extensions " + String.join(" ", unorderedExtensions);
      return sha256(config.getBytes(StandardCharsets.UTF_8));
    }

    String columns() {
      List<String> cols = new ArrayList<>(List.of("seed", "step", "event", "world"));
      reporters.forEach(r -> cols.add(clean(r)));
      return String.join("\t", cols);
    }

    static int positiveInt(Properties p, String key, Integer dflt) {
      int v = nonNegativeInt(p, key, dflt);
      if (v < 1) throw new UsageError("'" + key + "' must be at least 1");
      return v;
    }

    static int nonNegativeInt(Properties p, String key, Integer dflt) {
      String v = p.getProperty(key);
      if (v == null) {
        if (dflt == null) throw new UsageError("'" + key + "' is required");
        return dflt;
      }
      int n = Integer.parseInt(v.trim());
      if (n < 0) throw new UsageError("'" + key + "' must not be negative");
      return n;
    }
  }

  // ---------------------------------------------------------------- driving a model

  /** A snapshot of the model: one fingerprint row plus the world export it hashes. */
  record Snapshot(Row row, String world) {}

  record Row(int seed, int step, String event, String world, List<String> values) {
    String tsv() {
      return seed + "\t" + step + "\t" + event + "\t" + world
        + values.stream().map(v -> "\t" + v).collect(Collectors.joining());
    }

    static Row parse(String line) {
      String[] f = line.split("\t", -1);
      return new Row(Integer.parseInt(f[0]), Integer.parseInt(f[1]), f[2], f[3],
        List.of(Arrays.copyOfRange(f, 4, f.length)));
    }

    String worldFile() { return "s" + seed + "-" + step + "-" + event + ".csv.gz"; }

    String describe(List<String> reporterNames) {
      StringBuilder sb = new StringBuilder("step " + step + " (" + event + "), world " + world);
      for (int i = 0; i < values.size(); i++)
        sb.append(", ").append(i < reporterNames.size() ? reporterNames.get(i) : "?").append(" = ").append(values.get(i));
      return sb.toString();
    }
  }

  static final class ModelError extends RuntimeException {
    ModelError(String message) { super(message); }
  }

  static final class UsageError extends RuntimeException {
    UsageError(String message) { super(message); }
  }

  /** One fresh model instance for one seed, run the way BehaviorSpace runs it. */
  static final class Run implements AutoCloseable {
    final int seed;
    final List<String> ignoreGlobals;
    final HeadlessWorkspace ws;
    final Procedure preSetup, setup, go;
    final Procedure[] reporters;
    final SimpleJobOwner owner;
    // Reporters run with their own RNG so recording them can't perturb the model.
    // It is reseeded before every snapshot: `of` visits agents in random order, so
    // a sum's last digits depend on that order.
    final MersenneTwisterFast probeRng = new MersenneTwisterFast();
    final SimpleJobOwner probeOwner;
    final List<String> ignoreSections, unorderedExtensions;
    int steps = 0;
    boolean stopped = false;
    long setupNanos, goNanos;

    Run(Scenario sc, int seed) throws IOException {
      this.seed = seed;
      ignoreGlobals = sc.ignoreGlobals;
      ws = HeadlessWorkspace.newInstance();
      ws.silent_$eq(true);
      ws.open(sc.model.toString(), false);
      preSetup = ws.compileCommands(sc.preSetup);
      setup = ws.compileCommands(sc.setup);
      // Same as lab.Worker: __experimentstepend turns a `stop` inside go into a stopped run.
      go = ws.compileCommands(sc.go + "\n__experimentstepend");
      reporters = sc.reporters.stream().map(ws::compileReporter).toArray(Procedure[]::new);
      ws.seedRNGs(seed);
      owner = new SimpleJobOwner("nlperf", ws.world().mainRNG(), AgentKindJ.Observer());
      probeOwner = new SimpleJobOwner("nlperf-probe", probeRng, AgentKindJ.Observer());
      ignoreSections = sc.ignoreSections;
      unorderedExtensions = sc.unorderedExtensions;
    }

    /** Returns true if the code called `stop`. */
    boolean exec(Procedure p) {
      boolean stopped = ws.runCompiledCommands(owner, p);
      throwIfFailed();
      return stopped;
    }

    void throwIfFailed() {
      var ex = ws.lastLogoException();
      if (ex != null) {
        ws.clearLastLogoException();
        throw new ModelError(ex.getMessage());
      }
    }

    Snapshot snapshot(String event) {
      // Some extensions export objects they hold weakly (the table extension exports
      // every table not yet collected), so collect garbage first.
      System.gc();
      String world = sortExtensionObjects(
        maskSections(maskGlobals(Checksummer.exportWorld(ws), ignoreGlobals), ignoreSections), unorderedExtensions);
      List<String> values = new ArrayList<>();
      probeRng.setSeed(0);
      for (Procedure r : reporters) {
        Object v = ws.runCompiledReporter(probeOwner, r);
        var ex = ws.lastLogoException();
        if (ex != null) {
          ws.clearLastLogoException();
          values.add(clean("ERROR: " + ex.getMessage()));
        } else {
          values.add(clean(Dump.logoObject(v, true, false)));
        }
      }
      return new Snapshot(new Row(seed, steps, event, sha256(world.getBytes(StandardCharsets.UTF_8)).substring(0, 16), values), world);
    }

    @Override public void close() throws Exception { ws.dispose(); }
  }

  interface Checkpoint { boolean at(Snapshot s) throws Exception; }

  /**
   * Runs setup, then go until `steps` steps or until go calls stop, like lab.Worker.
   * With every > 0, snapshots after setup and every `every` steps; the callback
   * returns false to abandon the run (and then this returns null). Returns the
   * final snapshot. Time spent snapshotting is excluded from goNanos.
   */
  static Snapshot drive(Scenario sc, Run run, int every, Checkpoint cp) throws Exception {
    return drive(sc, run, every, cp, true);
  }

  static Snapshot drive(Scenario sc, Run run, int every, Checkpoint cp, boolean snapshotAtEnd) throws Exception {
    try {
      long t0 = System.nanoTime();
      run.exec(run.preSetup);
      run.exec(run.setup);
      run.setupNanos = System.nanoTime() - t0;
      if (every > 0 && !cp.at(run.snapshot("setup"))) return null;
      long goStart = System.nanoTime(), paused = 0;
      while (run.steps < sc.steps) {
        if (run.exec(run.go)) { run.stopped = true; break; }
        run.steps++;
        if (every > 0 && run.steps % every == 0 && run.steps < sc.steps) {
          long p0 = System.nanoTime();
          if (!cp.at(run.snapshot("checkpoint"))) return null;
          paused += System.nanoTime() - p0;
        }
      }
      run.goNanos = System.nanoTime() - goStart - paused;
      return snapshotAtEnd ? run.snapshot(run.stopped ? "stop" : "limit") : null;
    } catch (ModelError e) {
      return new Snapshot(new Row(run.seed, run.steps, "error", "-", List.of(clean(e.getMessage()))), null);
    }
  }

  // ---------------------------------------------------------------- fingerprints

  static final class Fingerprint {
    final Map<String, String> meta = new LinkedHashMap<>();
    String columns;
    final Map<Integer, List<Row>> rows = new LinkedHashMap<>();

    static Fingerprint load(Path golden) throws IOException {
      Path file = golden.resolve("fingerprint.tsv");
      if (!Files.isRegularFile(file)) return null;
      Fingerprint fp = new Fingerprint();
      for (String line : Files.readAllLines(file)) {
        if (line.startsWith("# ")) {
          String[] kv = line.substring(2).split(": ", 2);
          if (kv.length == 2) fp.meta.put(kv[0], kv[1]);
        } else if (fp.columns == null) {
          fp.columns = line;
        } else if (!line.isBlank()) {
          Row r = Row.parse(line);
          fp.rows.computeIfAbsent(r.seed(), s -> new ArrayList<>()).add(r);
        }
      }
      return fp;
    }

    Row finalRow(int seed) {
      List<Row> r = rows.get(seed);
      return r == null ? null : r.get(r.size() - 1);
    }

    /** Returns a reason this fingerprint can't be compared with the scenario, or null. */
    String incompatibility(Scenario sc) {
      if (!sc.configSha().equals(meta.get("config-sha256")))
        return "the scenario's run settings (pre-setup, setup, go, steps, seeds, checkpoint-every, reporters, ignore-*)"
          + " changed since the fingerprint was recorded; re-record it with: perf/nlperf record " + sc.name + " --force";
      return null;
    }

    /** Model changes are the point here, so just say which model is being compared. */
    void printModelIdentity(Scenario sc) {
      System.out.println("fingerprint recorded " + meta.get("recorded") + "; model now at " + gitIdentity()
        + (sc.modelSha.equals(meta.get("model-sha256")) ? " (main model file unchanged)" : " (main model file changed)"));
    }
  }

  static int record(Scenario sc, Path golden) throws Exception {
    Path worlds = golden.resolve("worlds");
    Files.createDirectories(worlds);
    List<String> lines = new ArrayList<>(List.of(
      "# nlperf fingerprint",
      "# scenario: " + sc.name,
      "# model: " + Paths.get("").toAbsolutePath().relativize(sc.model),
      "# model-sha256: " + sc.modelSha,
      "# config-sha256: " + sc.configSha(),
      "# recorded: " + now() + " from " + gitIdentity(),
      "# settings: steps=" + sc.steps + " checkpoint-every=" + sc.checkpointEvery + " seeds=" + Arrays.toString(sc.seeds),
      sc.columns()));
    System.out.println("recording " + sc.name + ": " + sc.seeds.length + " seed(s), up to " + sc.steps
      + " steps, snapshot every " + sc.checkpointEvery);
    for (int seed : sc.seeds) {
      long t0 = System.nanoTime();
      int[] count = {0};
      Snapshot last;
      try (Run run = new Run(sc, seed)) {
        last = drive(sc, run, sc.checkpointEvery, s -> {
          lines.add(s.row().tsv());
          writeGzip(worlds.resolve(s.row().worldFile()), s.world());
          count[0]++;
          return true;
        });
      }
      lines.add(last.row().tsv());
      if (last.world() != null) writeGzip(worlds.resolve(last.row().worldFile()), last.world());
      System.out.printf("  seed %d: %d snapshots, ended at step %d (%s) in %.1f s%n", seed, count[0] + 1,
        last.row().step(), last.row().event(), (System.nanoTime() - t0) / 1e9);
      if (last.row().event().equals("error"))
        System.out.println("  warning: seed " + seed + " ended with a runtime error: " + last.row().values().get(0));
    }
    Files.write(golden.resolve("fingerprint.tsv"), lines);
    return OK;
  }

  static int verify(Scenario sc, Path golden, Path results) throws Exception {
    Fingerprint fp = Fingerprint.load(golden);
    if (fp == null) throw new UsageError("no fingerprint at " + golden + "; record one first: perf/nlperf record " + sc.name);
    String why = fp.incompatibility(sc);
    if (why != null) throw new UsageError(why);
    deleteRecursively(results.resolve("mismatch")); // dumps from an earlier verify would mislead

    System.out.println("verifying " + sc.name);
    fp.printModelIdentity(sc);
    int failures = 0;
    for (int seed : sc.seeds) {
      List<Row> expected = fp.rows.getOrDefault(seed, List.of());
      int[] i = {0};
      boolean[] failed = {false};
      Checkpoint compare = s -> {
        Row exp = i[0] < expected.size() ? expected.get(i[0]) : null;
        i[0]++;
        if (s.row().equals(exp)) return true;
        failed[0] = true;
        reportMismatch(sc, golden, results, exp, s);
        return false;
      };
      long t0 = System.nanoTime();
      try (Run run = new Run(sc, seed)) {
        Snapshot last = drive(sc, run, sc.checkpointEvery, compare);
        if (last != null) compare.at(last);
      }
      if (!failed[0] && i[0] != expected.size()) {
        failed[0] = true;
        System.out.println("  seed " + seed + ": MISMATCH, the fingerprint has " + expected.size()
          + " snapshots but this run produced " + i[0]);
      }
      if (failed[0]) failures++;
      else System.out.printf("  seed %d: identical (%d snapshots, %.1f s)%n", seed, i[0], (System.nanoTime() - t0) / 1e9);
    }
    System.out.println(failures == 0
      ? "OK: behavior is identical to the fingerprint"
      : "DIFFERENT: " + failures + " of " + sc.seeds.length + " seed(s) diverged from the fingerprint");
    return failures == 0 ? OK : DIFFERENT;
  }

  static void reportMismatch(Scenario sc, Path golden, Path results, Row expected, Snapshot actual) throws IOException {
    Row a = actual.row();
    System.out.println("  seed " + a.seed() + ": MISMATCH");
    System.out.println("    expected: " + (expected == null ? "(no more snapshots)" : expected.describe(sc.reporters)));
    System.out.println("    actual:   " + a.describe(sc.reporters));
    if (actual.world() == null || expected == null) return;
    Path mismatchDir = results.resolve("mismatch");
    Files.createDirectories(mismatchDir);
    Path actualFile = mismatchDir.resolve(a.worldFile().replaceFirst("\\.gz$", ""));
    Files.writeString(actualFile, actual.world());
    Path expectedFile = golden.resolve("worlds").resolve(expected.worldFile());
    if (!Files.isRegularFile(expectedFile)) {
      // World exports aren't committed (they're large), so a fresh clone lacks them.
      System.out.println("    no golden world export to diff against; to get them, re-record on the baseline commit:");
      System.out.println("      git stash; git checkout <baseline>; perf/nlperf record " + sc.name + " --force; git checkout -; git stash pop");
      System.out.println("    this run's export: " + actualFile);
      return;
    }
    if (expected.step() == a.step() && !expected.world().equals(a.world()))
      describeWorldDiff(readGzip(expectedFile), actual.world());
    System.out.println("    world exports: " + expectedFile);
    System.out.println("               vs: " + actualFile);
  }

  /**
   * Replaces the named globals' values in a world export's GLOBALS row, for
   * values such as wall-clock timestamps that differ between runs but don't
   * feed back into the model.
   */
  static String maskGlobals(String world, List<String> names) {
    if (names.isEmpty()) return world;
    String[] lines = world.split("\n", -1);
    for (int i = 0; i + 2 < lines.length; i++) {
      if (!lines[i].equals("\"GLOBALS\"")) continue;
      List<String> header = csv(lines[i + 1]).stream().map(String::toLowerCase).toList();
      List<String> values = new ArrayList<>(csv(lines[i + 2]));
      for (String name : names) {
        int idx = header.indexOf(name);
        if (idx < 0 || idx >= values.size()) throw new UsageError("ignore-globals: the model has no global named '" + name + "'");
        values.set(idx, "<ignored>");
      }
      lines[i + 2] = values.stream().map(v -> "\"" + v.replace("\"", "\"\"") + "\"").collect(Collectors.joining(","));
      return String.join("\n", lines);
    }
    throw new UsageError("ignore-globals: no GLOBALS section in the world export");
  }

  /**
   * Sorts the object lines of the named extensions in a world export's
   * EXTENSIONS section, for extensions that export objects in hash order.
   * Each extension's block is its quoted name, its lines, then a blank line
   * (see workspace.ExtensionManager.exportWorld).
   */
  static String sortExtensionObjects(String world, List<String> extensions) {
    if (extensions.isEmpty()) return world;
    List<String> lines = new ArrayList<>(Arrays.asList(world.split("\n", -1)));
    int start = lines.indexOf("\"EXTENSIONS\"");
    if (start < 0) throw new UsageError("unordered-extensions: no EXTENSIONS section in the world export");
    for (int i = start + 1; i < lines.size(); i++) {
      String line = lines.get(i);
      if (!line.startsWith("\"") || !extensions.contains(line.replace("\"", ""))) continue;
      int end = i + 1;
      while (end < lines.size() && !lines.get(end).isEmpty()) end++;
      java.util.Collections.sort(lines.subList(i + 1, end));
      i = end;
    }
    return String.join("\n", lines);
  }

  /** A section header line in a world export, such as "TURTLES" or "OUTPUT". */
  static final String SECTION_HEADER = "\"[A-Z][A-Z -]*\"";

  /** Replaces the contents of the named sections of a world export. */
  static String maskSections(String world, List<String> sections) {
    if (sections.isEmpty()) return world;
    String[] lines = world.split("\n", -1);
    List<String> present = new ArrayList<>();
    boolean masking = false;
    for (int i = 0; i < lines.length; i++) {
      if (lines[i].matches(SECTION_HEADER)) {
        String name = lines[i].replace("\"", "");
        present.add(name);
        masking = sections.contains(name);
      } else if (masking && !lines[i].isEmpty()) {
        lines[i] = "<ignored>";
      }
    }
    for (String s : sections)
      if (!present.contains(s))
        throw new UsageError("ignore-sections: the world export has no section '" + s + "' (it has " + present + ")");
    return String.join("\n", lines);
  }

  /** Prints the first few differing lines of two world exports, column by column. */
  static void describeWorldDiff(String expected, String actual) {
    String[] e = expected.split("\n", -1), a = actual.split("\n", -1);
    String section = "", header = null;
    boolean headerNext = false;
    int differing = 0, shown = 0;
    for (int i = 0; i < Math.min(e.length, a.length); i++) {
      if (e[i].matches(SECTION_HEADER)) {
        section = e[i].replace("\"", "");
        headerNext = true;
        header = null;
      } else if (headerNext) {
        header = e[i];
        headerNext = false;
      }
      if (e[i].equals(a[i])) continue;
      differing++;
      if (shown++ >= 8) continue;
      List<String> ev = csv(e[i]), av = csv(a[i]), cols = header == null ? List.of() : csv(header);
      List<String> changes = new ArrayList<>();
      for (int c = 0; c < Math.max(ev.size(), av.size()); c++) {
        String x = c < ev.size() ? ev.get(c) : "", y = c < av.size() ? av.get(c) : "";
        if (!x.equals(y))
          changes.add((c < cols.size() ? cols.get(c) : "col " + (c + 1)) + ": " + abbreviate(x) + " -> " + abbreviate(y));
      }
      String id = ev.isEmpty() || header == null ? "" : " " + (cols.isEmpty() ? "" : cols.get(0) + "=") + abbreviate(ev.get(0));
      if (cols.size() > 1 && ev.size() > 1 && cols.get(1).equals("pycor")) id += " pycor=" + ev.get(1); // patches
      System.out.println("    [" + section + id + "] " + String.join("; ", changes));
    }
    if (shown > 8) System.out.println("    ... " + (shown - 8) + " more differing lines");
    if (e.length != a.length)
      System.out.println("    the exports have different line counts (" + e.length + " vs " + a.length
        + "), so agents were probably created or removed differently; diff the files to see where");
    System.out.println("    " + differing + " differing line(s) when compared position by position");
  }

  // ---------------------------------------------------------------- timing

  static int bench(Scenario sc, Path golden, Path results, String label) throws Exception {
    Fingerprint fp = Fingerprint.load(golden);
    Row expected = null;
    if (fp == null) {
      System.out.println("warning: no fingerprint recorded, so this run's behavior is unverified");
    } else if (fp.incompatibility(sc) != null) {
      System.out.println("warning: " + fp.incompatibility(sc));
    } else {
      fp.printModelIdentity(sc);
      expected = fp.finalRow(sc.benchSeed);
      if (expected == null) System.out.println("warning: bench.seed " + sc.benchSeed + " is not among the recorded seeds, so behavior is unverified");
    }

    int cpus = Runtime.getRuntime().availableProcessors();
    System.out.printf("benchmarking %s: seed %d, %d warmup + %d measured runs, %d CPUs, Java %s, JVM %s%n",
      sc.name, sc.benchSeed, sc.benchWarmup, sc.benchRuns, cpus, System.getProperty("java.version"), jvmOptions());
    String behavior = expected == null ? "unverified" : "same";
    List<Run> measured = new ArrayList<>();
    for (int i = 0; i < sc.benchWarmup + sc.benchRuns; i++) {
      boolean warmup = i < sc.benchWarmup;
      System.gc();
      Snapshot last;
      Run run = new Run(sc, sc.benchSeed);
      try (run) { last = drive(sc, run, 0, null); }
      boolean same = expected == null || last.row().equals(expected);
      if (!same) behavior = "CHANGED";
      System.out.printf("  %s %d: setup %.0f ms, go %.0f ms, %d steps%s%n",
        warmup ? "warmup" : "run", warmup ? i + 1 : i - sc.benchWarmup + 1,
        run.setupNanos / 1e6, run.goNanos / 1e6, run.steps, same ? "" : "  <-- final state differs from fingerprint");
      if (!warmup) measured.add(run);
    }

    double[] go = measured.stream().mapToDouble(r -> r.goNanos / 1e6).sorted().toArray();
    double[] setup = measured.stream().mapToDouble(r -> r.setupNanos / 1e6).sorted().toArray();
    int steps = measured.get(0).steps;
    Stats s = new Stats(go);
    String[] row = {
      now(), label.isBlank() ? "-" : clean(label.trim()), env("NLPERF_GIT_COMMIT"), env("NLPERF_GIT_TREE"), behavior,
      String.valueOf(steps), fmt(s.median), fmt(s.min), fmt(s.stddev), String.format("%.3f", s.median / Math.max(1, steps)),
      fmt(new Stats(setup).median), cpus + (System.getenv("NLPERF_CPUSET") == null ? "" : "@" + System.getenv("NLPERF_CPUSET")),
      String.valueOf(go.length), jvmOptions(),
      measured.stream().map(r -> fmt(r.goNanos / 1e6)).collect(Collectors.joining(","))
    };

    Files.createDirectories(results);
    Path tsv = results.resolve("bench.tsv");
    List<String[]> history = new ArrayList<>();
    if (Files.isRegularFile(tsv)) {
      List<String> lines = Files.readAllLines(tsv);
      for (String line : lines.subList(1, lines.size())) history.add(line.split("\t", -1));
    } else
      Files.writeString(tsv, "timestamp\tlabel\tcommit\ttree\tbehavior\tsteps\tmedian_go_ms\tmin_go_ms\tstddev_go_ms"
        + "\tms_per_step\tmedian_setup_ms\tcpus\truns\tjvm\tgo_ms_all\n");
    Files.writeString(tsv, String.join("\t", row) + "\n", StandardOpenOption.APPEND);

    System.out.printf("go: median %s ms, min %s ms, stddev %s ms (%.1f%%), %s ms/step%n",
      row[6], row[7], row[8], 100 * s.stddev / s.median, row[9]);
    System.out.println("behavior: " + switch (behavior) {
      case "same" -> "final state identical to the fingerprint";
      case "CHANGED" -> "CHANGED, final state differs from the fingerprint; run perf/nlperf verify " + sc.name + " for details";
      default -> "unverified";
    });
    compareWithBaseline(history, row);
    System.out.println("appended to " + tsv);
    return behavior.equals("CHANGED") ? DIFFERENT : OK;
  }

  /**
   * A/B timing: runs a reference model (A) and the scenario's model (B) alternately
   * in one JVM, in pairs ordered AB, BA, AB, ..., so drift in the machine's speed
   * affects both alike. Reports each pair's B/A ratio and their median, and checks
   * both versions' final states against the fingerprint.
   */
  static int ab(Scenario b, Path golden, Path results, Path referenceModel, String label) throws Exception {
    Scenario a = Scenario.load(b.file);
    a.model = referenceModel.toAbsolutePath();
    a.modelSha = sha256(Files.readAllBytes(a.model));
    Fingerprint fp = Fingerprint.load(golden);
    Row expected = fp == null || fp.incompatibility(b) != null ? null : fp.finalRow(b.benchSeed);
    if (expected == null) System.out.println("warning: no compatible fingerprint, so behavior is unverified");
    String reference = a.model.getParent().getParent().getFileName().toString();
    System.out.printf("A/B %s: A = %s, B = %s; seed %d, %d warmup + %d measured pairs%n",
      b.name, reference, gitIdentity(), b.benchSeed, b.benchWarmup, b.benchRuns);
    List<double[]> pairs = new ArrayList<>();
    boolean same = true;
    for (int i = 0; i < b.benchWarmup + b.benchRuns; i++) {
      double[] ms = new double[2];
      for (int k = 0; k < 2; k++) {
        int which = (i % 2 == 0) ? k : 1 - k; // 0 = A, 1 = B
        Scenario sc = which == 0 ? a : b;
        System.gc();
        Run run = new Run(sc, sc.benchSeed);
        Snapshot last;
        try (run) { last = drive(sc, run, 0, null); }
        if (expected != null && !last.row().equals(expected)) {
          same = false;
          System.out.println("  " + (which == 0 ? "A" : "B") + ": final state differs from the fingerprint");
        }
        ms[which] = run.goNanos / 1e6;
      }
      boolean warmup = i < b.benchWarmup;
      System.out.printf("  %s %d: A %.0f ms, B %.0f ms, B/A %+.1f%%%n", warmup ? "warmup" : "pair",
        warmup ? i + 1 : i - b.benchWarmup + 1, ms[0], ms[1], 100 * (ms[1] / ms[0] - 1));
      if (!warmup) pairs.add(ms);
    }
    double[] ratios = pairs.stream().mapToDouble(p -> p[1] / p[0]).sorted().toArray();
    double medianA = new Stats(pairs.stream().mapToDouble(p -> p[0]).sorted().toArray()).median();
    double medianB = new Stats(pairs.stream().mapToDouble(p -> p[1]).sorted().toArray()).median();
    double median = new Stats(ratios).median();
    System.out.printf("B vs A: median %+.1f%% (pairs from %+.1f%% to %+.1f%%); median A %.0f ms, B %.0f ms%n",
      100 * (median - 1), 100 * (ratios[0] - 1), 100 * (ratios[ratios.length - 1] - 1), medianA, medianB);
    System.out.println("behavior: " + (expected == null ? "unverified" : same ? "both identical to the fingerprint" : "DIFFERENT"));
    Files.createDirectories(results);
    Path tsv = results.resolve("ab.tsv");
    if (!Files.isRegularFile(tsv))
      Files.writeString(tsv, "timestamp\tlabel\tA\tB\tpairs\tB_vs_A_median\tB_vs_A_min\tB_vs_A_max\tmedian_A_ms\tmedian_B_ms\n");
    Files.writeString(tsv, String.join("\t", now(), label.isBlank() ? "-" : clean(label.trim()), reference, gitIdentity(),
      String.valueOf(pairs.size()), String.format("%+.2f%%", 100 * (median - 1)), String.format("%+.2f%%", 100 * (ratios[0] - 1)),
      String.format("%+.2f%%", 100 * (ratios[ratios.length - 1] - 1)), fmt(medianA), fmt(medianB)) + "\n", StandardOpenOption.APPEND);
    System.out.println("appended to " + tsv);
    return same ? OK : DIFFERENT;
  }

  /** Compares with the most recent run labeled "baseline", or else the first recorded run. */
  static void compareWithBaseline(List<String[]> history, String[] row) {
    if (history.isEmpty()) {
      System.out.println(row[1].equals("baseline")
        ? "recorded as the baseline; later runs are compared against it"
        : "first result for this scenario; label a run \"baseline\" to compare later runs against it");
      return;
    }
    String[] base = history.get(0);
    for (String[] h : history) if (h[1].equals("baseline")) base = h;
    double b = Double.parseDouble(base[6]), c = Double.parseDouble(row[6]);
    double delta = 100 * (c - b) / b;
    // Rough 95% interval for the difference: 2 * standard error of the difference of means.
    double sb = Double.parseDouble(base[8]), sc = Double.parseDouble(row[8]);
    double noise = 200 * Math.sqrt(sb * sb / Integer.parseInt(base[12]) + sc * sc / Integer.parseInt(row[12])) / b;
    System.out.printf("vs %s (%s, %s %s): median %s -> %s ms, %+.1f%%%s%n",
      base[1].equals("baseline") ? "baseline" : "first run", base[0], base[2], base[3], base[6], row[6], delta,
      Math.abs(delta) < noise ? String.format(" (within noise, about +/-%.1f%%)", noise) : "");
    if (!base[11].equals(row[11]) || !base[13].equals(row[13]))
      System.out.println("warning: CPU count or JVM options differ from that run, so the timings are not directly comparable");
    if (!base[5].equals(row[5]))
      System.out.println("warning: step counts differ (" + base[5] + " vs " + row[5] + "), so the runs did different work");
  }

  /**
   * One long run of bench.seed. At every checkpoint it writes a row with the time
   * the interval took (excluding snapshots), a world hash and the reporters, and
   * prints progress, so a run that takes hours can be watched and stopped at any
   * point. Two traces of the same scenario compare with compare-traces.
   */
  static int trace(Scenario sc, Path out) throws Exception {
    Files.createDirectories(out.getParent());
    List<String> cols = new ArrayList<>(List.of("step", "event", "world", "interval_ms", "ms_per_step", "go_ms"));
    sc.reporters.forEach(r -> cols.add(clean(r)));
    Files.write(out, List.of(
      "# nlperf trace",
      "# scenario: " + sc.name + ", seed " + sc.benchSeed,
      "# model: " + gitIdentity(),
      "# started: " + now() + ", Java " + System.getProperty("java.version") + ", JVM " + jvmOptions()
        + ", " + Runtime.getRuntime().availableProcessors() + " CPUs",
      String.join("\t", cols)));
    System.out.printf("tracing %s: seed %d, up to %d steps, a row every %d -> %s%n",
      sc.name, sc.benchSeed, sc.steps, sc.checkpointEvery, out);
    try (Run run = new Run(sc, sc.benchSeed)) {
      long goNanos = 0, start = System.nanoTime();
      Snapshot s;
      try {
        run.exec(run.preSetup);
        run.exec(run.setup);
        long setup = System.nanoTime() - start;
        s = run.snapshot("setup");
        traceRow(out, s.row(), setup, 0, 0);
        long intervalStart = System.nanoTime();
        int lastStep = 0;
        while (run.steps < sc.steps) {
          if (run.exec(run.go)) { run.stopped = true; break; }
          run.steps++;
          if (run.steps % sc.checkpointEvery == 0 || run.steps == sc.steps) {
            long interval = System.nanoTime() - intervalStart;
            goNanos += interval;
            s = run.snapshot(run.steps == sc.steps ? "limit" : "checkpoint");
            traceRow(out, s.row(), interval, run.steps - lastStep, goNanos);
            lastStep = run.steps;
            intervalStart = System.nanoTime();
          }
        }
        if (run.stopped) {
          long interval = System.nanoTime() - intervalStart;
          goNanos += interval;
          traceRow(out, run.snapshot("stop").row(), interval, run.steps - lastStep, goNanos);
        }
      } catch (ModelError e) {
        traceRow(out, new Row(run.seed, run.steps, "error", "-", List.of(clean(e.getMessage()))), 0, 0, goNanos);
        System.out.println("  runtime error at step " + run.steps + ": " + e.getMessage());
      }
    }
    return OK;
  }

  static void traceRow(Path out, Row r, long intervalNanos, int steps, long goNanos) throws IOException {
    double ms = intervalNanos / 1e6;
    String perStep = steps > 0 ? String.format("%.1f", ms / steps) : "-";
    Files.writeString(out, r.step() + "\t" + r.event() + "\t" + r.world() + "\t" + fmt(ms) + "\t" + perStep + "\t"
      + fmt(goNanos / 1e6) + r.values().stream().map(v -> "\t" + v).collect(Collectors.joining()) + "\n",
      StandardOpenOption.APPEND);
    System.out.printf("  %s step %d: %s ms/step, %.0f s of go so far, %s%n", now(), r.step(), perStep, goNanos / 1e9,
      String.join(", ", r.values()));
  }

  /** Lines a trace's rows up with another's: speed per interval, and whether the worlds stayed identical. */
  static int compareTraces(Path a, Path b) throws IOException {
    List<String[]> ra = traceRows(a), rb = traceRows(b);
    List<String> header = Files.readAllLines(a).stream().filter(l -> !l.startsWith("#")).limit(1).toList();
    List<String> cols = header.isEmpty() ? List.of() : List.of(header.get(0).split("\t", -1));
    System.out.println("A: " + a + "\n   " + traceMeta(a) + "\nB: " + b + "\n   " + traceMeta(b) + "\n");
    String rep = cols.size() > 6 ? cols.get(6) : "";
    System.out.printf("%8s  %10s  %10s  %8s  %-9s  %s%n", "step", "A ms/step", "B ms/step", "B vs A", "world", rep);
    double ta = 0, tb = 0;
    int n = Math.min(ra.size(), rb.size()), firstDiff = -1;
    for (int i = 0; i < n; i++) {
      String[] x = ra.get(i), y = rb.get(i);
      boolean same = x[0].equals(y[0]) && x[2].equals(y[2]);
      if (!same && firstDiff < 0) firstDiff = i;
      String change = "-";
      if (!x[4].equals("-") && !y[4].equals("-")) {
        double pa = Double.parseDouble(x[4]), pb = Double.parseDouble(y[4]);
        change = String.format("%+.1f%%", 100 * (pb - pa) / pa);
        ta += Double.parseDouble(x[3]);
        tb += Double.parseDouble(y[3]);
      }
      System.out.printf("%8s  %10s  %10s  %8s  %-9s  %s%n", x[0], x[4], y[4], change,
        same ? "identical" : "DIFFERENT", x.length > 6 ? x[6] : "");
    }
    if (ta > 0)
      System.out.printf("%nover the %d steps both traces cover: A %.1f min, B %.1f min of go, B %+.1f%%%n",
        Integer.parseInt(ra.get(n - 1)[0]), ta / 60000, tb / 60000, 100 * (tb - ta) / ta);
    System.out.println(firstDiff < 0
      ? "worlds identical at all " + n + " shared checkpoints"
      : "worlds DIFFER from step " + ra.get(firstDiff)[0] + " on");
    if (ra.size() != rb.size()) System.out.println("(the traces have " + ra.size() + " and " + rb.size() + " rows; compared the first " + n + ")");
    return firstDiff < 0 ? OK : DIFFERENT;
  }

  static List<String[]> traceRows(Path p) throws IOException {
    List<String> lines = Files.readAllLines(p).stream().filter(l -> !l.startsWith("#")).toList();
    return lines.stream().skip(1).filter(l -> !l.isBlank()).map(l -> l.split("\t", -1)).toList();
  }

  static String traceMeta(Path p) throws IOException {
    return Files.readAllLines(p).stream().filter(l -> l.startsWith("# model: ") || l.startsWith("# started: "))
      .map(l -> l.substring(2)).collect(Collectors.joining("; "));
  }

  static int profile(Scenario sc, Path golden, Path results, Path jfrFile) throws Exception {
    Files.createDirectories(results);
    System.out.printf("profiling %s: seed %d, %d warmup + %d recorded runs%n", sc.name, sc.benchSeed, sc.benchWarmup, sc.profileRuns);
    for (int i = 0; i < sc.benchWarmup; i++)
      try (Run run = new Run(sc, sc.benchSeed)) { drive(sc, run, 0, null); }
    Files.deleteIfExists(jfrFile);
    Path part = jfrFile.resolveSibling(jfrFile.getFileName() + ".part");
    for (int i = 0; i < sc.profileRuns; i++) {
      // Opening and compiling the model happens here, outside the recording.
      Run run = new Run(sc, sc.benchSeed);
      try (run; Recording rec = new Recording(Configuration.getConfiguration("profile"))) {
        rec.enable("jdk.ExecutionSample").withPeriod(Duration.ofMillis(1));
        rec.setToDisk(true);
        rec.setDestination(part);
        rec.start();
        drive(sc, run, 0, null, false);
        rec.stop();
      }
      // A JFR file is a sequence of self-contained chunks, so recordings concatenate.
      Files.write(jfrFile, Files.readAllBytes(part), StandardOpenOption.CREATE, StandardOpenOption.APPEND);
      Files.delete(part);
      System.out.printf("  run %d: setup %.0f ms, go %.0f ms, %d steps%n", i + 1, run.setupNanos / 1e6, run.goNanos / 1e6, run.steps);
    }
    printProcedureProfile(sc.model, jfrFile);
    return OK;
  }

  /**
   * Attributes the recorded samples to NetLogo procedures. The bytecode
   * generator names its classes _asm_procedure<name>_... (or _asm_<name>_...
   * for an anonymous procedure inside <name>), where <name> is the lowercased
   * procedure name with punctuation dropped (generate.Generator).
   */
  static void printProcedureProfile(Path model, Path jfrFile) throws IOException {
    List<String> sources = modelSources(model);
    Map<String, String> names = procedureNames(sources);
    Map<String, String> bodies = procedureBodies(sources);
    List<String> keys = names.keySet().stream().sorted(Comparator.comparingInt(String::length).reversed()).toList();
    Map<String, Proc> cache = new HashMap<>();
    Map<String, Integer> self = new HashMap<>(), total = new HashMap<>();
    int samples = 0, truncated = 0;
    try (RecordingFile rf = new RecordingFile(jfrFile)) {
      while (rf.hasMoreEvents()) {
        RecordedEvent e = rf.readEvent();
        if (!e.getEventType().getName().equals("jdk.ExecutionSample") || e.getStackTrace() == null) continue;
        samples++;
        if (e.getStackTrace().isTruncated()) truncated++;
        String innermost = null;
        boolean inAnonymous = false, sawCode = false;
        Set<String> onStack = new HashSet<>();
        for (RecordedFrame f : e.getStackTrace().getFrames()) {
          String cls = f.getMethod().getType().getName();
          if (!cls.startsWith(ASM)) continue;
          Proc p = cache.computeIfAbsent(cls, c -> procedureOf(c, keys, names, bodies));
          sawCode = true;
          if (p.name() == null) { inAnonymous = true; continue; } // nested anonymous procedure: no name in its class
          if (innermost == null) innermost = p.name() + (p.anonymous() || inAnonymous ? " [anonymous]" : "");
          onStack.add(p.name());
        }
        self.merge(innermost != null ? innermost : sawCode ? "(anonymous procedure)" : "(not in model code)", 1, Integer::sum);
        for (String p : onStack) total.merge(p, 1, Integer::sum);
      }
    }
    if (samples == 0) return;
    final int n = samples;
    System.out.printf("%ntime by NetLogo procedure (%d samples). self: the procedure's own code, including the%n"
      + "engine work it triggers; total: anything while the procedure is on the stack.%n%n    self   total  procedure%n", n);
    Set<String> rows = new HashSet<>(self.keySet());
    rows.addAll(total.keySet());
    rows.stream()
      .sorted(Comparator.comparingInt((String p) -> self.getOrDefault(p, 0)).reversed())
      .limit(30)
      .forEach(p -> System.out.printf("  %5.1f%%  %6s  %s%n", 100.0 * self.getOrDefault(p, 0) / n,
        total.containsKey(p) ? String.format("%.1f%%", 100.0 * total.get(p) / n) : "-", p));
    if (truncated > 0)
      System.out.printf("  (%d%% of stacks were truncated, so totals of outer procedures are understated)%n", 100 * truncated / n);
  }

  static final String ASM = "org.nlogo.prim._asm_";

  /** A frame's procedure: its name (null for a nested anonymous procedure), and whether it's anonymous. */
  record Proc(String name, boolean anonymous) {}

  static Proc procedureOf(String className, List<String> keys, Map<String, String> names, Map<String, String> bodies) {
    String rest = className.substring(ASM.length());
    boolean anonymous = !rest.startsWith("procedure");
    if (!anonymous) rest = rest.substring("procedure".length());
    for (String k : keys)
      if (rest.startsWith(k)) return new Proc(names.get(k), anonymous);
    if (!anonymous) return new Proc(rest.replaceFirst("_.*", ""), false);
    // An anonymous procedure inside another one is named after that one's
    // generated name (__lambda<n>), so find the procedure whose source contains
    // its code instead. The class name holds that code, normalized like the keys,
    // between the parent's name and the instruction class (and a hash, if long).
    String code = rest.replaceFirst("^_*lambda\\d+_", "").replaceFirst("^(anonymous|arrow)(command|reporter)", "")
      .replaceFirst("\\$[0-9a-f]{32}\\$.*$", "").replaceFirst("_[a-z0-9]+_\\d+$", "");
    if (code.length() >= 12)
      for (var e : bodies.entrySet())
        if (e.getValue().contains(code)) return new Proc(e.getKey(), true);
    return new Proc(null, true);
  }

  /** The model's code section and the .nls files beside it. */
  static List<String> modelSources(Path model) throws IOException {
    List<String> sources = new ArrayList<>();
    String main = Files.readString(model);
    int start = main.indexOf("<code><![CDATA["), end = main.indexOf("]]></code>");
    sources.add(start >= 0 && end > start ? main.substring(start, end) : main.split("@#\\$#@#\\$#@")[0]);
    try (var files = Files.walk(model.getParent())) {
      for (Path p : files.filter(f -> f.toString().endsWith(".nls")).toList()) sources.add(Files.readString(p));
    }
    return sources;
  }

  /** Lowercase with everything but identifier characters dropped, as Generator names classes. */
  static String normalize(String s) {
    return s.toLowerCase(Locale.ROOT).codePoints().filter(Character::isJavaIdentifierPart)
      .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append).toString();
  }

  /** Procedure names keyed by their normalized form. */
  static Map<String, String> procedureNames(List<String> sources) {
    Map<String, String> names = new HashMap<>();
    Pattern to = Pattern.compile("(?im)^[ \\t]*to(?:-report)?[ \\t]+([^\\s\\[\\];]+)");
    for (String src : sources) {
      Matcher m = to.matcher(src);
      while (m.find()) {
        String key = normalize(m.group(1));
        if (!key.isEmpty()) names.putIfAbsent(key, m.group(1).toLowerCase(Locale.ROOT));
      }
    }
    return names;
  }

  /** Each procedure's body with comments removed, normalized. */
  static Map<String, String> procedureBodies(List<String> sources) {
    Map<String, String> bodies = new LinkedHashMap<>();
    Pattern proc = Pattern.compile("(?ims)^[ \\t]*to(?:-report)?[ \\t]+([^\\s\\[\\];]+)(.*?)^[ \\t]*end\\b");
    for (String src : sources) {
      Matcher m = proc.matcher(src);
      while (m.find())
        bodies.putIfAbsent(m.group(1).toLowerCase(Locale.ROOT), normalize(m.group(2).replaceAll(";[^\\n]*", "")));
    }
    return bodies;
  }

  record Stats(double median, double min, double stddev) {
    Stats(double[] sorted) {
      this(sorted.length % 2 == 1 ? sorted[sorted.length / 2] : (sorted[sorted.length / 2 - 1] + sorted[sorted.length / 2]) / 2,
        sorted[0], stddev(sorted));
    }

    static double stddev(double[] xs) {
      double mean = Arrays.stream(xs).average().orElse(0);
      return Math.sqrt(Arrays.stream(xs).map(x -> (x - mean) * (x - mean)).sum() / xs.length);
    }
  }

  // ---------------------------------------------------------------- utilities

  static String jvmOptions() {
    return ManagementFactory.getRuntimeMXBean().getInputArguments().stream()
      .filter(a -> !a.startsWith("-D") && !a.startsWith("--add-exports") && !a.startsWith("-XX:FlightRecorderOptions"))
      .collect(Collectors.joining(" "));
  }

  static String gitIdentity() { return env("NLPERF_GIT_COMMIT") + " (" + env("NLPERF_GIT_TREE") + ")"; }

  static String env(String name) {
    String v = System.getenv(name);
    return v == null || v.isEmpty() ? "unknown" : v;
  }

  static String now() { return Instant.now().truncatedTo(ChronoUnit.SECONDS).toString(); }

  static String fmt(double ms) { return String.format("%.1f", ms); }

  static String clean(String s) { return s.replaceAll("[\t\r\n]+", " "); }

  static String abbreviate(String s) { return s.length() <= 60 ? s : s.substring(0, 57) + "..."; }

  static String sha256(byte[] bytes) {
    try {
      return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
    } catch (java.security.NoSuchAlgorithmException e) {
      throw new IllegalStateException(e);
    }
  }

  static void deleteRecursively(Path dir) throws IOException {
    if (!Files.exists(dir)) return;
    try (var paths = Files.walk(dir)) {
      for (Path p : paths.sorted(Comparator.reverseOrder()).toList()) Files.delete(p);
    }
  }

  static void writeGzip(Path file, String content) throws IOException {
    try (var out = new GZIPOutputStream(Files.newOutputStream(file))) {
      out.write(content.getBytes(StandardCharsets.UTF_8));
    }
  }

  static String readGzip(Path file) throws IOException {
    try (var in = new GZIPInputStream(Files.newInputStream(file))) {
      return new String(in.readAllBytes(), StandardCharsets.UTF_8);
    }
  }

  /** Splits one line of NetLogo's export CSV (quoted fields, doubled quotes). */
  static List<String> csv(String line) {
    List<String> out = new ArrayList<>();
    StringBuilder f = new StringBuilder();
    boolean quoted = false;
    for (int i = 0; i < line.length(); i++) {
      char c = line.charAt(i);
      if (quoted) {
        if (c != '"') f.append(c);
        else if (i + 1 < line.length() && line.charAt(i + 1) == '"') { f.append('"'); i++; }
        else quoted = false;
      } else if (c == '"') {
        quoted = true;
      } else if (c == ',') {
        out.add(f.toString());
        f.setLength(0);
      } else {
        f.append(c);
      }
    }
    out.add(f.toString());
    return out;
  }
}
