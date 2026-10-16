# perf: reproducible benchmarks for B3GET

`perf/nlperf` runs B3GET headless on the official NetLogo 7.0.4 release, using
the Java runtime it bundles, in a pinned Docker environment. A **behavioral
fingerprint** was recorded from the unmodified model. Every model change is
checked against it, so a speedup only counts if the simulation is exactly the
same, and then timed.

```sh
perf/nlperf verify  b3get            # identical to the baseline fingerprint? (~2.5 min)
perf/nlperf bench   b3get my-change  # timing, compared with the baseline (~5 min)
perf/nlperf profile b3get            # time per NetLogo procedure (~2 min)
perf/nlperf history b3get            # all bench results so far
```

All you need is Docker. The first run builds the image, which downloads
NetLogo 7.0.4 (~400 MB, pinned by checksum).

The baseline fingerprint, `golden/b3get/fingerprint.tsv`, was recorded from
`6ac8750` (upstream master). Only re-record it with `perf/nlperf record b3get
--force` on an unmodified model, or after deliberately changing the scenario.
The same fingerprint also came out bit-identical on NetLogo's unreleased
7.1.0 development build running on Java 21.

## What "identical" means

Each snapshot hashes the model's full `export-world` output: the RNG state,
globals, every animal and patch variable, plots, and the table extension's
tables. Because the RNG state is part of the hash, a change that draws random
numbers in a different order or number is caught even if the world happens to
look the same. Snapshots are taken after setup, every 50 ticks, and at tick
300, for seeds 1, 2 and 3.

A few things are left out because they come from the wall clock, not the
simulation, and they only label files and the on-screen log
(`interface.nls`):

- the globals `simulation-id` (built from `time:create ""`), `start-date-and-time`
  and `output-body`
- the `OUTPUT` section (the simulation log)

Two more adjustments keep the comparison deterministic:

- `metafile.csv` is deleted before each run, because the model loads it back in
  at setup.
- The table extension exports tables in `WeakHashMap` order and includes ones
  not yet garbage-collected. So a snapshot runs a full GC first and sorts the
  tables; their contents are still compared.

`record` reran everything in a fresh JVM and got identical results, so nothing
else varies between runs. `bench` also compares every timed run's final state
with the fingerprint and marks the result `CHANGED` if it differs.

When `verify` fails, it names the first diverging snapshot and the first
changed variables. For example, here is the output after changing
`update-patch-color` from `((a + b) / 2) / plant-quality` to
`a / (2 * plant-quality) + b / (2 * plant-quality)`:

```
  seed 1: MISMATCH
    expected: step 50 (checkpoint), world 7ec262e3b0ae6eb4, count anima1s with [ is.alive ] = 102, count anima1s = 116, ...
    actual:   step 50 (checkpoint), world 02cd34b33e2be95e, count anima1s with [ is.alive ] = 102, count anima1s = 116, ...
    [PATCHES pxcor=59 pycor=99] pcolor: 53.45688296002078 -> 53.45688296002077
    [PATCHES pxcor=5 pycor=98] pcolor: 53.211856169894695 -> 53.2118561698947
```

The two forms are equal on paper, but the second rounds twice. About 270
patches per seed came out different in the last digit, while every summary
reporter stayed the same. By contrast, rewriting the expression as
`(a + b) / (2 * plant-quality)` passes `verify`. Dividing by 2 is exact in
binary floating point, so both forms round the same quotient once.

The full world exports behind the fingerprint (13 MB) aren't committed. On a
fresh clone, `verify` still detects a difference but can't show which variable
changed; re-record on the baseline commit to get the exports locally.

## Keeping a model change identical

These are the usual ways a "pure refactor" of NetLogo code changes results.
`verify` catches all of them.

- **Random draws** must be the same in number and order. `ask` and `[...] of`
  visit agents in random order, using the RNG. `one-of`, `n-of`, `random`,
  `random-float` and `shuffle` draw from it. So replacing `[x] of agents` with
  `map [a -> [x] of a] sort agents`, or moving a `random` call, changes
  everything that follows.
- **Floating-point arithmetic** must happen in the same order:
  `a + b + c` and `a + (b + c)` can differ in the last bits, and so can summing
  the same values in a different order.
- **Agent creation order** decides `who` numbers.
- **Table insertion order** decides `table:keys` order.

## Timings

A run takes about 39 s (129 ms per tick). The spread between runs is about
0.3%, so `bench` can resolve changes of a percent or two. `record` takes about
5 minutes; `verify` takes 2.5 minutes, less when it fails, since each seed stops
at its first mismatch.

- Pin CPUs with `NLPERF_CPUSET=2-5 perf/nlperf bench ...`, and only compare
  runs with the same pinning and JVM options (`bench` warns otherwise).
- Later runs are compared with the most recent run labeled `baseline`. The
  "within noise" note is a rough 95% interval from the spread *within* one
  invocation; drift *between* invocations can be larger, so re-run the
  baseline right next to a candidate before trusting a small win.
- Each bench row records the commit and a hash of uncommitted changes to
  `code/` and `data/`, so results stay tied to the model they measured.
- Timings cover setup and the go loop. They exclude opening and compiling the
  model and taking snapshots.

## Profiles

`perf/nlperf profile b3get` records one run with JFR, sampling every 1 ms. It
then prints the time per NetLogo procedure: **self** is the procedure's own
code, including the engine work it triggers; **total** is everything while
it's on the stack. After that come the hottest JVM methods, which show which
primitives are expensive (e.g. `_removeitem`, `TableExtension$Get`). The
recording is kept in `perf/results/b3get/` for JDK Mission Control.

The procedure names come from the classes NetLogo's bytecode generator emits
(`_asm_procedure<name>_...`). Rows marked `[anonymous]` are anonymous
procedures, placed by finding their code in the model's source. This mapping
is approximate. Instructions the generator doesn't compile into classes are
counted toward the nearest compiled procedure below them, usually the caller.
So a small procedure can show a little self time that belongs to the
procedures it calls; `make-decisions` is an example.

Baseline profile, first 300 ticks (self / total):

```
   32.7%       -  sta2us-get-decisions [anonymous]   (its loop over genes x surrounding objects)
   12.3%   12.3%  check-energy
    8.0%   55.2%  make-decisions
    6.3%       -  do-actions [anonymous]
    5.5%    5.5%  first-string-is-part-of-last?
    5.0%    5.8%  get-action-cost-of
    2.9%    4.3%  status-of
    1.7%   46.8%  sta2us-get-decisions
```

## Long runs

The 300-tick scenario starts from about 100 animals, but real runs last weeks
and slow down as the population grows. `trace` covers that case: one long run
that writes a row every `checkpoint-every` ticks with the time per tick for
that interval, a world hash, and the reporters (population, environment size,
decisions per tick):

```sh
perf/nlperf trace b3get-long my-label        # up to 20,000 ticks; stop it any time
perf/nlperf compare-traces A.tsv B.tsv       # speed per interval, and whether the worlds match
```

To compare two versions of the model, run each from its own git worktree, so
they don't share `results/metafile.csv`, pinned to separate cores:

```sh
git worktree add --detach ../B3GET-original 6ac8750
git -C ../B3GET-original checkout performance -- perf
NLPERF_CPUSET=2-5 ../B3GET-original/perf/nlperf trace b3get-long original
NLPERF_CPUSET=8-11 perf/nlperf trace b3get-long optimized
```

## Scenario files

`perf/scenarios/<name>.properties`:

| key | default | meaning |
|---|---|---|
| `model` | (required) | model path from the repo root |
| `pre-setup` | empty | commands run after seeding and before setup |
| `setup`, `go` | `setup`, `go` | the setup and go commands |
| `steps` | (required) | maximum go calls per run (a run also ends when go calls `stop`) |
| `seeds` | `1 2 3` | seeds for `record` and `verify` |
| `checkpoint-every` | `steps / 10` | snapshot interval in steps |
| `reporter.1`, `reporter.2`, ... | none | reporters shown with each snapshot, run with their own RNG |
| `ignore-globals` | none | globals left out of the fingerprint |
| `ignore-sections` | none | world-export sections left out of the fingerprint |
| `unordered-extensions` | none | extensions whose exported objects are sorted before hashing |
| `bench.warmup`, `bench.runs` | `3`, `10` | warmup and measured runs for `bench` |
| `bench.seed` | first seed | seed used for timing and profiling |
| `profile.runs` | `3` | runs recorded by `profile` |
| `jvm` | `-Xms2g -Xmx2g` | JVM options for model runs (`NLPERF_JVM_OPTS` overrides) |

Changing any run setting (everything except `bench.*`, `profile.runs` and
`jvm`) invalidates the fingerprint, so `verify` refuses until it's re-recorded.

## Environment

- **Image** (`perf/Dockerfile`): the NetLogo 7.0.4 Linux release (jars, bundled
  runtime, bundled extensions) on top of Zulu JDK 21.0.2. The JDK is only used
  to compile the harness and read profiles. It's rebuilt automatically when
  the Dockerfile changes.
- **Model runs** use NetLogo's bundled Java 17.0.2 with its launcher's options.
  They work like BehaviorSpace runs (a fresh workspace per run, `go` until
  `steps` or `stop`), with `print` output silenced and no network access.
- **The repo** is bind-mounted at its host path, and the container runs as
  your user.

## Limits

- The scenario covers the first 300 ticks from the default population, with
  `output-results?` off as in the model's defaults. Long runs with large
  evolved populations, or runs that write results, may have other hotspots.
- `model-sha256` in the fingerprint covers `code/B3GET.nlogox` only. The bench
  `tree` column covers all of `code/` and `data/`.

## Layout

```
perf/nlperf               wrapper script (host side)
perf/Dockerfile           pinned environment
perf/harness/NlPerf.java  headless driver: record / verify / bench / profile
perf/scenarios/           scenario files
perf/golden/<scenario>/   fingerprint.tsv (committed) and world exports (local only)
perf/results/<scenario>/  bench.tsv history, JFR profiles, mismatch dumps (gitignored)
```
