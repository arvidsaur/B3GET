# b3get extension

B3GET's own NetLogo extension. It holds the busiest loops of the model in Java;
the model logic itself (statuses, action names, costs, energy) stays in NetLogo.
NetLogo loads `b3get.jar` from this folder because the model lists `b3get` in its
`extensions` line, so nothing needs installing. The jar is committed; rebuild it
after changing `src/`.

| Primitive | Replaces | Used in |
|---|---|---|
| `b3get:sta2us-decisions` | the loops over alleles and objects in `sta2us-get-decisions`, and its table of statuses by identity | `sta2us.nls` |
| `b3get:position` | `position` of an item in a list, with a quick test that skips most unequal lists | `check-energy`, `index-updated` |
| `b3get:with-item` | `filter [ x -> item i x = value ] list` | `get-action-cost-of`, `decisions-targeting` |
| `b3get:sum-item` | `sum map [ x -> item i x ] list` | `get-action-cost-of` |
| `b3get:matching-alleles` | the loop over identity loci in `relatedness-with` | `relatedness-with` |
| `b3get:precision` | `precision`, without its slow decimal conversion except next to a rounding tie | `complete-action`; also in `b3get:sta2us-decisions` |
| `b3get:lput-completed` | `lput` of a completed action, also updating the caller's index of completed-action costs | `complete-action` |
| `b3get:completed-cost` | `sum-item` of the completed actions with a given target and name, looked up in that index | `get-action-cost-of` |

Each primitive reports exactly what the NetLogo code it replaces reported. It uses
NetLogo's own equality (`Equality.equals`), and it runs `sta2us-get-action` from the
model to name actions. `perf/nlperf verify` confirms the model's behavior is
unchanged.

`get-action-cost-of` runs for every interaction and used to scan all of the
target's completed actions each time. Each individual now keeps an index of its
list of completed actions: the costs summed by target and action name, in list
order, so each sum is the same number the scan gave. An index is used only for the
exact list it was built from (NetLogo lists never change), so a list changed in any
other way than by `b3get:lput-completed` just gets a new index.

NetLogo's `precision` (`Approximate.approximate`) converts the number to decimal
text with `Double.toString` and rounds that, which is slow. `b3get:precision` gets
the same result by arithmetic whenever the number is not next to a rounding tie,
and calls `Approximate.approximate` otherwise (the comment on `precision` in
`B3getExtension.java` explains why that is exact). `test/PrecisionTest.java`
compares the two, bit for bit, on tens of millions of numbers, including numbers
next to ties; run it with the Java that runs NetLogo:

```sh
javac --release 17 -cp "code/b3get/b3get.jar:<NetLogo>/lib/app/*" -d /tmp/test code/b3get/test/PrecisionTest.java
java -cp "code/b3get/b3get.jar:<NetLogo>/lib/app/*:/tmp/test" PrecisionTest
```

## Building

With a JDK 17 or later, pass the folder of your NetLogo 7.0.4 installation that
contains `netlogo-7.0.4.jar` (`lib/app` on Linux):

```sh
code/b3get/build.sh /path/to/NetLogo-7.0.4/lib/app
```

`perf/nlperf` rebuilds the jar in its container whenever `src/` is newer than the
jar. The build is reproducible, so the same sources give the same jar.
