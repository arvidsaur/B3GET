# b3get extension

B3GET's own NetLogo extension. It holds the busiest loops of the model in Java;
the model logic itself (statuses, action names, costs, energy) stays in NetLogo.
NetLogo loads `b3get.jar` from this folder because the model lists `b3get` in its
`extensions` line, so nothing needs installing. The jar is committed; rebuild it
after changing `src/`.

| Primitive | Replaces | Used in |
|---|---|---|
| `b3get:sta2us-decisions` | the loops over alleles and objects in `sta2us-get-decisions` | `sta2us.nls` |
| `b3get:position` | `position` of an item in a list, with a quick test that skips most unequal lists | `check-energy`, `index-updated` |

Each primitive reports exactly what the NetLogo code it replaces reported. It uses
NetLogo's own equality (`Equality.equals`) and rounding (`Approximate.approximate`,
which is `precision`), and it runs `sta2us-get-action` from the model to name
actions. `perf/nlperf verify` confirms the model's behavior is unchanged.

## Building

With a JDK 17 or later, pass the folder of your NetLogo 7.0.4 installation that
contains `netlogo-7.0.4.jar` (`lib/app` on Linux):

```sh
code/b3get/build.sh /path/to/NetLogo-7.0.4/lib/app
```

`perf/nlperf` rebuilds the jar in its container whenever `src/` is newer than the
jar. The build is reproducible, so the same sources give the same jar.
