#!/usr/bin/env python3
"""Make a larger B3GET starting population from an exported one.

Every animal is copied COPIES more times. Each copy gets a fresh identity,
and its carried.items refer to the copies made alongside it, so a mother's
copy carries copies of her own infants and gestatees. Each copied group is
shifted together by a random offset of up to SHIFT patches in x and y,
wrapping around the 100 x 100 world. Every other field is written exactly as
in the input, quoting included, so the model imports the same values.

usage: make-population.py <in.csv> <out.csv> [copies=3] [seed=1] [shift=5]

population-460.csv: make-population.py data/population.csv perf/populations/population-460.csv 3 1 5
"""

import random
import re
import sys

WORLD = 100  # min-pxcor 0 .. max-pxcor 99, wrapping (the same for y)


def read_csv(text):
    """Rows of (value, was_quoted) fields. Quoted fields may hold commas, quotes and newlines."""
    rows, row, field = [], [], []
    quoted = in_quotes = False
    i, n = 0, len(text)
    while i < n:
        c = text[i]
        if in_quotes:
            if c == '"' and i + 1 < n and text[i + 1] == '"':
                field.append('"')
                i += 1
            elif c == '"':
                in_quotes = False
            else:
                field.append(c)
        elif c == '"':
            in_quotes = quoted = True
        elif c == ',':
            row.append((''.join(field), quoted))
            field, quoted = [], False
        elif c in '\r\n':
            if c == '\r' and i + 1 < n and text[i + 1] == '\n':
                i += 1
            row.append((''.join(field), quoted))
            rows.append(row)
            row, field, quoted = [], [], False
        else:
            field.append(c)
        i += 1
    if field or row:
        row.append((''.join(field), quoted))
        rows.append(row)
    return rows


def write_field(value, quoted):
    return '"' + value.replace('"', '""') + '"' if quoted else value


def wrap(coordinate):
    """Into [-0.5, 99.5), the world's extent."""
    return (coordinate + 0.5) % WORLD - 0.5


def main(argv):
    if len(argv) < 3:
        sys.exit(__doc__)
    src, dst = argv[1], argv[2]
    copies = int(argv[3]) if len(argv) > 3 else 3
    rng = random.Random(int(argv[4]) if len(argv) > 4 else 1)
    shift = float(argv[5]) if len(argv) > 5 else 5.0

    with open(src, newline='', encoding='utf-8') as f:
        text = f.read()
    newline = '\r\n' if '\r\n' in text else '\n'
    rows = read_csv(text)
    header, body = [v for v, _ in rows[0]], rows[1:]
    col = {name: header.index(name) for name in ('my.identity', 'xcor', 'ycor', 'carried.items', 'group.identity')}

    originals = [int(float(r[col['my.identity']][0])) for r in body]
    used = set(originals) | {9999999}  # import-population treats 9999999 as "no identity"

    def fresh_identity():
        while True:
            candidate = rng.randrange(9999999)
            if candidate not in used:
                used.add(candidate)
                return candidate

    out = list(rows)
    for _ in range(copies):
        identities = {old: fresh_identity() for old in originals}
        offsets = {}
        for r in body:
            group = r[col['group.identity']][0]
            if group not in offsets:
                offsets[group] = (rng.uniform(-shift, shift), rng.uniform(-shift, shift))
            dx, dy = offsets[group]
            copy = list(r)
            old_id = int(float(r[col['my.identity']][0]))
            copy[col['my.identity']] = (str(identities[old_id]), r[col['my.identity']][1])
            copy[col['xcor']] = (repr(wrap(float(r[col['xcor']][0]) + dx)), r[col['xcor']][1])
            copy[col['ycor']] = (repr(wrap(float(r[col['ycor']][0]) + dy)), r[col['ycor']][1])
            carried, quoted = r[col['carried.items']]
            carried = re.sub(r'\d+(?:\.\d+)?',
                             lambda m: str(identities.get(int(float(m.group())), m.group())), carried)
            copy[col['carried.items']] = (carried, quoted)
            out.append(copy)

    with open(dst, 'w', newline='', encoding='utf-8') as f:
        f.write(newline.join(','.join(write_field(v, q) for v, q in row) for row in out) + newline)
    print(f'{dst}: {len(body)} animals -> {len(out) - 1}')


if __name__ == '__main__':
    main(sys.argv)
