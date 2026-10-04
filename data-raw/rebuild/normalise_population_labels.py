#!/usr/bin/env python3
"""One label per population.

`population_label` in population_information, and `population` in
sample_information, held pipe-joined alternatives for the handful of
populations whose source datasets used more than one name for the same group
(GBR as both "British" and "English", MXL as both "Mexican_American" and
"Mexican_LA"). Worse, the joined form was written only to the individual rows
that arrived under both names, so `count(pop, population)` split a population
in two.

This script makes `population_label` single-valued, moves the remaining names
to a new `population_alt` column so they stay matchable, and rewrites
sample_information's `population` from the pop -> label map rather than
leaving it per-sample. Run from data-raw/rebuild; writes in place to
../../inst/extdata and to the copies here.

The fix is also applied at source in build_tables.py and
apply_igsr_splits.py, so a full rebuild no longer produces joined labels.
"""

import csv, os, sys

# Chosen primary label for each population carrying more than one. Everything
# not listed falls back to the first component, with a warning.
PRIMARY = {
    "GBR":        "British",           # alt: English
    "IBS":        "Iberian",           # alt: Spanish
    "FIN":        "Finnish",           # alt: Finish -- "Finish" is a typo in
                                       # the legacy labels, kept as an alias
    "MXL":        "Mexican_American",  # alt: Mexican_LA
    "MEXHapMap":  "Mexican_American",  # alt: Mexican_LA
    "CEUHapMap":  "CEPH_Europeans",    # alt: European
}

HERE = os.path.dirname(os.path.abspath(__file__))
# Only the installed tables. The copies in this directory are stale build
# intermediates from before add_aadr.py and apply_review.py ran.
TARGETS = [os.path.join(HERE, "..", "..", "inst", "extdata")]


def read(path):
    with open(path, newline="") as f:
        r = csv.DictReader(f, delimiter="\t")
        return list(r), r.fieldnames


def write(path, rows, fields):
    with open(path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fields, delimiter="\t",
                           quoting=csv.QUOTE_MINIMAL, lineterminator="\n")
        w.writeheader()
        w.writerows(rows)


def split_label(pop, label):
    """-> (primary, alt) for one population."""
    parts = [p for p in label.split("|") if p and p != "NA"]
    if len(parts) <= 1:
        return (label, "NA")
    if pop in PRIMARY:
        primary = PRIMARY[pop]
        if primary not in parts:
            sys.exit(f"PRIMARY['{pop}'] = {primary!r} is not one of {parts}")
    else:
        primary = parts[0]
        print(f"  warning: no PRIMARY entry for {pop}; taking {primary!r} "
              f"from {parts}")
    alt = "|".join(p for p in parts if p != primary)
    return (primary, alt or "NA")


for out in TARGETS:
    pi_path = os.path.join(out, "population_information.Rtable")
    si_path = os.path.join(out, "sample_information.Rtable")
    dl_path = os.path.join(out, "population_dplace_link.Rtable")
    if not os.path.exists(pi_path):
        continue
    print(f"-> {os.path.relpath(out, HERE)}")

    pi, pi_fields = read(pi_path)
    label_of = {}
    n_split = 0
    for r in pi:
        primary, alt = split_label(r["pop"], r["population_label"])
        if alt != "NA":
            n_split += 1
        r["population_label"] = primary
        r["population_alt"] = alt
        label_of[r["pop"]] = primary
    if "population_alt" not in pi_fields:
        i = pi_fields.index("population_label") + 1
        pi_fields = pi_fields[:i] + ["population_alt"] + pi_fields[i:]
    write(pi_path, pi, pi_fields)
    print(f"   population_information: {len(pi)} populations, "
          f"{n_split} had alternative labels")

    # sample_information: population is now derived, never stored per sample
    si, si_fields = read(si_path)
    n_fixed = 0
    unknown = set()
    for r in si:
        if r["pop"] in label_of:
            if r["population"] != label_of[r["pop"]]:
                n_fixed += 1
            r["population"] = label_of[r["pop"]]
        elif r["pop"] not in ("NA", ""):
            unknown.add(r["pop"])
    write(si_path, si, si_fields)
    print(f"   sample_information: {len(si)} samples, {n_fixed} labels "
          f"realigned to their population")
    if unknown:
        print(f"   warning: {len(unknown)} pop codes absent from "
              f"population_information: {sorted(unknown)[:5]}")

    # the dplace link table carries a label for readability only
    if os.path.exists(dl_path):
        dl, dl_fields = read(dl_path)
        n_dl = 0
        for r in dl:
            if r["pop"] in label_of and r["population_label"] != label_of[r["pop"]]:
                r["population_label"] = label_of[r["pop"]]
                n_dl += 1
        write(dl_path, dl, dl_fields)
        print(f"   population_dplace_link: {n_dl} labels updated")
