#!/usr/bin/env python3
"""Verify the primary country estimates in a fresh reproduction output."""

from __future__ import annotations

import csv
import math
import sys
from pathlib import Path


EXPECTED = {
    "Australia": 0.0000811688311688,
    "New Zealand": -0.14048618048618,
    "Croatia": 0.218928865314582,
}
TOLERANCE = 1e-10


def main() -> int:
    if len(sys.argv) != 2:
        print("usage: verify_reproduced_results.py REPRODUCED_OUTPUT", file=sys.stderr)
        return 2
    path = Path(sys.argv[1]) / "analysis" / "Iodine_PublicP3C_Harmonized_Effects.csv"
    if not path.is_file():
        print(f"FAIL missing {path}", file=sys.stderr)
        return 1
    with path.open(encoding="utf-8-sig", newline="") as handle:
        rows = list(csv.DictReader(handle))
    observed = {row["country"]: float(row["slope_change"]) for row in rows}
    failures = []
    for country, expected in EXPECTED.items():
        value = observed.get(country)
        if value is None or not math.isfinite(value) or abs(value - expected) > TOLERANCE:
            failures.append(f"{country}: observed={value!r}; expected={expected!r}")
    if failures:
        print("FAIL primary-result verification", file=sys.stderr)
        print("\n".join(failures), file=sys.stderr)
        return 1
    expected_figures = [
        "Figure_1.pdf", "Figure_1.png", "Figure_1.tiff",
        "Figure_5.pdf", "Figure_5.png", "Figure_5.tiff",
    ]
    expected_figures.extend(
        f"Iodine_PublicP3C_Figure{number}.{suffix}"
        for number in (2, 3, 4)
        for suffix in ("pdf", "png", "svg", "tiff")
    )
    expected_figures.extend(
        f"Iodine_PublicP3C_FigureS1_Histology.{suffix}"
        for suffix in ("pdf", "png", "svg", "tiff")
    )
    missing_figures = [
        name for name in expected_figures
        if not (Path(sys.argv[1]) / "figures" / name).is_file()
    ]
    if missing_figures:
        print("FAIL missing reproduced figures: " + ", ".join(missing_figures), file=sys.stderr)
        return 1
    print("PRIMARY_RESULTS_VERIFIED: PASS")
    print(f"REPRODUCED_FIGURES_VERIFIED: {len(expected_figures)}")
    print(f"TOLERANCE: {TOLERANCE}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
