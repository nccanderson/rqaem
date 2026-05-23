#!/usr/bin/env python3
"""Regenerate inst/extdata/python_fixtures.json.

Runs the unmodified reference Rqa.py / RqaDur.py over a curated set of
inputs and dumps every scalar metric (plus the recurrence matrix) for
each case. The R parity tests in tests/testthat/test-rqa.R load this
JSON and expect rqa() to match to tolerance 1e-8.

If you change the cases here, re-run this script and commit the new
JSON. Do not edit python_fixtures.json by hand.

Run from the package root:

    uv run --with numpy --with scipy --with scikit-image \
        data-raw/make_python_fixtures.py
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

# Make the reference Python implementation importable.
HERE = Path(__file__).resolve().parent
PYREF = HERE.parent.parent / "rqa_original" / "RqaPython"
if not PYREF.is_dir():
    sys.exit(f"reference Python directory not found: {PYREF}")
sys.path.insert(0, str(PYREF))

import numpy as np  # noqa: E402

from Rqa import Rqa  # noqa: E402
from RqaDur import RqaDur  # noqa: E402


# ----- inputs ----------------------------------------------------------------

# TestRqa.py Example 1 — 40-fixation scanpath, radius 64.
TEST_RQA_XY = [
    [510.4, 385.4], [466.5, 429.5], [406.0, 448.9], [135.1, 332.8],
    [296.1, 409.2], [117.5, 398.3], [317.7, 327.4], [439.3, 305.5],
    [302.4, 270.2], [444.0, 347.5], [507.3, 454.3], [341.2, 327.5],
    [308.8, 259.3], [459.1, 270.8], [493.9, 293.2], [630.3, 341.8],
    [655.9, 431.7], [798.6, 529.0], [851.1, 400.3], [768.9, 488.8],
    [485.0, 595.2], [256.0, 707.6], [358.8, 652.0], [264.7, 564.3],
    [ 87.5, 551.0], [ 68.3, 557.6], [310.1, 583.2], [474.2, 559.0],
    [449.9, 611.5], [176.9, 570.0], [279.8, 567.2], [358.3, 576.0],
    [440.2, 550.4], [505.4, 612.0], [655.4, 555.6], [884.5, 557.4],
    [884.1, 516.5], [683.5, 448.1], [635.3, 325.3], [570.5, 292.5],
]

# TestRqa.py Example 2 — categorical sequence, radius 0.1.
TEST_RQA_CATEGORIES = [
    37, 28, 28, 28, 19, 18, 9, 1, 1, 9, 9, 1, 1, 9, 9,
    17, 34, 42, 43, 44, 44, 45, 36, 28, 19, 11, 1,
]

# TestRqaDur.py Example 1 — durations from real data, radius 64.
TEST_RQADUR_XYD = [
    [523.00, 420.70, 343], [591.20, 491.70,  83], [819.40, 616.20, 199],
    [880.10, 609.90, 282], [904.80, 316.50, 267], [869.60, 246.30, 297],
    [652.50, 232.60, 284], [495.40, 236.40, 212], [345.00, 275.70, 485],
    [251.30, 411.60, 343], [217.70, 472.40, 593], [449.40, 617.80, 385],
    [559.20, 371.80, 311], [512.50, 347.70, 206], [385.10, 338.50, 282],
    [331.70, 332.00, 277], [251.40, 198.20, 178], [128.50,  78.40, 308],
    [ 45.80,  39.80, 442], [ 97.00, 113.70, 198], [ 97.10, 372.20, 436],
    [166.40, 487.20, 343], [274.70, 611.80, 431], [615.50, 632.60, 268],
    [859.30, 625.80, 266], [905.80, 582.00, 184], [903.90, 390.00, 308],
    [735.40, 294.30, 236], [617.90, 276.90, 219], [525.70, 284.60, 320],
    [533.60, 429.80, 414], [736.70, 403.60, 316], [744.00, 347.20, 291],
    [684.00, 255.40, 218], [603.40, 210.30, 200], [425.50, 182.60, 380],
    [314.40, 195.40, 330], [344.70, 296.00, 672], [429.00, 313.60, 289],
    [484.60, 312.60, 224], [562.20, 320.80, 403], [635.70, 266.30, 241],
    [648.60, 226.70, 187],
]

# TestRqaDur.py Example 2 — same xy as Example 1 of TestRqa but with
# constant 0.5 durations. (Rqa and RqaDur agree up to a constant here.)
TEST_RQADUR_CONST_XY = TEST_RQA_XY
TEST_RQADUR_CONST_D = [0.5] * len(TEST_RQA_XY)


# Deterministic 200-point random walk for stress.
def random_walk(n: int, step: float, seed: int) -> list[list[float]]:
    rng = np.random.default_rng(seed)
    steps = rng.normal(0.0, step, size=(n, 2))
    pts = np.cumsum(steps, axis=0) + np.array([500.0, 500.0])
    return pts.tolist()


# ----- helpers ---------------------------------------------------------------

def _scalar(v):
    """Coerce numpy scalars to native Python; emit NaN as None.

    Standard JSON has no NaN literal and jsonlite::fromJSON rejects the
    non-standard `NaN` Python's json module emits by default. None
    becomes `null`, which the R loader maps to NA_real_.
    """
    if isinstance(v, np.generic):
        v = v.item()
    if isinstance(v, float) and v != v:  # NaN check
        return None
    return v


def _materialise(result: dict) -> dict:
    """Flatten an Rqa/RqaDur result into JSON-safe Python types."""
    out = {}
    for k, v in result.items():
        if k == "recmat":
            if isinstance(v, list):
                out[k] = [[_scalar(x) for x in row] for row in v]
            else:
                # nan default when n <= 1
                out[k] = None
        else:
            out[k] = _scalar(v)
    return out


def _case(name: str, kind: str, fixations, *, radius, line_length=2,
          min_cluster=8, duration=None) -> dict:
    param = {"linelength": line_length, "radius": radius,
             "mincluster": min_cluster}
    if kind == "rqa":
        raw = Rqa(list(fixations), param)
    elif kind == "rqa_dur":
        raw = RqaDur(list(fixations), list(duration), param)
    else:
        raise ValueError(f"unknown kind: {kind!r}")
    return {
        "name": name,
        "kind": kind,
        "params": {
            "radius": radius,
            "line_length": line_length,
            "min_cluster": min_cluster,
        },
        "input": {
            "fixations": list(fixations),
            "duration": list(duration) if duration is not None else None,
        },
        "result": _materialise(raw),
    }


# ----- case list -------------------------------------------------------------

def build_cases() -> list[dict]:
    cases: list[dict] = []

    # Headline parity cases — the originals' own test inputs.
    cases.append(_case(
        "test_rqa_xy",        "rqa",
        TEST_RQA_XY,           radius=64,  line_length=2, min_cluster=8,
    ))
    cases.append(_case(
        "test_rqa_categories", "rqa",
        TEST_RQA_CATEGORIES,   radius=0.1, line_length=2, min_cluster=8,
    ))
    xy = [[fd[0], fd[1]] for fd in TEST_RQADUR_XYD]
    durs = [fd[2] for fd in TEST_RQADUR_XYD]
    cases.append(_case(
        "test_rqa_dur_real",   "rqa_dur",
        xy,                    radius=64,  line_length=2, min_cluster=8,
        duration=durs,
    ))
    cases.append(_case(
        "test_rqa_dur_const",  "rqa_dur",
        TEST_RQADUR_CONST_XY,  radius=64,  line_length=2, min_cluster=8,
        duration=TEST_RQADUR_CONST_D,
    ))

    # Edge cases.
    cases.append(_case("edge_n0", "rqa", [], radius=64))
    cases.append(_case("edge_n1", "rqa", [[1.0, 2.0]], radius=64))
    cases.append(_case(
        "edge_identical", "rqa",
        [[10.0, 10.0]] * 6, radius=64,
    ))
    cases.append(_case(
        "edge_no_recurrence", "rqa",
        [[i * 1000.0, 0.0] for i in range(5)], radius=10,
    ))
    cases.append(_case(
        "edge_all_recurrent", "rqa",
        [[0.0, 0.0]] * 5, radius=1,
    ))
    # Single diagonal: two identical short scans with a translation in
    # time means r_{i, i+k} = 1 for one k and otherwise 0.
    cases.append(_case(
        "edge_single_diagonal", "rqa",
        [[0.0, 0.0], [10.0, 0.0], [20.0, 0.0],
         [0.0, 0.0], [10.0, 0.0], [20.0, 0.0]],
        radius=0.5,
    ))
    # Single vertical line: one fixation that repeats four times in a
    # row, surrounded by non-recurrent points.
    cases.append(_case(
        "edge_single_vertical", "rqa",
        [[0.0, 0.0], [50.0, 50.0], [50.0, 50.0], [50.0, 50.0],
         [50.0, 50.0], [100.0, 100.0]],
        radius=1.0,
    ))

    # Stress case — fixed-seed 200-point random walk.
    cases.append(_case(
        "stress_random_walk_200", "rqa",
        random_walk(200, step=25.0, seed=12345),
        radius=64,
    ))

    return cases


# ----- main ------------------------------------------------------------------

def main() -> int:
    out_path = HERE.parent / "inst" / "extdata" / "python_fixtures.json"
    out_path.parent.mkdir(parents=True, exist_ok=True)
    cases = build_cases()
    payload = {
        "generated_by": "data-raw/make_python_fixtures.py",
        "source": "rqa_original/RqaPython (unmodified)",
        "cases": cases,
    }
    with out_path.open("w") as f:
        json.dump(payload, f, indent=2, allow_nan=True)
        f.write("\n")
    print(f"wrote {len(cases)} cases to {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
