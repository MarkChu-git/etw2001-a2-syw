#!/usr/bin/env python3
"""Create or score an external dataset discovery matrix."""

from __future__ import annotations

import argparse
import csv
import json
import sys
from pathlib import Path
from typing import Any


FIELDS = [
    "name",
    "source",
    "url",
    "domain",
    "relevance",
    "credibility",
    "compatibility",
    "joinability",
    "variables_units",
    "citation_quality",
    "cleaning_burden",
    "limitation_risk",
    "join_key",
    "years",
    "unit",
    "notes",
]

SCORE_FIELDS = [
    "relevance",
    "credibility",
    "compatibility",
    "joinability",
    "variables_units",
    "citation_quality",
    "cleaning_burden",
    "limitation_risk",
]


def emit_template() -> None:
    writer = csv.DictWriter(sys.stdout, fieldnames=FIELDS)
    writer.writeheader()
    writer.writerow({
        "name": "Example candidate",
        "source": "Official source",
        "url": "https://example.org/data",
        "domain": "macroeconomic context",
        "relevance": 3,
        "credibility": 3,
        "compatibility": 2,
        "joinability": 2,
        "variables_units": 2,
        "citation_quality": 2,
        "cleaning_burden": 2,
        "limitation_risk": 2,
        "join_key": "Country + Year",
        "years": "2011-2014",
        "unit": "documented unit",
        "notes": "Replace this row with real candidates.",
    })


def load_rows(path: Path) -> list[dict[str, Any]]:
    with path.open(newline="", encoding="utf-8-sig") as fh:
        return list(csv.DictReader(fh))


def score_rows(rows: list[dict[str, Any]]) -> list[dict[str, Any]]:
    scored = []
    for row in rows:
        total = 0
        zeros = []
        for field in SCORE_FIELDS:
            try:
                score = int(str(row.get(field, "")).strip() or "0")
            except ValueError:
                score = 0
            score = max(0, min(score, 3))
            total += score
            if score == 0:
                zeros.append(field)
        required_zero = [f for f in zeros if f in {"credibility", "citation_quality", "joinability"}]
        recommendation = "select" if total >= 18 and not required_zero else "reject"
        scored.append({**row, "total_score": total, "zero_fields": zeros, "recommendation": recommendation})
    return sorted(scored, key=lambda r: int(r["total_score"]), reverse=True)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--template", action="store_true", help="Print CSV template")
    parser.add_argument("--input", help="CSV candidate matrix to score")
    parser.add_argument("--json", action="store_true", help="Emit JSON")
    args = parser.parse_args()

    if args.template:
        emit_template()
        return 0

    if not args.input:
        parser.error("Use --template or --input <csv>")

    rows = score_rows(load_rows(Path(args.input)))
    result = {
        "status": "success",
        "summary": f"Scored {len(rows)} dataset candidates.",
        "candidates": rows,
        "selected": [r for r in rows if r["recommendation"] == "select"][:3],
        "next_actions": [
            "Review selected candidates for fit with the research question.",
            "Record rejected candidates and reasons.",
            "Create provenance rows for the final three datasets before implementation.",
        ],
    }

    if args.json:
        print(json.dumps(result, indent=2, ensure_ascii=False))
    else:
        for row in rows:
            print(f"{row.get('total_score'):>2} {row.get('recommendation'):>6} {row.get('name')} — {row.get('source')}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
