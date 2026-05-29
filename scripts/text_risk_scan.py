#!/usr/bin/env python3
"""Read-only academic prose and citation risk scanner."""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path
from typing import Any


PATTERNS = [
    ("high", "unsupported_causality", r"\b(drives?|causes?|proves?|confirms?|determines?)\b", "Replace with descriptive wording unless causal method is present."),
    ("medium", "causal_noun", r"\b(causality|causal|cause-and-effect)\b", "Ensure the sentence denies or properly supports causality."),
    ("medium", "per_capita_unit", r"\bper[- ]capita\b", "Check whether the assignment intends per-person or per-1,000-person units."),
    ("medium", "vague_source", r"\b(studies show|experts believe|research shows|data shows)\b", "Name the source or remove the vague attribution."),
    ("medium", "ai_like_phrase", r"\b(comprehensive|robust|seamless|crucial|pivotal|delve|landscape|framework developed here)\b", "Use shorter, concrete wording."),
    ("medium", "overclaim", r"\b(most actionable|clear evidence|strongly demonstrates|undeniably|automatically)\b", "Add evidence or weaken the claim."),
    ("low", "missing_access_date_hint", r"\b(World Bank|IMF|United Nations|Kaggle|OECD|World Development Indicators)\b", "Confirm this source has a reference entry and access date."),
    ("low", "threshold_language", r"\b(threshold|inflection point|cutoff)\b", "Ensure threshold claims are supported by actual threshold analysis."),
]


def scan_file(path: Path) -> list[dict[str, Any]]:
    text = path.read_text(encoding="utf-8", errors="replace")
    findings: list[dict[str, Any]] = []
    for i, line in enumerate(text.splitlines(), start=1):
        for level, kind, pattern, fix in PATTERNS:
            if re.search(pattern, line, re.I):
                findings.append({
                    "risk_level": level,
                    "kind": kind,
                    "file": str(path),
                    "line": i,
                    "text": line.strip()[:240],
                    "suggested_fix": fix,
                })
    return findings


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--files", nargs="+", required=True, help="Files to scan")
    parser.add_argument("--json", action="store_true", help="Emit JSON")
    args = parser.parse_args()

    findings: list[dict[str, Any]] = []
    missing: list[str] = []
    for raw in args.files:
        path = Path(raw)
        if not path.exists():
            missing.append(raw)
            continue
        findings.extend(scan_file(path))

    high = sum(1 for f in findings if f["risk_level"] == "high")
    medium = sum(1 for f in findings if f["risk_level"] == "medium")
    status = "warning" if findings or missing else "success"
    result = {
        "status": status,
        "summary": f"Text risk scan completed with {high} high and {medium} medium findings.",
        "findings": findings,
        "missing_files": missing,
        "next_actions": [
            "Review high-risk causal language first.",
            "Check citation/access-date hints against the references section.",
            "Do not mechanically replace terms; verify the evidence behind each sentence.",
        ],
    }

    if args.json:
        print(json.dumps(result, indent=2, ensure_ascii=False))
    else:
        print(result["summary"])
        for f in findings:
            print(f"- [{f['risk_level']}] {f['file']}:{f['line']} {f['kind']}: {f['text']}")
        for m in missing:
            print(f"- [error] Missing file: {m}")
    return 0 if status == "success" else 1


if __name__ == "__main__":
    raise SystemExit(main())
