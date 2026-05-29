#!/usr/bin/env python3
"""Read-only artifact audit for ETW2001 Assignment 2."""

from __future__ import annotations

import argparse
import json
import re
import subprocess
from pathlib import Path
from typing import Any


DEFAULT_HEADINGS = [
    "Page 1",
    "Page 2",
    "Page 3",
    "Page 4",
    "Page 5",
    "Page 6",
    "Page 7",
    "Page 8",
    "Page 9",
    "Page 10",
    "Page 11",
]


def run_text(cmd: list[str]) -> tuple[int, str, str]:
    try:
        proc = subprocess.run(cmd, text=True, capture_output=True, check=False)
        return proc.returncode, proc.stdout, proc.stderr
    except FileNotFoundError as exc:
        return 127, "", str(exc)


def pdf_pages(path: Path) -> dict[str, Any]:
    if not path.exists():
        return {"status": "missing", "pages": None, "error": "PDF not found"}
    code, out, err = run_text(["pdfinfo", str(path)])
    if code != 0:
        return {"status": "error", "pages": None, "error": err.strip() or out.strip()}
    match = re.search(r"^Pages:\s+(\d+)", out, re.MULTILINE)
    return {
        "status": "success" if match else "warning",
        "pages": int(match.group(1)) if match else None,
        "error": "" if match else "Could not parse page count",
    }


def pdf_text(path: Path) -> str:
    if not path.exists():
        return ""
    code, out, _ = run_text(["pdftotext", str(path), "-"])
    return out if code == 0 else ""


def find_external_datasets(data_dir: Path) -> list[str]:
    if not data_dir.exists():
        return []
    files = []
    for p in sorted(data_dir.iterdir()):
        if p.suffix.lower() not in {".csv", ".xlsx", ".xls", ".json", ".rds"}:
            continue
        name = p.name.lower()
        if name.startswith("superstore"):
            continue
        files.append(str(p))
    return files


def audit(root: Path) -> dict[str, Any]:
    app = root / "app.R"
    data_dir = root / "data"
    report_qmd = root / "report" / "report.qmd"
    report_pdf = root / "report" / "report.pdf"
    screenshot = root / "report" / "dashboard_screenshot.png"
    brief = root / "ETW2001 - Assignment 2.md"

    text = pdf_text(report_pdf)
    page_info = pdf_pages(report_pdf)
    external = find_external_datasets(data_dir)
    headings = {h: (h in text) for h in DEFAULT_HEADINGS}

    risks: list[dict[str, Any]] = []
    if page_info.get("pages") != 11:
        risks.append({
            "risk_level": "high",
            "summary": "Final PDF is not 11 physical pages under conservative interpretation.",
            "evidence": page_info,
            "suggested_fix": "Make the submitted PDF exactly 11 pages and avoid a separate cover page unless it is explicitly excluded from the limit.",
        })
    if len(external) < 3:
        risks.append({
            "risk_level": "high",
            "summary": "Fewer than three external dataset files detected.",
            "evidence": external,
            "suggested_fix": "Discover and add three credible external datasets with provenance.",
        })
    for heading, present in headings.items():
        if not present:
            risks.append({
                "risk_level": "medium",
                "summary": f"Expected report heading not detected: {heading}",
                "evidence": {"heading": heading, "present": present},
                "suggested_fix": "Check PDF text and QMD page headings.",
            })

    artifacts = {
        "brief": {"path": str(brief), "exists": brief.exists()},
        "app": {"path": str(app), "exists": app.exists()},
        "data_dir": {"path": str(data_dir), "exists": data_dir.exists()},
        "external_datasets": external,
        "report_qmd": {"path": str(report_qmd), "exists": report_qmd.exists()},
        "report_pdf": {"path": str(report_pdf), "exists": report_pdf.exists(), **page_info},
        "dashboard_screenshot": {"path": str(screenshot), "exists": screenshot.exists()},
        "headings": headings,
        "ai_acknowledgement_detected": bool(re.search(r"\bAI\b|Claude|Codex|ChatGPT|OpenAI", text, re.I)),
        "references_detected": "References" in text or "Citations" in text,
    }

    return {
        "status": "warning" if risks else "success",
        "summary": "Artifact audit completed.",
        "artifacts": artifacts,
        "failures": risks,
        "next_actions": [
            "Resolve high-risk failures first.",
            "Run text_risk_scan.py for prose/citation risks.",
            "Run manual dashboard verification after UI changes.",
        ],
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=".", help="Assignment root directory")
    parser.add_argument("--json", action="store_true", help="Emit JSON")
    args = parser.parse_args()

    result = audit(Path(args.root).resolve())
    if args.json:
        print(json.dumps(result, indent=2, ensure_ascii=False))
    else:
        print(result["summary"])
        for failure in result["failures"]:
            print(f"- [{failure['risk_level']}] {failure['summary']}")
    return 0 if result["status"] == "success" else 1


if __name__ == "__main__":
    raise SystemExit(main())
