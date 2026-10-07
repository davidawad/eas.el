#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Regenerate the Vega gallery verdicts from the templates' x-eas.vega blocks.

Rewrites test/vega-examples/manifest.json (each example's status, and its
note as the reason when it is not a pass) and, in
docs/design/vega-gallery-coverage.md, the category table and the Partial
list.  The rest of the document is kept as it is.

Usage: scripts/eas-vega-coverage.py  (from anywhere; paths hang off the root)
"""

import json
import pathlib
import re

ROOT = pathlib.Path(__file__).resolve().parent.parent
MANIFEST = ROOT / "test" / "vega-examples" / "manifest.json"
DOC = ROOT / "docs" / "design" / "vega-gallery-coverage.md"


def verdict(name):
    """The x-eas.vega block of template NAME (templates/vega/NAME.json)."""
    path = ROOT / "templates" / "vega" / f"{name}.json"
    return json.loads(path.read_text(encoding="utf-8"))["x-eas"]["vega"]


def manifest():
    """Rewrite manifest.json; return its examples."""
    data = json.loads(MANIFEST.read_text(encoding="utf-8"))
    for e in data["examples"]:
        v = verdict(e["template"])
        e["status"] = v["status"]
        e.pop("reason", None)
        if v["status"] != "pass":
            e["reason"] = v["note"]
    MANIFEST.write_text(json.dumps(data, indent=1, ensure_ascii=False) + "\n", encoding="utf-8")
    return data["examples"]


def table(examples):
    """The category table, in the manifest's category order."""
    cats = []
    for e in examples:
        if e["category"] not in cats:
            cats.append(e["category"])
    rows = ["| category | pass | partial | examples |", "|---|--:|--:|--:|"]
    tp = tq = 0
    for c in cats:
        es = [e for e in examples if e["category"] == c]
        p = sum(e["status"] == "pass" for e in es)
        q = sum(e["status"] == "partial" for e in es)
        tp, tq = tp + p, tq + q
        rows.append(f"| {c} | {p} | {q} | {len(es)} |")
    unsupported = sum(e["status"] == "unsupported" for e in examples)
    assert unsupported == 0, "add an unsupported column to the table"
    rows.append(f"| **all** | **{tp}** | **{tq}** | **{len(examples)}** |")
    return "\n".join(rows)


def partials(examples):
    """The Partial section's list."""
    return "\n".join(f"- **{e['name']}** ({e['category']}): {e['reason']}"
                     for e in examples if e["status"] == "partial")


def doc(examples):
    """Rewrite the table and the Partial list of the coverage document."""
    text = DOC.read_text(encoding="utf-8")
    text = re.sub(r"\| category \| pass \| partial \| examples \|\n(?:\|.*\|\n)+",
                  table(examples) + "\n", text, count=1)
    text = re.sub(r"(## Partial\n\n)(?:- \*\*.*\n)+", lambda m: m.group(1) + partials(examples) + "\n",
                  text, count=1)
    DOC.write_text(text, encoding="utf-8")


if __name__ == "__main__":
    doc(manifest())
