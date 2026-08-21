#!/usr/bin/env python3
"""Standalone integrity and safety verifier for the public repository release."""

from __future__ import annotations

import csv
import hashlib
import json
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MANIFEST = ROOT / "FILE_MANIFEST.csv"
REQUIRED = [
    "README.md", "CHANGELOG.md", "LICENSE", "DATA_RIGHTS.md", "CITATION.cff", ".zenodo.json",
    "reproduce.sh", "software_session.md", "data_source_manifest.csv",
    "analysis_code/public_p3c_models.R", "analysis_code/public_p3c_figures.R",
    "figure_code/public_p5b2_figures.R",
    "input_data/Iodine_PublicP3A1_Australia_Incidence.csv",
    "input_data/Iodine_PublicP3A1_NewZealand_Incidence.csv",
    "input_data/Iodine_PublicP3A1_Croatia_Incidence.csv",
    "verification/verify_reproduced_results.py", "verification/REPRODUCTION_QC.md",
]
TEXT_SUFFIXES = {"", ".md", ".txt", ".csv", ".R", ".py", ".sh", ".cff", ".json"}
SECRET_PATTERNS = [
    re.compile(r"-----BEGIN [A-Z ]*PRIVATE KEY-----"),
    re.compile(r"(?i)(api[_-]?key|access[_-]?token|client[_-]?secret|password)\s*[:=]\s*[^\s,]{8,}"),
]
LOCAL_PATH = re.compile(r"/(Users|Volumes)/[^\s,\"']+")


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def main() -> int:
    failures: list[str] = []
    rows: list[dict[str, str]] = []
    for item in REQUIRED:
        if not (ROOT / item).is_file():
            failures.append(f"missing:{item}")

    if (ROOT / ".zenodo.json").is_file():
        metadata = json.loads((ROOT / ".zenodo.json").read_text(encoding="utf-8"))
        if metadata.get("version") != "1.0.0" or metadata.get("license") != "mit":
            failures.append("zenodo_metadata:version_or_license")
        if len(metadata.get("creators", [])) != 5:
            failures.append("zenodo_metadata:creator_count")

    for path in ROOT.rglob("*"):
        if path.is_symlink():
            failures.append(f"symlink:{path.relative_to(ROOT)}")
        if not path.is_file() or path == MANIFEST or path.suffix not in TEXT_SUFFIXES:
            continue
        content = path.read_text(encoding="utf-8", errors="replace")
        if LOCAL_PATH.search(content):
            failures.append(f"absolute_path:{path.relative_to(ROOT)}")
        if any(pattern.search(content) for pattern in SECRET_PATTERNS):
            failures.append(f"secret_pattern:{path.relative_to(ROOT)}")

    if MANIFEST.is_file():
        with MANIFEST.open(encoding="utf-8-sig", newline="") as handle:
            rows = list(csv.DictReader(handle))
        for row in rows:
            path = ROOT / row["path"]
            if not path.is_file():
                failures.append(f"manifest_missing:{row['path']}")
            elif str(path.stat().st_size) != row["size_bytes"] or sha256(path) != row["sha256"]:
                failures.append(f"manifest_mismatch:{row['path']}")
    else:
        failures.append("missing:FILE_MANIFEST.csv")

    if failures:
        print("REPOSITORY_RELEASE_STATUS: FAIL")
        for failure in failures:
            print(f"FAIL {failure}")
        return 1
    print("REPOSITORY_RELEASE_STATUS: PASS")
    print(f"MANIFEST_ENTRIES: {len(rows)}")
    print("SENSITIVE_OR_RESTRICTED_DATA_INCLUDED: NO")
    print("CODE_LICENSE: MIT")
    print("DATA_LICENSE_BOUNDARY_DOCUMENTED: YES")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
