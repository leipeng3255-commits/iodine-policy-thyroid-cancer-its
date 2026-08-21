# Iodine-policy transitions and thyroid-cancer incidence

Reproducibility materials for the manuscript **“Iodine-policy transitions and
thyroid-cancer incidence: country-specific interrupted time-series analyses in
Australia, New Zealand, and Croatia.”**

## Scientific scope

This repository reproduces country-specific ecological interrupted time-series
analyses around documented population iodine-policy transitions. Policy timing,
population urinary-iodine evidence, and registry outcomes are separate evidence
layers. The analyses estimate temporal associations; they do not estimate an
individual iodine dose, a treatment effect, or a causal effect of fortification.
No pooled cross-country effect is calculated.

## Repository and citation ownership

- Repository custodian: GitHub account `leipeng3255-commits`, confirmed by the
  authenticated GitHub owner selection used to create this deposit.
- Repository: `https://github.com/leipeng3255-commits/iodine-policy-thyroid-cancer-its`.
- Archived release: [`v1.0.0`](https://github.com/leipeng3255-commits/iodine-policy-thyroid-cancer-its/releases/tag/v1.0.0).
- Manuscript and software creators are listed in `CITATION.cff` and
  `.zenodo.json`. GitHub account ownership does not change authorship.

## Contents

- `analysis_code/`: locked country-specific ITS and robustness workflow.
- `figure_code/`: final publication-figure workflow.
- `input_data/`: frozen public aggregate analytic inputs required by the code.
- `derived_aggregate_data/`: source data for manuscript figures and tables.
- `metadata/`: source-level provenance, URLs, versions, access dates, and hashes.
- `verification/`: standalone release-integrity checks.
- `reproduce.sh`: clean temporary-directory reproduction entry point.
- `DATA_RIGHTS.md`: file-level licensing and redistribution boundaries.
- `software_session.md`: locked software versions.

No participant-level, identifiable, restricted-access, or third-party raw
workbook/PDF/JSON files are included.

## Reproduction

Requirements:

- R 4.5 or later;
- R packages `nlme`, `ggplot2`, and `patchwork`;
- Poppler command-line utility `pdftocairo` for vector and TIFF exports;
- Python 3.10 or later for the standalone release verifier.

From a clean clone:

```bash
python3 verification/verify_release.py
bash reproduce.sh
```

`reproduce.sh` copies the frozen inputs and scripts into a new temporary
directory, reruns the country-specific models and figure workflows, and writes
new artefacts to `reproduced_output/`. The model workflow stops if the three
primary slope-change estimates fail to reproduce the locked estimates within
the prespecified numerical tolerance. Existing outputs are never silently
overwritten.

## Data provenance and reuse

All analytical inputs are aggregate public-health statistics or author-created
transformations of such statistics. The repository-level MIT License applies
to original code only. Data and derived outputs retain source-specific terms;
see `DATA_RIGHTS.md` and `metadata/` before reuse.

Principal sources include the Australian Institute of Health and Welfare,
Health New Zealand / Te Whatu Ora, the Croatian Institute of Public Health,
the WHO Mortality Database, IARC Cancer Over Time/CI5plus, WHO micronutrient
sources, FSANZ, and the Croatian official gazette. Source organisations do not
endorse this analysis.

## Citation

Use the software citation exposed by GitHub from `CITATION.cff`. The immutable
version-of-record is archived at Zenodo under
[`10.5281/zenodo.22040562`](https://doi.org/10.5281/zenodo.22040562). The concept
DOI for all repository versions is
[`10.5281/zenodo.22040561`](https://doi.org/10.5281/zenodo.22040561).

## Release status

Scientific content is frozen. Public release `v1.0.0` and its Zenodo archive
were completed on 21 August 2026.
