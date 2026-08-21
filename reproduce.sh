#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
output_dir="$repo_root/reproduced_output"

if [[ -e "$output_dir" ]]; then
  echo "STOP: $output_dir already exists; move it aside before rerunning." >&2
  exit 2
fi

for command_name in Rscript pdftocairo; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    echo "STOP: required command not found: $command_name" >&2
    exit 2
  fi
done

if ! Rscript -e 'stopifnot(requireNamespace("nlme", quietly=TRUE), requireNamespace("ggplot2", quietly=TRUE), requireNamespace("patchwork", quietly=TRUE))'; then
  echo "STOP: required R packages are missing: nlme, ggplot2, patchwork" >&2
  exit 2
fi

task_work_dir=$(mktemp -d "${TMPDIR:-/tmp}/iodine-its-reproduction.XXXXXX")
cleanup() {
  rm -rf "$task_work_dir"
}
trap cleanup EXIT

mkdir -p "$task_work_dir/analysis_code" "$task_work_dir/figure_code"
cp "$repo_root"/analysis_code/*.R "$task_work_dir/analysis_code/"
cp "$repo_root"/figure_code/*.R "$task_work_dir/figure_code/"
cp "$repo_root"/input_data/*.csv "$task_work_dir/"

Rscript "$task_work_dir/analysis_code/public_p3c_models.R"
Rscript "$task_work_dir/analysis_code/public_p3c_figures.R"
Rscript "$task_work_dir/figure_code/public_p5b2_figures.R"

mkdir -p "$output_dir/analysis" "$output_dir/figures"
find "$task_work_dir" -maxdepth 1 -type f \( -name 'Iodine_PublicP3C_*.csv' -o -name 'Iodine_PublicP3C_*.md' \) -exec cp {} "$output_dir/analysis/" \;
find "$task_work_dir" -maxdepth 1 -type f \( -name 'Iodine_PublicP3C_Figure*.pdf' -o -name 'Iodine_PublicP3C_Figure*.png' -o -name 'Iodine_PublicP3C_Figure*.svg' -o -name 'Iodine_PublicP3C_Figure*.tiff' \) -exec cp {} "$output_dir/figures/" \;
if [[ -d "$task_work_dir/Iodine_PublicP3C_Figures" ]]; then
  cp -R "$task_work_dir/Iodine_PublicP3C_Figures/." "$output_dir/figures/"
fi
if [[ -d "$task_work_dir/figures" ]]; then
  cp -R "$task_work_dir/figures/." "$output_dir/figures/"
fi

python3 "$repo_root/verification/verify_reproduced_results.py" "$output_dir"
echo "REPRODUCTION_STATUS: PASS"
echo "OUTPUT_DIRECTORY: $output_dir"
