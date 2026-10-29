# Codebook extraction (step 1 of the codebook data)

These scripts read the frequency tables from the official NEPS codebook PDFs
(neps-data.de, CC BY 4.0) and write two CSV files per starting cohort and language:

- `codebook_vars_<SC>_<version>_<lang>.csv`: one row per variable
- `codebook_freq_<SC>_<version>_<lang>.csv`: one row per value of a variable

Step 2 is `dev/build_codebook_data.R`, which turns the CSV files into the `.rds` files
in `inst/extdata/codebook` that the app reads.

## How to run

1. Copy `extract_codebook.py` and `run_all.ps1` into a working folder outside the
   repository (so far `C:\work_cowork\codebook_extract`; backup of that folder on M:).
2. Put the codebook PDFs into the same folder and list them in `$jobs` in `run_all.ps1`
   (file, cohort, SUF version, language).
3. Run `.\run_all.ps1` in PowerShell from that folder. It installs `pdfplumber` if needed
   and skips codebooks that are already done. The CSV files are written to `output/`.
4. Set `csv_dir` in `dev/build_codebook_data.R` to that `output/` folder and run it from the
   package root.

The PDFs and CSV files are not part of the repository (large, and they can be recreated
from the public codebooks).
