# Extract the frequency tables from all NEPS codebooks in this folder into CSV files.
# Run in PowerShell from this folder:   .\run_all.ps1
# (if scripts are blocked:  powershell -ExecutionPolicy Bypass -File .\run_all.ps1)
# Already finished codebooks are skipped, so the script can simply be started again.

Set-Location $PSScriptRoot

# 1. Python available?
$py = Get-Command python -ErrorAction SilentlyContinue
if (-not $py) { $py = Get-Command py -ErrorAction SilentlyContinue }
if (-not $py) { Write-Host "Python not found. Please install it (python.org or Software Center) and run again."; exit 1 }
$python = $py.Source
& $python --version

# 2. pdfplumber available? Otherwise install it for this user.
& $python -c "import pdfplumber" 2>$null
if ($LASTEXITCODE -ne 0) {
    Write-Host "Installing pdfplumber ..."
    & $python -m pip install --user pdfplumber
}

# 3. All codebooks: file, cohort, SUF version, language
$jobs = @(
    @("NEPS_SC1_Codebook_12-0-0_de.pdf", "SC1", "12-0-0", "de"),
    @("NEPS_SC1_Codebook_12-0-0_en.pdf", "SC1", "12-0-0", "en"),
    @("NEPS_SC2_Codebook_11-0-0_de.pdf", "SC2", "11-0-0", "de"),
    @("NEPS_SC2_Codebook_11-0-0_en.pdf", "SC2", "11-0-0", "en"),
    @("NEPS_SC3_Codebook_13-1-0_de.pdf", "SC3", "13-1-0", "de"),
    @("NEPS_SC3_Codebook_13-1-0_en.pdf", "SC3", "13-1-0", "en"),
    @("NEPS_SC4_Codebook_16-0-0_de.pdf", "SC4", "16-0-0", "de"),
    @("NEPS_SC4_Codebook_16-0-0_en.pdf", "SC4", "16-0-0", "en"),
    @("NEPS_SC5_Codebook_19-0-0_de.pdf", "SC5", "19-0-0", "de"),
    @("NEPS_SC5_Codebook_19-0-0_en.pdf", "SC5", "19-0-0", "en"),
    @("NEPS_SC6_Codebook_17-0-0_de.pdf", "SC6", "17-0-0", "de"),
    @("NEPS_SC6_Codebook_17-0-0_en.pdf", "SC6", "17-0-0", "en"),
    @("NEPS_SC8_Codebook_2-0-0_de.pdf",  "SC8", "2-0-0",  "de"),
    @("NEPS_SC8_Codebook_2-0-0_en.pdf",  "SC8", "2-0-0",  "en")
)

New-Item -ItemType Directory -Force -Path "output" | Out-Null
foreach ($j in $jobs) {
    $done = "output\codebook_vars_$($j[1])_$($j[2])_$($j[3]).csv"
    if (Test-Path $done) { Write-Host "skip $($j[0]) (already done)"; continue }
    Write-Host "`n=== $($j[0]) ($(Get-Date -Format HH:mm)) ==="
    & $python extract_codebook.py $j[0] $j[1] $j[2] output $j[3]
}
Write-Host "`nAll done ($(Get-Date -Format HH:mm)). Results in the folder 'output'."
