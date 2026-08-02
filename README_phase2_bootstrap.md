# Phase 2 — Bootstrap migration

This package creates the new bootstrap folder without deleting any existing R files.

## Copy location

Merge the contents into the project root:

`C:/Users/Jason/Projects/srma-agent-toolkit`

## New files

- `R/bootstrap/00_setup_project.R`
- `R/bootstrap/01_install_core_packages.R`
- `R/bootstrap/02_verify_core_environment.R`
- `R/bootstrap/03_install_advanced_packages.R`
- `R/bootstrap/04_verify_advanced_environment.R`

## Safe migration rule

Do not delete the existing root-level files yet:

- `R/00_setup(1).R`
- `R/01_install_packages.R`
- `R/02_verify_environment.R`
- `R/03_install_advanced_packages.R`
- `R/04_verify_advanced_environment.R`

First run the new bootstrap scripts and confirm that they pass.

## Recommended run order

```r
setwd("C:/Users/Jason/Projects/srma-agent-toolkit")

source("R/bootstrap/00_setup_project.R")
source("R/bootstrap/01_install_core_packages.R")
source("R/bootstrap/02_verify_core_environment.R")
```

Advanced packages are optional for initialization and screening:

```r
source("R/bootstrap/03_install_advanced_packages.R")
source("R/bootstrap/04_verify_advanced_environment.R")
```

## After verification

After both new verification scripts behave correctly, archive the old root-level
bootstrap files rather than deleting them immediately.

Suggested archive location:

`R/legacy/bootstrap_v2/`
