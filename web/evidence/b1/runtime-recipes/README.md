# Historical execution recipes

These are the exact workspace-specific scripts used for the B1 runtime recovery/verification, retained for audit. Absolute paths identify this execution environment; they are not a portable installer. Downloaded source archives and runtime binaries are not included. Normal deployments should install the lock-specified R version and the operating-system development dependencies, then run the supported setup path.

The package archive repairs use exact lock versions and fresh libraries. They do not copy previously installed Block A R packages. `verify_b1_clean.py` records the clean test commands. Later focused regression tests are listed in the PM report. The DOCX maintenance script runs the unchanged prescribed utility in an isolated scientific source copy.
