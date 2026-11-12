# rcVPC test fixtures

Anchor values for the reference-corrected VPC (issue #105).

- `make_inputs.R` generates `obs.csv`, `sim.csv` and `sim_ref.csv`, which hold synthetic one-compartment PK data. The observed design uses varying dose and weight. The reference design uses dose 100 and WT 70 for everyone.
- `pharmpy_expected.py` computes `expected_pharmpy.csv` using Pharmpy's PsN rcVPC implementation, `pharmpy.tools.vpc.psn_vpc.calculate_reference_correction` (pharmpy-core 1.8.0). It covers three cases: normal scale with lb = 0, normal scale with lb = -0.5, and log scale.

To regenerate:

    Rscript make_inputs.R .
    docker run --rm -v $PWD:/in:ro -v $PWD:/out --entrypoint /app/venv/bin/python \
      pmx:latest -I /in/pharmpy_expected.py /in /out

In `expected_pharmpy.csv`, `sim = 0` marks the observed data.
