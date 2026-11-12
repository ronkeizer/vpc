# Generate rcVPC expected values with Pharmpy's PsN refcorr implementation.
import sys
import numpy as np
import pandas as pd
from pharmpy.tools.vpc.psn_vpc import calculate_reference_correction
import pharmpy

indir, outdir = sys.argv[1], sys.argv[2]
obs = pd.read_csv(f"{indir}/obs.csv")
sim = pd.read_csv(f"{indir}/sim.csv")
ref = pd.read_csv(f"{indir}/sim_ref.csv")

def split(df):
    return [g.reset_index(drop=True)[["dv"]].rename(columns={"dv": "DV"})
            for _, g in df.groupby("sim", sort=True)]

rows = []
for case, logdv, lb in [("normal_lb0", False, 0.0), ("normal_lbneg", False, -0.5), ("log", True, 0.0)]:
    f = np.log if logdv else (lambda x: x)
    dv = pd.Series(f(obs["dv"].values), name="DV")
    pred = pd.Series(f(obs["pred"].values))
    predref = pd.Series(f(obs["predref"].values))
    simdata = [d.assign(DV=f(d["DV"])) for d in split(sim)]
    refdata = [d.assign(DV=f(d["DV"])) for d in split(ref)]
    rc_obs, rc_sim = calculate_reference_correction(dv, pred, predref, simdata, refdata, logdv, lb)
    for i, v in enumerate(np.asarray(rc_obs)):
        rows.append((case, 0, i + 1, float(v)))
    for s, ser in enumerate(rc_sim):
        for i, v in enumerate(np.asarray(ser)):
            rows.append((case, s + 1, i + 1, float(v)))

out = pd.DataFrame(rows, columns=["case", "sim", "rec", "rc_dv"])
out.to_csv(f"{outdir}/expected_pharmpy.csv", index=False, float_format="%.12g")
print("pharmpy", pharmpy.__version__, out.shape)
