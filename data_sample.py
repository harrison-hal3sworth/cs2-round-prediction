from awpy import Demo
import pandas as pd

DEMO = "./rawdemos/spirit-vs-m80-m1-dust2.dem"

# ---- 1. Parse the demo ------------------------------------------------------
dem = Demo(DEMO)
dem.parse(player_props=["team_num", "current_equip_value"])

# awpy returns polars tables -> convert everything to pandas right away
rounds = dem.rounds.to_pandas()
kills = dem.kills.to_pandas()
ticks = dem.ticks.to_pandas()

# ---- 2. Team identity -------------------------------------------------------
# Keep only players (team_num 2 = T, 3 = CT); drops coaches/spectators
ticks = ticks[ticks["team_num"].isin([2, 3])].copy()

# Team A = players who were CT in the first round (players never switch teams)
first = ticks[ticks["round_num"] == ticks["round_num"].min()]
start_ct = set(first.loc[first["team_num"] == 3, "steamid"])
ticks["team"] = ticks["steamid"].isin(start_ct).map({True: "A_start_ct", False: "B_start_t"})

# Print rosters so real team names can be attached later if needed
print("Rosters (3 = started CT, 2 = started T):")
print(first.groupby("team_num")["name"].unique(), "\n")

# ---- 3. Team equipment value when freeze time ends -------------------------
# Attach each round's freeze_end tick, keep ticks at or after it
t = ticks.merge(rounds[["round_num", "freeze_end"]], on="round_num")
t = t[t["tick"] >= t["freeze_end"]]

# Snapshot = the first tick after freeze time ends in each round
first_tick = t.groupby("round_num")["tick"].transform("min")
snap = t[t["tick"] == first_tick].copy()

# int64 so the CT - T gap can go negative (fixes the 4,294,961,446 bug)
snap["equip"] = snap["current_equip_value"].astype("int64")
snap["side"] = snap["team_num"].map({3: "ct", 2: "t"})

# One row per round: total equipment + team label for each side
econ = (
    snap.groupby(["round_num", "side"])
    .agg(equip=("equip", "sum"), team=("team", "first"))
    .unstack("side")
)
econ.columns = [f"{side}_{col}" for col, side in econ.columns]  # ct_equip, t_equip, ct_team, t_team
econ = econ.reset_index()

# ---- 4. Build the round-level sample table ---------------------------------
sample = rounds[["round_num", "winner", "reason", "bomb_plant"]].merge(
    econ, on="round_num", how="left"
)
sample["equip_gap_ct"] = sample["ct_equip"] - sample["t_equip"]
sample["planted"] = sample["bomb_plant"].notna()

sample = sample[
    ["round_num", "ct_team", "t_team", "winner", "reason", "planted",
     "ct_equip", "t_equip", "equip_gap_ct"]
].sort_values("round_num")

# ---- 5. Output --------------------------------------------------------------
print(sample.head(15).to_string(index=False))
print(f"\n{len(rounds)} rounds, {len(kills)} kills parsed")

sample.to_csv("sample_rounds.csv", index=False)
kills.to_csv("sample_kills.csv", index=False)