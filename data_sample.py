import os
from playwright.async_api import async_playwright
import asyncio
import pandas as pd
from awpy import Demo



async def download_url(url):
    output_dir = "./data/tempfiles"
    os.makedirs(output_dir, exist_ok=True)

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False)
        page = await browser.new_page()

        async with page.expect_download() as download_info:
            try:
                await page.goto(url)
            except Exception as e:
                # HLTV causes Playwright to report
                # "Download is starting" as a navigation error.
                if "Download is starting" not in str(e):
                    raise

        download = await download_info.value

        filename = download.suggested_filename
        output_path = os.path.join(output_dir, filename)

        await download.save_as(output_path)

        await browser.close()

    return output_path


def extract_rar(file):
    return 0
 

def parse_demo(demo_file):

    # ---- 1. Parse the demo ------------------------------------------------------
    dem = Demo(demo_file)
    dem.parse(player_props=["team_name", "team_num", "current_equip_value"])

    # awpy returns polars tables -> convert everything to pandas right away
    rounds = dem.rounds.to_pandas()
    kills = dem.kills.to_pandas()
    ticks = dem.ticks.to_pandas()

    # ---- 2. Team equipment value when freeze time ends -------------------------
    # Keep only players (team_num 2 = T, 3 = CT); drops coaches/spectators
    ticks = ticks[ticks["team_num"].isin([2, 3])]

    # Attach each round's freeze_end tick, keep ticks at or after it
    t = ticks.merge(rounds[["round_num", "freeze_end"]], on="round_num")
    t = t[t["tick"] >= t["freeze_end"]]

    # Snapshot = the first tick after freeze time ends in each round
    first_tick = t.groupby("round_num")["tick"].transform("min")
    snap = t[t["tick"] == first_tick].copy()

    # int64 so the CT - T gap can go negative (fixes the 4,294,961,446 bug)
    snap["equip"] = snap["current_equip_value"].astype("int64")
    snap["side"] = snap["team_num"].map({3: "ct", 2: "t"})

    # One row per round: total equipment + team name for each side
    econ = (
        snap.groupby(["round_num", "side"])
        .agg(equip=("equip", "sum"), team=("team_num", "first"))
        .unstack("side")
    )
    econ.columns = [f"{side}_{col}" for col, side in econ.columns]  # ct_equip, t_equip, ct_team, t_team
    econ = econ.reset_index()

    # ---- 3. Build the round-level sample table ---------------------------------
    sample = rounds[["round_num", "winner", "reason", "bomb_plant"]].merge(
        econ, on="round_num", how="left"
    )
    sample["equip_gap_ct"] = sample["ct_equip"] - sample["t_equip"]
    sample["planted"] = sample["bomb_plant"].notna()

    sample = sample[
        ["round_num", "ct_team", "t_team", "winner", "reason", "planted",
         "ct_equip", "t_equip", "equip_gap_ct"]
    ].sort_values("round_num")

    # ---- 4. Output --------------------------------------------------------------
    return sample, kills


# Example

#DEMO = "./data/rawdemos/darkwall-vs-krytiepacani-m2-dust2.dem"
#
#sample, kills = parse_demo(DEMO)
#
#sample.to_csv("sample_rounds.csv", index=False)
#kills.to_csv("sample_kills.csv", index=False)

import asyncio

asyncio.run(
    download_url("https://www.hltv.org/download/demo/112548")
)