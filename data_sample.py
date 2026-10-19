import os
import re
from playwright.async_api import async_playwright
import asyncio
import rarfile
import subprocess
import pandas as pd
from awpy import Demo



def pipeline(urls):

    parsed_demo_samples = []
    parsed_demo_kills = []

    for url in urls:
        # Get HLTV demo ID from the URL
        demo_id = re.search(r"/demo/(\d+)", url).group(1)

        rar_file = asyncio.run(download_url(url))

        # Skip URLs that return a 404 or fail to download
        if rar_file is None:
            continue

        demo_files = extract_rar(rar_file)

        # Skip if no demo files were extracted
        if not demo_files:
            print(f"No demo files found for {url}; skipping.")
            continue

        for demo_file in demo_files:
            # Get the map name
            filename = os.path.basename(demo_file)

            match = re.search(r"-m\d+-(.+)\.dem$", filename)

            if match:
                map_name = match.group(1)
            else: # There was only one map played
                map_name = re.search(r"-([^-]+)\.dem$", filename).group(1)

            sample, kills = parse_demo(demo_file,
                                       delete_demo = True
                                       )
            # Add identifiers as the first two columns
            for df in (sample, kills):
                df.insert(0, "map_name", map_name)
                df.insert(0, "file_name", filename)
                df.insert(0, "file_id", demo_id)

            parsed_demo_samples.append(sample)
            parsed_demo_kills.append(kills)

    # Combine all demos into two data frames
    full_samples = pd.concat(parsed_demo_samples, ignore_index=True)
    full_kills = pd.concat(parsed_demo_kills, ignore_index=True)

    # Save csv files in the data folder
    os.makedirs("./data", exist_ok = True)

    full_samples.to_csv("./data/sample_rounds.csv", index = False)
    full_kills.to_csv("./data/sample_kills.csv", index = False)

    return full_samples, full_kills


async def download_url(url):
    output_dir = "./data/tempfiles"
    os.makedirs(output_dir, exist_ok=True)

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False)
        page = await browser.new_page()

        try:
            async with page.expect_download(timeout=10000) as download_info:
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

            return output_path

        except Exception as e:
            print(f"No download detected within 10 seconds or download failed: {url}")
            print(f"{type(e).__name__}: {e}")
            return None

        finally:
            await browser.close()




def extract_rar(rar_path):

    output_dir = os.path.dirname(rar_path)

    result = subprocess.run(
        [
            r"C:\Program Files\7-Zip\7z.exe",
            "e",
            rar_path,
            f"-o{output_dir}",
            "*.dem",
            "-y"
        ],
        capture_output=True,
        text=True,
        check=True
    )

    print(result.stdout)

    demo_files = [
        os.path.join(output_dir, filename)
        for filename in os.listdir(output_dir)
        if filename.lower().endswith(".dem")
    ]

    os.remove(rar_path)

    print(demo_files)

    return demo_files
 

def parse_demo(demo_file, delete_demo = False):

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

    # ---- 4. Delete Final (optional) ---------------------------------------------
    print(f"Deleting: {demo_file}")
    
    if delete_demo == True:
        os.remove(demo_file)

    # ---- 5. Output --------------------------------------------------------------
    return sample, kills



# Example

#DEMO = "./data/rawdemos/darkwall-vs-krytiepacani-m2-dust2.dem"
#
#sample, kills = parse_demo(DEMO)
#
#sample.to_csv("sample_rounds.csv", index=False)
#kills.to_csv("sample_kills.csv", index=False)

#asyncio.run(
#    download_url("https://www.hltv.org/download/demo/112548")
#)

base_url = "https://www.hltv.org/download/demo/"

urls = [
    f"{base_url}{demo_id}"
    for demo_id in range(112974, 112975)
]

parsed_demo_samples, parsed_demo_kills = pipeline(urls)

print(parsed_demo_samples)
print(parsed_demo_kills)
