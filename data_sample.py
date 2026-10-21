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

            # If demo file fails to parse, prepare to skip it
            result = parse_demo(demo_file, delete_demo=True)

            if result is None:
                continue

            sample, kills = result

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
            # Time out within 5 seconds without download starting
            async with page.expect_download(timeout=5000) as download_info:
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
 



def parse_demo(demo_file, delete_demo=False):

    try:
        # ---- 1. Parse the demo ------------------------------------------------------
        # Awpy 3.x uses direct dataset access instead of dem.parse()
        dem = Demo(demo_file)

        # awpy returns polars tables -> convert everything to pandas right away
        rounds = dem.rounds.to_pandas()
        kills = dem.kills.to_pandas()
        round_economy = dem.round_economy.to_pandas()
        item_events = dem.item_events.to_pandas()
        bomb = dem.bomb.to_pandas()

        print("Round columns:", rounds.columns.tolist())
        print("Economy columns:", round_economy.columns.tolist())
        print("Item event columns:", item_events.columns.tolist())

        # ---- 2. Starting cash and carried equipment --------------------------------
        # Snapshot players at the beginning of each round's freeze time.
        # This is intended to capture cash before the buy phase.
        start_rounds = rounds.dropna(subset=["start_tick"]).copy()
        start_ticks = sorted(start_rounds["start_tick"].astype(int).unique())

        if start_ticks:
            snapshots = dem.snapshots(ticks=start_ticks).to_pandas()

            # Match each snapshot tick to its round number
            tick_to_round = dict(
                zip(
                    start_rounds["start_tick"].astype(int),
                    start_rounds["round_num"]
                )
            )

            snapshots["round_num"] = snapshots["tick"].map(tick_to_round)

            # Standardize side names
            snapshots["side"] = snapshots["side"].replace({
                "counter-terrorist": "ct",
                "terrorist": "t",
                "CT": "ct",
                "T": "t"
            })

            # Sum starting cash and carried equipment across players
            starting_econ = (
                snapshots.groupby(["round_num", "side"])
                .agg(
                    starting_cash=("money", "sum"),
                    carried_equip=("equipment_value_round_start", "sum")
                )
                .unstack("side")
            )

            starting_econ.columns = [
                f"{side}_{col}" for col, side in starting_econ.columns
            ]
            starting_econ = starting_econ.reset_index()

        else:
            starting_econ = pd.DataFrame(columns=[
                "round_num",
                "ct_starting_cash", "t_starting_cash",
                "ct_carried_equip", "t_carried_equip"
            ])

        # ---- 3. Equipment value at freeze time ending --------------------------------
        # round_economy contains one row per round and side.
        round_economy["side"] = round_economy["side"].replace({
            "counter-terrorist": "ct",
            "terrorist": "t",
            "CT": "ct",
            "T": "t"
        })

        econ = (
            round_economy.pivot(
                index="round_num",
                columns="side",
                values="equipment_value"
            )
            .rename(columns={"ct": "ct_equip", "t": "t_equip"})
            .reset_index()
        )

        # ---- 4. Equipment purchase costs during the buy phase -----------------------
        # Item events include tracked weapon-item purchases, but not armor,
        # helmets, or defuse kits.
        purchases = item_events[
            item_events["action"] == "purchase"
        ].copy()

        purchases["side"] = purchases["side"].replace({
            "counter-terrorist": "ct",
            "terrorist": "t",
            "CT": "ct",
            "T": "t"
        })

        # Assign purchases to rounds using the round's start and freeze-end ticks.
        purchases["round_num"] = pd.NA

        for row in rounds.itertuples(index=False):
            if pd.isna(row.start_tick) or pd.isna(row.freeze_end_tick):
                continue

            mask = (
                (purchases["tick"] >= row.start_tick) &
                (purchases["tick"] <= row.freeze_end_tick)
            )
            purchases.loc[mask, "round_num"] = row.round_num

        purchases = purchases.dropna(subset=["round_num"]).copy()
        purchases["round_num"] = purchases["round_num"].astype(int)

        if not purchases.empty:
            purchase_costs = (
                purchases.groupby(["round_num", "side"])["cost"]
                .sum(min_count=1)
                .unstack("side")
                .rename(columns={
                    "ct": "ct_purchase_cost",
                    "t": "t_purchase_cost"
                })
                .reset_index()
            )
        else:
            purchase_costs = pd.DataFrame(columns=[
                "round_num", "ct_purchase_cost", "t_purchase_cost"
            ])

        # ---- 5. Determine whether the bomb was planted ------------------------------
        # A finish_plant event indicates a completed bomb plant.
        planted_rounds = set()

        if not bomb.empty and "event" in bomb.columns:
            plant_ticks = bomb.loc[
                bomb["event"] == "finish_plant", "tick"
            ]

            for row in rounds.itertuples(index=False):
                if pd.isna(row.start_tick) or pd.isna(row.end_tick):
                    continue

                if (
                    (plant_ticks >= row.start_tick) &
                    (plant_ticks <= row.end_tick)
                ).any():
                    planted_rounds.add(row.round_num)

        # ---- 6. Build the round-level sample table ---------------------------------
        sample = rounds[
            ["round_num", "winner_side", "reason_name"]
        ].rename(columns={
            "winner_side": "winner",
            "reason_name": "reason"
        })

        sample = (
            sample
            .merge(starting_econ, on="round_num", how="left")
            .merge(econ, on="round_num", how="left")
            .merge(purchase_costs, on="round_num", how="left")
        )

        sample["planted"] = sample["round_num"].isin(planted_rounds)

        # Team-side gaps: positive means CT has the larger value
        sample["equip_gap_ct"] = sample["ct_equip"] - sample["t_equip"]

        sample["starting_cash_gap_ct"] = (
            sample["ct_starting_cash"] - sample["t_starting_cash"]
        )

        sample["carried_equip_gap_ct"] = (
            sample["ct_carried_equip"] - sample["t_carried_equip"]
        )

        sample["purchase_cost_gap_ct"] = (
            sample["ct_purchase_cost"] - sample["t_purchase_cost"]
        )

        # Side labels are used here; actual team identities switch sides at halftime.
        sample["ct_team"] = "counter-terrorist"
        sample["t_team"] = "terrorist"

        sample = sample[
            [
                "round_num",
                "ct_team", "t_team",
                "winner", "reason", "planted",
                "ct_starting_cash", "t_starting_cash",
                "starting_cash_gap_ct",
                "ct_carried_equip", "t_carried_equip",
                "carried_equip_gap_ct",
                "ct_purchase_cost", "t_purchase_cost",
                "purchase_cost_gap_ct",
                "ct_equip", "t_equip", "equip_gap_ct"
            ]
        ].sort_values("round_num")

        # ---- 7. Delete Demo (optional) -----------------------------------------------
        print(f"Deleting: {demo_file}")

        if delete_demo:
            os.remove(demo_file)

        # ---- 8. Output ---------------------------------------------------------------
        return sample, kills

    except Exception as e:
        print(f"Skipping demo file: {demo_file}")
        print(f"Error: {type(e).__name__}: {e}")

        if delete_demo and os.path.exists(demo_file):
            os.remove(demo_file)

        return None





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
    for demo_id in range(112874, 112974)
]

parsed_demo_samples, parsed_demo_kills = pipeline(urls)

print(parsed_demo_samples)
print(parsed_demo_kills)