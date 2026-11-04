
import os
import re
import csv
import glob

from datetime import datetime
from bs4 import BeautifulSoup


# ==========================================
# SETTINGS
# ==========================================

HTML_FOLDER = "html_files"

OUTPUT_FILE = "hltv_historical_rankings.csv"

ERROR_FILE = "hltv_scraping_errors.csv"


# ==========================================
# 1. SCRAPE ONE HTML FILE
# ==========================================

def scrape_hltv_html(filepath):

    print(f"\nProcessing: {filepath}")

    with open(
        filepath,
        "r",
        encoding="utf-8",
        errors="replace"
    ) as file:

        html = file.read()

    soup = BeautifulSoup(
        html,
        "html.parser"
    )

    # ======================================
    # EXTRACT DATE
    # ======================================

    page_text = soup.get_text(
        " ",
        strip=True
    )

    date_match = re.search(
        r"World ranking on "
        r"([A-Za-z]+ \d{1,2}(?:st|nd|rd|th)?, \d{4})",
        page_text
    )

    if date_match:

        date_string = re.sub(
            r"(\d+)(st|nd|rd|th)",
            r"\1",
            date_match.group(1)
        )

        ranking_date = datetime.strptime(
            date_string,
            "%B %d, %Y"
        ).strftime("%Y-%m-%d")

    else:

        # Fall back to filename
        filename = os.path.basename(filepath)

        date_match = re.search(
            r"\d{4}-\d{2}-\d{2}",
            filename
        )

        if not date_match:
            raise ValueError(
                "Could not identify ranking date."
            )

        ranking_date = date_match.group()

    print("Ranking date:", ranking_date)

    # ======================================
    # EXTRACT TEAMS
    # ======================================

    teams = []

    ranking_cards = soup.select(
        "div.ranked-team.standard-box"
    )

    print(
        "Ranking cards found:",
        len(ranking_cards)
    )

    for card in ranking_cards:

        name_element = card.select_one(
            ".ranking-header .name"
        )

        rank_element = card.select_one(
            ".position"
        )

        points_element = card.select_one(
            "span.points"
        )

        if not all([
            name_element,
            rank_element,
            points_element
        ]):
            continue

        rank_match = re.search(
            r"\d+",
            rank_element.get_text(
                " ",
                strip=True
            )
        )

        points_match = re.search(
            r"[\d,]+",
            points_element.get_text(
                " ",
                strip=True
            )
        )

        if not rank_match or not points_match:
            continue

        teams.append({

            "date": ranking_date,

            "rank": int(
                rank_match.group()
            ),

            "team": name_element.get_text(
                " ",
                strip=True
            ),

            "points": int(
                points_match.group().replace(",", "")
            )

        })

    # ======================================
    # REMOVE DUPLICATES
    # ======================================

    unique_teams = {}

    for team in teams:

        unique_teams[team["rank"]] = team

    teams = sorted(
        unique_teams.values(),
        key=lambda x: x["rank"]
    )

    if not teams:

        raise ValueError(
            "No teams found. HTML structure may have changed."
        )

    print(
        f"Successfully extracted {len(teams)} teams."
    )

    return teams


# ==========================================
# 2. PROCESS ALL HTML FILES
# ==========================================

def scrape_all_html():

    files = glob.glob(
        os.path.join(
            HTML_FOLDER,
            "*.html"
        )
    )

    files.sort()

    print("\n================================")
    print("HLTV HTML SCRAPER")
    print("================================")

    print("HTML files found:", len(files))

    all_teams = []
    errors = []

    for index, filepath in enumerate(
        files,
        start=1
    ):

        print("\n--------------------------------")
        print(
            f"File {index}/{len(files)}"
        )
        print("--------------------------------")

        try:

            teams = scrape_hltv_html(
                filepath
            )

            all_teams.extend(teams)

        except Exception as e:

            print("ERROR:", e)

            errors.append({
                "file": filepath,
                "error": str(e)
            })

    return all_teams, errors


# ==========================================
# 3. SAVE MASTER CSV
# ==========================================

def save_master_csv(teams):

    if not teams:
        print("No data to save.")
        return

    # Sort by date and rank
    teams.sort(
        key=lambda x: (
            x["date"],
            x["rank"]
        )
    )

    with open(
        OUTPUT_FILE,
        "w",
        newline="",
        encoding="utf-8-sig"
    ) as file:

        writer = csv.DictWriter(
            file,
            fieldnames=[
                "date",
                "rank",
                "team",
                "points"
            ]
        )

        writer.writeheader()

        writer.writerows(teams)

    print(
        f"\nSaved {len(teams)} records to {OUTPUT_FILE}"
    )


# ==========================================
# 4. SAVE ERRORS
# ==========================================

def save_errors(errors):

    if not errors:
        return

    with open(
        ERROR_FILE,
        "w",
        newline="",
        encoding="utf-8-sig"
    ) as file:

        writer = csv.DictWriter(
            file,
            fieldnames=[
                "file",
                "error"
            ]
        )

        writer.writeheader()

        writer.writerows(errors)

    print(
        f"Saved errors to {ERROR_FILE}"
    )


# ==========================================
# 5. MAIN
# ==========================================

def main():

    all_teams, errors = scrape_all_html()

    save_master_csv(all_teams)

    save_errors(errors)

    print("\n================================")
    print("SCRAPING COMPLETE")
    print("================================")

    print("Total team records:", len(all_teams))

    print(
        "Successful HTML files:",
        len(glob.glob(
            os.path.join(HTML_FOLDER, "*.html")
        )) - len(errors)
    )

    print("Failed HTML files:", len(errors))

    print("Output:", OUTPUT_FILE)


if __name__ == "__main__":

    main()
