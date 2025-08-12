# Code to scrape ICD-10 code groups (for example, M30-M36) from https://www.icd10data.com/ICD10CM/Codes and save them to a file.

import re
import requests
from bs4 import BeautifulSoup

# Configure headers to mimic a browser
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    "Accept-Language": "en-US,en;q=0.9",
}


def scrape_icd10_codes():
    base_url = "https://www.icd10data.com/ICD10CM/Codes"
    # Pattern matches level 2 codes (for example, M30-M36).
    href_pattern = re.compile(r"/[A-Z0-9]{3}-[A-Z0-9]{3}/[A-Z0-9]{3}-[A-Z0-9]{3}")

    print(f"Fetching ICD-10 codes from: {base_url}")

    try:
        response = requests.get(base_url, headers=HEADERS)
        response.raise_for_status()

        soup = BeautifulSoup(response.text, "html.parser")
        code_texts = []

        for a in soup.find_all("a", href=href_pattern):
            text = a.text.strip()
            if text:
                code_texts.append(text)

        # Remove duplicates while preserving order
        seen = set()
        unique_code_texts = [x for x in code_texts if not (x in seen or seen.add(x))]

        return unique_code_texts

    except requests.RequestException as e:
        print(f"Error fetching data: {e}")
        return []
    except Exception as e:
        print(f"An error occurred: {e}")
        return []


def save_results(codes):
    filename = "ICD10_code_ranges.txt"
    with open(filename, "w", encoding="utf-8") as f:
        for code in codes:
            f.write(f"{code}\n")
    print(f"Saved {len(codes)} codes to {filename}")


if __name__ == "__main__":
    print("Starting ICD-10 code extraction...")
    icd10_codes = scrape_icd10_codes()

    if icd10_codes:
        save_results(icd10_codes)
        print("Extraction completed successfully!")
        print(f"Sample codes: {icd10_codes[:5]}...")
    else:
        print("No codes were extracted.")
