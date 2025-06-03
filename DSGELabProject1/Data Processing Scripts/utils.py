from pathlib import Path
import re
from math import floor


def count_csv_rows(file_path: Path) -> int:
    with open(file_path, "r", encoding="latin-1") as f:
        return sum(1 for _ in f) - 1  # subtract 1 for header


def format_seconds_to_hms(seconds: int) -> str:
    hours = floor(seconds / 3600)
    minutes = floor((seconds % 3600) / 60)
    secs = floor(seconds % 60)
    return f"{hours} h {minutes} min {secs} s"


def find_latest_file_by_date(directory: Path, filename_pattern: str) -> Path | None:
    pattern = re.compile(filename_pattern)
    latest_file = None
    latest_date = ""

    for file in directory.glob("*"):
        match = pattern.match(file.name)
        if match:
            date_str = match.group(1)
            if date_str > latest_date:
                latest_date = date_str
                latest_file = file
    return directory / latest_file
