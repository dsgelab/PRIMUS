#!/usr/bin/env python3
import sys, os, gzip, datetime

# ─── CONFIG ─────────────────────────────────────────────────────────────────────
LOG_DIR     = "/media/volume/Projects/jg/Logs/File_sum"
SAMPLE_SIZE = 1000   # non-null samples per column for type inference
# ────────────────────────────────────────────────────────────────────────────────

def open_file(path):
    if path.endswith(".gz"):
        return gzip.open(path, "rt", encoding="utf-8", errors="ignore")
    else:
        return open(path, "r", encoding="utf-8", errors="ignore")


def infer_type(sample):
    if not sample:
        return "chr"
    # integer check
    if all(s.lstrip("+-").isdigit() for s in sample):
        return "int"
    # float check
    try:
        _ = [float(s) for s in sample]
        return "float"
    except:
        pass
    # date YYYY-MM-DD check
    if all(len(s) == 10 and s[4] == '-' and s[7] == '-' for s in sample):
        return "date(YYYY-MM-DD)"
    return "chr"


def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} path/to/file.csv[.gz]", file=sys.stderr)
        sys.exit(1)

    path = sys.argv[1]
    if not os.path.isfile(path) or not os.access(path, os.R_OK):
        print(f"Error: cannot read {path}", file=sys.stderr)
        sys.exit(1)

    os.makedirs(LOG_DIR, exist_ok=True)
    ts = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    logname = os.path.join(LOG_DIR, f"file_summary_log_{ts}.log")

    with open(logname, "w") as logf, open_file(path) as f:
        # 1) Read header
        header_line = f.readline().rstrip("\n")
        if not header_line:
            print("Error: empty or unreadable header", file=sys.stderr)
            sys.exit(1)
        header = header_line.split(",")
        ncols = len(header)

        # Prepare stats
        cols = [{"missing": 0, "uniques": set(), "sample": []} for _ in range(ncols)]
        first3 = []
        nrows = 0

        # 2) Process every line after header
        for raw in f:
            nrows += 1
            line = raw.rstrip("\n")
            parts = line.split(",")
            # pad or truncate to ncols
            if len(parts) < ncols:
                parts += [""] * (ncols - len(parts))
            elif len(parts) > ncols:
                parts = parts[:ncols]

            # capture first 3 rows
            if nrows <= 3:
                first3.append(line)

            # update per-column stats
            for i, val in enumerate(parts):
                if val == "" or val.upper() == "NA":
                    cols[i]["missing"] += 1
                else:
                    cols[i]["uniques"].add(val)
                    if len(cols[i]["sample"]) < SAMPLE_SIZE:
                        cols[i]["sample"].append(val)

        # 3) Write report
        logf.write(f"Report generated: {datetime.datetime.now():%Y-%m-%d %H:%M:%S}\n")
        logf.write(f"File: {path}\n")
        logf.write(f"Dimensions: {nrows} rows × {ncols} cols\n\n")

        logf.write("Column names:\n")
        for c in header:
            logf.write(f"  • {c}\n")
        logf.write("\nFirst 3 data rows:\n")
        for r in first3:
            logf.write(f"  {r}\n")
        logf.write("\n")

        for idx, colname in enumerate(header, start=1):
            st = cols[idx-1]
            miss = st["missing"]
            uniq = len(st["uniques"])
            pct  = (miss / nrows * 100) if nrows else 0.0
            dtype = infer_type(st["sample"])
            logf.write(f"Column #{idx}: {colname}\n")
            logf.write(f"  Type: {dtype}\n")
            logf.write(f"  Unique values: {uniq}\n")
            logf.write(f"  Missing: {miss}/{nrows} ({pct:.2f}%)\n\n")

    print(f"Wrote report to {logname}")

if __name__ == "__main__":
    main()
