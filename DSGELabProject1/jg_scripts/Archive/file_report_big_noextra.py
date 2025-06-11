#!/usr/bin/env python3
import sys, os, csv, gzip, datetime

# ─── CONFIG ─────────────────────────────────────────────────────────────────────
LOG_DIR = "/media/volume/Projects/jg/Logs/File_sum"
SAMPLE_SIZE = 1000  # non-null samples per column
# ────────────────────────────────────────────────────────────────────────────────

def open_file(path):
    if path.endswith(".gz"):
        return gzip.open(path, mode="rt", newline="")
    else:
        return open(path, mode="r", newline="")

def infer_type(sample):
    if not sample:
        return "chr"
    # all-int?
    if all(s.lstrip("+-").isdigit() for s in sample):
        return "int"
    # all-float?
    def is_float(x):
        try:
            float(x)
            return True
        except:
            return False
    if all(is_float(s) for s in sample):
        return "float"
    # all-date?
    def is_date(x):
        parts = x.split("-")
        return len(parts)==3 and all(p.isdigit() for p in parts) and len(parts[0])==4
    if all(is_date(s) for s in sample):
        return "date(YYYY-MM-DD)"
    return "chr"

def main():
    if len(sys.argv)!=2:
        print(f"Usage: {sys.argv[0]} path/to/file.csv[.gz]", file=sys.stderr)
        sys.exit(1)

    path = sys.argv[1]
    if not os.path.isfile(path) or not os.access(path, os.R_OK):
        print(f"Error: cannot read {path}", file=sys.stderr)
        sys.exit(1)

    # ensure log directory exists
    os.makedirs(LOG_DIR, exist_ok=True)

    ts = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    logname = os.path.join(LOG_DIR, f"file_summary_log_{ts}.log")

    with open(logname, "w") as logf, open_file(path) as f:
        reader = csv.reader(f)

        # read header
        try:
            header = next(reader)
        except StopIteration:
            print("Error: empty file", file=sys.stderr)
            sys.exit(1)

        ncols = len(header)
        # initialize per-column stats
        cols = [{
            "missing": 0,
            "uniques": set(),
            "sample": []
        } for _ in range(ncols)]
        first3 = []
        nrows = 0

        # stream through the rest
        for row in reader:
            if len(row) != ncols:
                # skip ragged rows
                continue
            nrows += 1
            if len(first3) < 3:
                first3.append(row)
            for i, val in enumerate(row):
                if val == "" or val.upper() == "NA":
                    cols[i]["missing"] += 1
                else:
                    cols[i]["uniques"].add(val)
                    if len(cols[i]["sample"]) < SAMPLE_SIZE:
                        cols[i]["sample"].append(val)

        # write report
        logf.write(f"Report generated: {datetime.datetime.now():%Y-%m-%d %H:%M:%S}\n")
        logf.write(f"File: {path}\n")
        logf.write(f"Dimensions: {nrows} rows × {ncols} cols\n\n")

        logf.write("Column names:\n")
        for c in header:
            logf.write(f"  • {c}\n")
        logf.write("\nFirst 3 data rows:\n")
        for r in first3:
            logf.write("  " + ",".join(r) + "\n")
        logf.write("\n")

        for idx, colname in enumerate(header, start=1):
            data = cols[idx-1]
            miss = data["missing"]
            uniq = len(data["uniques"])
            pct = (miss / nrows * 100) if nrows else 0.0
            dtype = infer_type(data["sample"])
            logf.write(f"Column #{idx}: {colname}\n")
            logf.write(f"  Type: {dtype}\n")
            logf.write(f"  Unique values: {uniq}\n")
            logf.write(f"  Missing: {miss}/{nrows} ({pct:.2f}%)\n\n")

    print(f"Wrote report to {logname}")

if __name__ == "__main__":
    main()
