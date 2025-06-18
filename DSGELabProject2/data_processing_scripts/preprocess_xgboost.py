import pandas as pd
from utils import find_latest_file_by_date


def fct_lump_min(series, min_count, other_label='other'):
    """
    Lump together categories that appear fewer than min_count times
    """
    counts = series.value_counts()
    to_lump = counts[counts < min_count].index
    return series.replace(to_lump, other_label)


df_file = find_latest_file_by_date("ProcessedData", r"J069DiagnosesWithPrescriptions_(\d{8}).csv")
df = pd.read_csv(df_file)

sex_map = {1.0: "male", 2.0: "female"}
df["SEX_DOC"] = df["SEX_DOC"].map(sex_map)
df["SEX_PAT"] = df["SEX_PAT"].map(sex_map)
df["VISIT_DATE"] = pd.to_datetime(df["VISIT_DATE"], format="%Y-%m-%d")
df["SPECIALTY"] = df["SPECIALTY"].str.replace(" ", "_")

df["MONTH"] = df["VISIT_DATE"].dt.month
df["WEEKDAY"] = df["VISIT_DATE"].dt.weekday
df["LANGUAGE"] = fct_lump_min(df["LANGUAGE"], 10000)
df = df[["SOURCE", "MONTH", "WEEKDAY", "SPECIALTY", "LANGUAGE", "AGE_DOC", "SEX_DOC", "BIRTH_REGION_DOC", "SEX_PAT", "HOME_REGION_PAT", "AGE_PAT", "PRESCRIBED"]]
df = pd.get_dummies(df, columns=["SOURCE", "SPECIALTY", "LANGUAGE", "SEX_DOC", "BIRTH_REGION_DOC", "SEX_PAT", "HOME_REGION_PAT"])

df.to_csv("ProcessedData/xgboost_data.csv", index=False)
