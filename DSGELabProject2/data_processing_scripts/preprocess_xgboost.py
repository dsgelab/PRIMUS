import pandas as pd
from sklearn.model_selection import train_test_split
import argparse
from argparse_utils import probability
from utils import find_latest_file_by_date, format_seconds_to_hms
import time


def fct_lump_min(series, min_count, other_label="other"):
    """
    Lump together categories that appear fewer than min_count times
    """
    counts = series.value_counts()
    to_lump = counts[counts < min_count].index
    return series.replace(to_lump, other_label)


def split_train_test_df(df, test_size):
    X, y = df.drop("PRESCRIBED", axis=1), df["PRESCRIBED"]
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size, random_state=42, stratify=y)
    train_df = pd.concat([X_train, y_train], axis=1)
    test_df = pd.concat([X_test, y_test], axis=1)
    return train_df, test_df


def preprocess_data(test_size, categorical_encoding, outdir, user_suffix, input_file):
    df_file = find_latest_file_by_date("ProcessedData", rf"{input_file}_(\d{{8}})\.csv")
    print(f"Using {df_file}")
    df = pd.read_csv(df_file)

    sex_map = {1.0: "male", 2.0: "female"}
    df["SEX_DOC"] = df["SEX_DOC"].map(sex_map)
    df["SEX_PAT"] = df["SEX_PAT"].map(sex_map)
    df["VISIT_DATE"] = pd.to_datetime(df["VISIT_DATE"], format="%Y-%m-%d")
    df["SPECIALTY"] = df["SPECIALTY"].str.replace(" ", "_")

    df["MONTH"] = df["VISIT_DATE"].dt.month
    df["WEEKDAY"] = df["VISIT_DATE"].dt.weekday
    df["LANGUAGE_DOC"] = fct_lump_min(df["LANGUAGE_DOC"], 10000)
    disease_history_cols = df.columns[df.columns.str.startswith("HAD_")].tolist()
    medication_history_cols = df.columns[df.columns.str.startswith("GOT_")].tolist()
    print("disease_history_cols: ", len(disease_history_cols))
    print("medication_history_cols: ", len(medication_history_cols))
    df = df[
        [
            "MONTH",
            "WEEKDAY",
            "SPECIALTY",
            "LANGUAGE_DOC",
            "AGE_DOC",
            "SEX_DOC",
            "HOME_REGION_DOC",
            "SEX_PAT",
            "HOME_REGION_PAT",
            "AGE_PAT",
            "BIRTH_YEAR_DOC",
            "BIRTH_YEAR_PAT",
            "MEAN_YEARLY_DIAGNOSES",
            "MEAN_YEARLY_PRESCRIPTIONS",
            "PRESCRIBED",
            *disease_history_cols,
            *medication_history_cols,
        ]
    ]
    categorical_features = ["SPECIALTY", "LANGUAGE_DOC", "SEX_DOC", "HOME_REGION_DOC", "SEX_PAT", "HOME_REGION_PAT"]
    filename_suffix = ""
    if categorical_encoding == "one_hot":
        df = pd.get_dummies(df, columns=categorical_features, drop_first=True)
        train_df, test_df = split_train_test_df(df, test_size)
    elif categorical_encoding == "freq":
        train_df, test_df = split_train_test_df(df, test_size)
        freq_maps = {key: train_df[key].value_counts(normalize=True).to_dict() for key in categorical_features}
        for key in categorical_features:
            train_df[key] = train_df[key].map(freq_maps[key])
            test_df[key] = test_df[key].map(freq_maps[key])
        filename_suffix = "_freq"

    train_df.to_csv(f"{outdir}/xgboost_train{filename_suffix}{user_suffix}.csv", index=False)
    test_df.to_csv(f"{outdir}/xgboost_test{filename_suffix}{user_suffix}.csv", index=False)


def main():
    start_time = time.time()

    parser = argparse.ArgumentParser()
    parser.add_argument("--outdir", help="Path to the output directory (default=./).", type=str, default="./")
    parser.add_argument("--testsize", help="Test set size as a decimal (default=0.15).", type=probability, default=0.15)
    parser.add_argument(
        "--cat-encoding", help="Encoding method for categorical features (default=one_hot).", type=str, choices=["one_hot", "freq"], default="one_hot"
    )
    parser.add_argument("--suffix", help="Suffix for output files (default='').", type=str, default="")
    parser.add_argument(
        "--inputfile",
        help="Path pattern to the file containing training samples (see utils.py::find_latest_file_by_date).",
        type=str,
        default="J069DiagnosesWithPrescriptions",
    )
    args = parser.parse_args()

    preprocess_data(args.testsize, args.cat_encoding, args.outdir, args.suffix, args.inputfile)

    print(f"Script finished in {format_seconds_to_hms(time.time() - start_time)}.")


if __name__ == "__main__":
    main()
