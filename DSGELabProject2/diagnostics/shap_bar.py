# This script creates custom shap plots that combine categorical features split into binary features back to the original features.
# For example, features HOME_REGION_Uusimaa, HOME_REGION_Pirkanmaa and combined into a single feature HOME_REGION.

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pickle
import re
import shap
import os
import json

DATE = "2025-08-14-1958"
MODEL_FILE = f"XGBoost/results/xgb_extensive_model_{DATE}.pkl"
TEST_FILE = "XGBoost/xgboost_test_extensive.csv"
SHAP_DF_FILE = f"XGBoost/grouped_shap_extensive_{DATE}.csv"
SHAP_VAL_DF_FILE = f"XGBoost/shap_val_extensive_{DATE}.csv"
GROUPS_FILE = "feature_groups.json"
FIG_FILE = f"XGBoost/results/xgb_extensive_shap_bar_agg_{DATE}.png"
seed = 123


def get_shap_sample_size(test_len, shap_dsize):
    """
    Get the sample size for which SHAP values will be calculated. If auto is selected, clamps the value between 5000 and 100000.
    """
    if shap_dsize == "auto":
        shap_min = min(5000, test_len)
        shap_max = min(100000, test_len)
        return max(shap_min, min(int(0.5 * test_len), shap_max))
    return int(shap_dsize * test_len)


if not os.path.exists(SHAP_DF_FILE):
    print("Calculating shap values.")
    with open(MODEL_FILE, "rb") as f:
        model = pickle.load(f)

    df_test = pd.read_csv(TEST_FILE)
    X_test = df_test.drop("PRESCRIBED", axis=1)
    cols = X_test.columns.tolist()
    n_shap = get_shap_sample_size(len(X_test), "auto")
    X_test_shap = X_test.sample(n=n_shap, random_state=seed)
    explainer = shap.Explainer(model, X_test_shap, seed=seed)
    shap_values = explainer(X_test_shap)

    feature_groups = {
        "AGE_AT_VISIT_DOC": ["AGE_AT_VISIT_DOC"],
        "BIRTH_YEAR_DOC": ["BIRTH_YEAR_DOC"],
        "AGE_AT_VISIT_PAT": ["AGE_AT_VISIT_PAT"],
        "BIRTH_YEAR_PAT": ["BIRTH_YEAR_PAT"],
        "SEX_DOC": ["SEX_DOC_male"],
        "SEX_PAT": ["SEX_PAT_male"],
        "MEAN_YEARLY_PRESCRIPTIONS_DOC": ["MEAN_YEARLY_PRESCRIPTIONS_DOC"],
        "MEAN_YEARLY_PRESCRIPTIONS_PAT": ["MEAN_YEARLY_PRESCRIPTIONS_PAT"],
        "MEAN_YEARLY_DIAGNOSES_DOC": ["MEAN_YEARLY_DIAGNOSES_DOC"],
        "MEAN_YEARLY_DIAGNOSES_PAT": ["MEAN_YEARLY_DIAGNOSES_PAT"],
        "LANGUAGE_DOC": [col for col in cols if "LANGUAGE_DOC" in col],
        "HOME_REGION_DOC": [col for col in cols if "HOME_REGION_DOC" in col],
        "HOME_REGION_PAT": [col for col in cols if "HOME_REGION_PAT" in col],
        "WEEKDAY": ["WEEKDAY"],
        "MONTH": ["MONTH"],
        "SPECIALTY": [col for col in cols if col.startswith("SPECIALTY")],
        "DIAGNOSIS_HISTORY_PAT": [col for col in cols if re.match(r"^HAD_ICD10_(.+)_PAT$", col)],
        "PRESCRIPTION_HISTORY_PAT": [col for col in cols if re.match(r"^GOT_ATC_(.+)_PAT$", col)],
        "DIAGNOSIS_HISTORY_DOC": [col for col in cols if re.match(r"^HAD_ICD10_(.+)_DOC$", col)],
        "PRESCRIPTION_HISTORY_DOC": [col for col in cols if re.match(r"^GOT_ATC_(.+)_DOC$", col)],
    }

    # 3. Calculate the Mean |SHAP| for each original feature by summing the Mean |SHAP| of its dummies.
    aggregated_importance = {}
    importance = {}

    for feature_name, dummy_list in feature_groups.items():
        total_mean_abs_shap = 0.0
        for dummy_name in dummy_list:
            # Find the index of this specific dummy variable
            if dummy_name in shap_values.feature_names:
                idx = shap_values.feature_names.index(dummy_name)
                # Calculate the mean absolute SHAP for this single dummy variable
                mean_abs_dummy = np.mean(np.abs(shap_values.values[:, idx]))
                # Add it to the total for the categorical feature
                total_mean_abs_shap += mean_abs_dummy
                # Store the mean absolute SHAP for this dummy variable
                importance[dummy_name] = mean_abs_dummy
            else:
                print(f"Warning: Feature '{dummy_name}' not found in explainer output.")
        # Store the total summed mean absolute SHAP for the original feature
        aggregated_importance[feature_name] = total_mean_abs_shap

    # 4. Sort the results for plotting
    sorted_aggregated = dict(sorted(aggregated_importance.items(), key=lambda item: item[1], reverse=True))

    # 5. Create a bar plot
    features = list(sorted_aggregated.keys())
    importance_vals = list(sorted_aggregated.values())

    df_shap = pd.DataFrame({"FEATURE": features, "MEAN_SHAP": importance_vals})
    df_shap.to_csv(SHAP_DF_FILE, index=False)

    df_shap_val = pd.DataFrame({"FEATURE": list(importance.keys()), "MEAN_SHAP": list(importance.values())})
    df_shap_val.to_csv(SHAP_VAL_DF_FILE, index=False)
else:
    print("Loading shap values from file.")
    df_shap = pd.read_csv(SHAP_DF_FILE)
    features = df_shap["FEATURE"].tolist()
    importance_vals = df_shap["MEAN_SHAP"].tolist()

    df_shap_val = pd.read_csv(SHAP_VAL_DF_FILE)

    with open(GROUPS_FILE, "r") as f:
        feature_groups = json.load(f)

red = "#ff0454"
plt.figure(figsize=(10, 8))
for i, (feature, importance) in enumerate(zip(features, importance_vals)):
    plt.text(importance + 0.001, i, f"+{round(importance, 2)}", ha="left", va="center", fontsize=12, color=red)
plt.barh(features, importance_vals, color=red)
plt.xlabel("Sum of Mean |SHAP| for Aggregated Feature", fontsize=14)
plt.title("Mean Absolute SHAP Values by Aggregated Feature (extensive)", fontsize=16)
plt.xlim(right=max(importance_vals) * 1.1)
plt.gca().invert_yaxis()  # Most important at the top
plt.tight_layout()
plt.savefig(FIG_FILE, dpi=300, bbox_inches="tight")
print("Plot saved to", FIG_FILE)

hist_features = ["DIAGNOSIS_HISTORY_PAT", "PRESCRIPTION_HISTORY_PAT", "DIAGNOSIS_HISTORY_DOC", "PRESCRIPTION_HISTORY_DOC"]
for hist_feature in hist_features:
    hist_cols = feature_groups[hist_feature]
    n_cols = 20
    hist_df = df_shap_val[df_shap_val["FEATURE"].isin(hist_cols)].sort_values("MEAN_SHAP", ascending=False).iloc[:n_cols]

    plt.figure(figsize=(10, 8))
    plt.barh(hist_df["FEATURE"], hist_df["MEAN_SHAP"], color='blue')
    plt.xlabel("Sum of Mean |SHAP|", fontsize=14)
    plt.title(f"Mean Absolute SHAP Values for {hist_feature} (extensive), Top {n_cols}", fontsize=16)
    plt.xlim(right=max(hist_df["MEAN_SHAP"]) * 1.1)
    plt.gca().invert_yaxis()  # Most important at the top
    plt.tight_layout()
    plt.savefig(f"XGBoost/results/xgb_extensive_shap_bar_agg_{hist_feature}_{DATE}.png", dpi=300, bbox_inches="tight")
    print(f"Plot saved to XGBoost/results/xgb_extensive_shap_bar_agg_{hist_feature}_{DATE}.png")
