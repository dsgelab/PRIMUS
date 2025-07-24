import pandas as pd
import xgboost as xgb
import pickle
import argparse
import matplotlib

matplotlib.use("Agg")
from matplotlib import pyplot as plt
from sklearn.model_selection import RandomizedSearchCV, train_test_split, learning_curve
from sklearn.metrics import (
    average_precision_score,
    precision_recall_curve,
    roc_auc_score,
    roc_curve,
    confusion_matrix,
    ConfusionMatrixDisplay,
    accuracy_score,
    f1_score,
)
from sklearn.calibration import calibration_curve
from scipy.stats import loguniform
from time import time
from datetime import datetime
import numpy as np
import shap
from argparse_utils import probability, probability_or_auto
from utils import format_seconds_to_hms
from matplotlib.backends.backend_pdf import PdfPages


def create_xgb_model(args, seed, df_train, **kwargs):
    return xgb.XGBClassifier(
        objective="binary:logistic",
        scale_pos_weight=sum(df_train["PRESCRIBED"] == 0) / sum(df_train["PRESCRIBED"]) if args.balanced == 1 else 1,
        eval_metric="average_precision" if args.balanced == 1 else "logloss",
        max_delta_step=1 if args.balanced else 0,
        n_estimators=1000,
        early_stopping_rounds=20,
        tree_method=args.tmethod,
        n_jobs=args.nproc,
        random_state=seed,
        **kwargs,
    )


def savefig(path, ax=None):
    plt.tight_layout()
    kwargs = {"dpi": 300, "bbox_inches": "tight"}
    if ax is not None:
        fig = ax.get_figure()
        fig.savefig(path, **kwargs)
        plt.close(fig)
    else:
        plt.savefig(path, **kwargs)
        plt.close()


def plot_precision_recall_curve(y_pred, y_test, ind_samples, auprcs, positive_rate, ax=None, suffix=""):
    if ax is None:
        _, ax = plt.subplots()
    ax.plot(np.linspace(0, 1), positive_rate * np.ones(50), "--k", label="random, AUPRC=" + str(round(positive_rate, 3)))
    for i, inds in enumerate(ind_samples):
        precision, recall, _ = precision_recall_curve(y_test[inds], y_pred[inds])
        if i == 0:
            ax.plot(
                recall,
                precision,
                linewidth=1,
                c="b",
                label="XGBoost, AUPRC=" + str(round(np.mean(auprcs), 3)) + " ± " + str(round(np.std(auprcs), 3)),
            )
        else:
            ax.plot(recall, precision, linewidth=1, c="b")
    ax.set_title(f"Precision-Recall Curve{suffix}", fontsize=20)
    ax.set_xlabel("recall")
    ax.set_ylabel("precision")
    ax.set_ylim([0, 1.01])
    ax.legend()
    return ax


def plot_roc_curve(y_pred, y_test, ind_samples, aucs, ax=None, suffix=""):
    if ax is None:
        _, ax = plt.subplots()
    ax.plot(np.linspace(0, 1), np.linspace(0, 1), "--k", label="random, AUC=0.5")
    for i, inds in enumerate(ind_samples):
        fpr, tpr, _ = roc_curve(y_test[inds], y_pred[inds])
        if i == 0:
            ax.plot(fpr, tpr, linewidth=1, c="b", label="XGBoost, AUC=" + str(round(np.mean(aucs), 3)) + " ± " + str(round(np.std(aucs), 3)))
        else:
            ax.plot(fpr, tpr, linewidth=1, c="b")
    ax.set_title(f"ROC Curve{suffix}", fontsize=20)
    ax.set_xlabel("False positive rate")
    ax.set_ylabel("True positive rate")
    ax.set_ylim([0, 1.01])
    ax.legend()
    return ax


def plot_probability_density(prob_class0, prob_class1, positive_threshold, ax=None, suffix=""):
    if ax is None:
        _, ax = plt.subplots()
    ax.hist(prob_class0, bins=120, alpha=0.5, weights=np.ones(len(prob_class0)) / len(prob_class0), color="red", label="Negative Class (y=0)")
    ax.hist(prob_class1, bins=120, alpha=0.5, weights=np.ones(len(prob_class1)) / len(prob_class1), color="blue", label="Positive Class (y=1)")
    ax.axvline(positive_threshold, color="k", linestyle="dashed", linewidth=2, label=f"Positive Threshold={positive_threshold:.3f}")
    ax.set_xlabel("Predicted Probability", fontsize=12)
    ax.set_ylabel("Density", fontsize=12)
    ax.set_title(f"Distribution of Predicted Probabilities by True Class{suffix}", fontsize=20)
    ax.legend(loc="upper right")
    ax.grid(True, linestyle="--", alpha=0.6)
    return ax


def plot_confusion_matrix(cm, ax=None, suffix=""):
    disp = ConfusionMatrixDisplay(confusion_matrix=cm)
    if ax is None:
        _, ax = plt.subplots()
    disp.plot(cmap=plt.cm.Blues, values_format="d", ax=ax)
    ax.set_title(f"Confusion Matrix with Threshold=Positive Rate{suffix}", fontsize=20)
    return ax


def plot_calibration_curve(y_pred, y_test, ax=None, suffix=""):
    if ax is None:
        _, ax = plt.subplots()
    prob_true, prob_pred = calibration_curve(y_test, y_pred, n_bins=10, strategy="uniform")
    ax.plot(prob_pred, prob_true, marker="o", label="Calibration curve")
    ax.plot([0, 1], [0, 1], linestyle="--", color="gray", label="Perfectly calibrated")
    ax.set_xlabel("Mean predicted probability")
    ax.set_ylabel("Fraction of positives")
    ax.set_title(f"Calibration Plot{suffix}", fontsize=20)
    ax.legend()
    ax.grid(True, linestyle="--", alpha=0.6)
    return ax


def plot_shap_bar(shap_values, max_display, ax=None, suffix=""):
    if ax is not None:
        plt.sca(ax)
    shap.plots.bar(shap_values, max_display=max_display, show=False)
    if ax is None:
        ax = plt.gca()
    ax.set_title(f"Mean Absolute SHAP Values by Feature{suffix}", fontsize=20)
    return ax


def plot_shap_beeswarm(shap_values, max_display, ax=None, suffix=""):
    if ax is not None:
        plt.sca(ax)
    shap.plots.beeswarm(shap_values, max_display=max_display, show=False)
    if ax is None:
        ax = plt.gca()
    ax.set_title(f"SHAP Beeswarm Plot{suffix}", fontsize=20)
    return ax


def save_plots_to_pdf(
    pdf_path,
    y_pred,
    y_test,
    ind_samples,
    auprcs,
    aucs,
    positive_rate,
    positive_threshold,
    prob_class0,
    prob_class1,
    cm,
    shap_values,
    shap_max_display,
    calculate_shap,
    suffix="",
):
    plot_funcs = [
        (
            plot_precision_recall_curve,
            dict(y_pred=y_pred, y_test=y_test, ind_samples=ind_samples, auprcs=auprcs, positive_rate=positive_rate, suffix=suffix),
        ),
        (plot_roc_curve, dict(y_pred=y_pred, y_test=y_test, ind_samples=ind_samples, aucs=aucs, suffix=suffix)),
        (plot_probability_density, dict(prob_class0=prob_class0, prob_class1=prob_class1, positive_threshold=positive_threshold, suffix=suffix)),
        (plot_confusion_matrix, dict(cm=cm, suffix=suffix)),
    ]
    if calculate_shap:
        plot_funcs.extend(
            [
                (plot_shap_bar, dict(shap_values=shap_values, max_display=shap_max_display, suffix=suffix)),
                (plot_shap_beeswarm, dict(shap_values=shap_values, max_display=shap_max_display, suffix=suffix)),
            ]
        )
    with PdfPages(pdf_path) as pdf:
        for func, kwargs in plot_funcs:
            fig, ax = plt.subplots(figsize=(8, 6))
            kwargs_with_ax = kwargs | {"ax": ax}
            func(**kwargs_with_ax)
            pdf.savefig(fig, bbox_inches="tight")
            plt.close(fig)


def predict_proba(model, X):
    return model.predict_proba(X)[:, 1]


def get_shap_sample_size(test_len, shap_dsize):
    """
    Get the sample size for which SHAP values will be calculated. If auto is selected, clamps the value between 5000 and 100000.
    """
    if shap_dsize == "auto":
        shap_min = min(5000, test_len)
        shap_max = min(100000, test_len)
        return max(shap_min, min(int(0.5 * test_len), shap_max))
    return int(shap_dsize * test_len)


def train():
    start_time = time()

    parser = argparse.ArgumentParser()
    parser.add_argument("--outdir", help="Path to the output directory (default=./).", type=str, default="./")
    parser.add_argument("--trainfile", help="Path to the file containing training samples. Only used if --mode is 'train'.", type=str, default=None)
    parser.add_argument("--testfile", help="Path to the file containing test samples.", type=str)
    parser.add_argument(
        "--balanced",
        help="1 if balanced class weights are used, 0 if not (default=0). Only used if --mode is 'train'.",
        type=int,
        choices=[1, 0],
        default=0,
    )
    parser.add_argument("--dropna", help="1 if rows with missing values are dropped, 0 if not (default=0).", type=int, choices=[1, 0], default=0)
    parser.add_argument(
        "--valsize",
        help="Size of the validation set; used in early stopping (default=0.1). Only used if --mode is 'train'.",
        type=probability,
        default=0.1,
    )
    parser.add_argument(
        "--dsize", help="Proportion of the training data to be used (default=1). Only used if --mode is 'train'.", type=probability, default=1
    )
    parser.add_argument("--nproc", help="Number of parallel processes used (default=2).", type=int, default=2)
    parser.add_argument(
        "--nsearch", help="Number of hyperparameter search iterations (default=10). Only used if --mode is 'train'.", type=int, default=10
    )
    parser.add_argument(
        "--tmethod",
        help="Tree method (default=auto). Only used if --mode is 'train'.",
        type=str,
        choices=["auto", "exact", "approx", "hist"],
        default="auto",
    )
    parser.add_argument(
        "--shapdsize",
        help="Proportion of the test data to be used for estimating SHAP values (default=1, or 'auto' for 50%% clamped between 5000 and 100000).",
        type=probability_or_auto,
        default=1,
    )
    parser.add_argument(
        "--fitlc", help="Whether to fit a learning curve (default=1). Only used if --mode is 'train'.", type=int, choices=[1, 0], default=1
    )
    parser.add_argument("--suffix", help="Suffix for output files (default='').", type=str, default="")
    parser.add_argument(
        "--mode",
        help="Mode of the script; 'train' or 'test' (default='train'). Test mode applies the test set to the given model.",
        type=str,
        choices=["train", "test"],
        default="train",
    )
    parser.add_argument(
        "--modelfile", help="Path to the model file to be used for testing (default=None). Required for --mode is 'test'.", type=str, default=None
    )
    parser.add_argument(
        "--testfileorig",
        help="Path to the original test file; used to align columns for model testing. If not set, assumes that the test file is already aligned.",
        type=str,
        default=None,
    )
    parser.add_argument(
        "--shap", help="Whether to calculate shap values (default=1). 1 to calculate, 0 to skip.", type=int, choices=[1, 0], default=1
    )

    args = parser.parse_args()

    if args.mode == "test":
        if args.modelfile is None:
            raise ValueError("If --mode is 'test', --modelfile must be provided.")

    seed = 123
    np.random.seed(seed)

    df_test = pd.read_csv(args.testfile)
    test_len = len(df_test)
    if args.dropna == 1:
        df_test = df_test.dropna().reset_index(drop=True)
    print(f"Test set size: {len(df_test)}/{test_len} ({round(len(df_test) / test_len * 100, 2)}%)\n")

    summary_list = [
        ("suffix", args.suffix),
        ("testfile", args.testfile),
        ("test_size", len(df_test)),
        ("dropna", args.dropna),
        ("trainset_proportion", args.dsize),
    ]
    current_datetime = datetime.now().strftime("%Y-%m-%d-%H%M")
    suffix = "" if args.suffix == "" else f"_{args.suffix}"
    plot_suffix = "" if args.suffix == "" else f" ({args.suffix})"

    if args.mode == "train":
        df_train = pd.read_csv(args.trainfile)
        train_len = len(df_train)
        df_train = df_train.sample(frac=args.dsize, random_state=seed).reset_index(drop=True)
        if args.dropna == 1:
            df_train = df_train.dropna().reset_index(drop=True)
        print(f"Training set size: {len(df_train)}/{train_len} ({round(len(df_train) / train_len * 100, 2)}%)")

        X_train = df_train.drop("PRESCRIBED", axis=1)
        y_train = df_train["PRESCRIBED"]
        X_train, X_val, y_train, y_val = train_test_split(X_train, y_train, test_size=args.valsize, random_state=seed, stratify=y_train)

        search_spaces = {
            "learning_rate": loguniform(1e-3, 0.3),
            "max_depth": np.arange(4, 15),
            "min_child_weight": loguniform(1, 10),
            "gamma": np.arange(0, 10),
            "subsample": np.arange(0.5, 1.0, 0.1),
            "colsample_bytree": np.arange(0.5, 1.0, 0.1),
            "lambda": loguniform(0.1, 10),
            "alpha": loguniform(0.1, 1),
        }

        xgb_model = create_xgb_model(args, seed, df_train)

        search = RandomizedSearchCV(xgb_model, search_spaces, n_iter=args.nsearch, cv=3, verbose=4, scoring="average_precision", random_state=seed)
        search.fit(X_train, y_train, eval_set=[(X_val, y_val)], verbose=False)

        cv_df = pd.DataFrame.from_dict(search.cv_results_)

        # Plot the best search score as a function of the number of iterations
        cv_cummax_scores = cv_df["mean_test_score"].cummax()
        plt.plot(cv_cummax_scores)
        plt.xlabel("Number of iterations passed")
        plt.ylabel("Best search score")
        plt.title(f"Best Test Mean AUPRC during Hyperparameter Search{plot_suffix}")
        savefig(f"{args.outdir}/xgb{suffix}_search_score_{current_datetime}.png")
        print("Search score plot saved.")

        cv_df.to_csv(f"{args.outdir}/xgb{suffix}_cv_results_{current_datetime}.csv", index=False)

        tuned_model = search.best_estimator_
        # Check how the model performs against a model with default hyperparameters
        default_model = create_xgb_model(args, seed, df_train)
        default_model.fit(X_train, y_train, eval_set=[(X_val, y_val)], verbose=False)
        auprc_val_default = average_precision_score(y_val, predict_proba(default_model, X_val))
        auprc_val_tuned = average_precision_score(y_val, predict_proba(tuned_model, X_val))
        search_improvement = auprc_val_tuned - auprc_val_default

        print(f"\nValidation AUPRC (tuned): {auprc_val_tuned:.3f}")
        print(f"Validation AUPRC (default): {auprc_val_default:.3f}")
        print(f"Improvement in AUPRC with tuned hyperparameters: {search_improvement:.4f}\n")
        print(f"Choosing {'tuned' if auprc_val_tuned > auprc_val_default else 'default'} model.")

        model = tuned_model if auprc_val_tuned > auprc_val_default else default_model
        tuned_params = search.best_params_ if auprc_val_tuned > auprc_val_default else {}

        # Save model
        with open(f"{args.outdir}/xgb{suffix}_model_{current_datetime}.pkl", "wb") as f:
            pickle.dump(model, f)
            print("Model saved.")

        # Diagnostics for over/underfitting
        y_pred_train = predict_proba(model, X_train)
        auprc_train = average_precision_score(y_train, y_pred_train)
        auprc_val = max(auprc_val_tuned, auprc_val_default)
        print(f"\nTrain AUPRC: {auprc_train:.3f}")
        print(f"Validation AUPRC: {auprc_val:.3f}\n")

        summary_list.extend(
            [
                ("balanced", args.balanced),
                ("nsearch", args.nsearch),
                ("search_improvement", search_improvement),
                ("train_AUPRC", auprc_train),
                ("val_AUPRC", auprc_val),
                ("trainfile", args.trainfile),
                ("train_size", len(df_train)),
            ]
        )

        # Plot learning curve
        if args.fitlc:
            train_sizes, train_scores, val_scores = learning_curve(
                create_xgb_model(args, seed, df_train, **tuned_params),
                X_train,
                y_train,
                cv=3,
                scoring="average_precision",
                n_jobs=args.nproc,
                random_state=seed,
                fit_params={"eval_set": [(X_val, y_val)], "verbose": False},
                verbose=2,
            )
            plt.plot(train_sizes, np.mean(train_scores, axis=1), "o-", label="Train AUPRC")
            plt.plot(train_sizes, np.mean(val_scores, axis=1), "o-", label="Val AUPRC")
            plt.title(f"Learning Curve{plot_suffix}")
            plt.xlabel("Training set size")
            plt.ylabel("AUPRC")
            plt.legend()
            savefig(f"{args.outdir}/xgb{suffix}_learning_curve_{current_datetime}.png")
            print("Learning curve saved.")
    elif args.mode == "test":
        with open(args.modelfile, "rb") as f:
            model = pickle.load(f)
        summary_list.append(("model_file", args.modelfile))
        if args.testfileorig is not None:
            # Align test columns to match the original test file
            df_test_orig = pd.read_csv(args.testfileorig)
            orig_cols = list(df_test_orig.columns)
            test_cols = list(df_test.columns)
            # Remove columns not in original
            cols_to_remove = [col for col in test_cols if col not in orig_cols]
            df_test = df_test.drop(columns=cols_to_remove)
            # Add columns in original but not in test
            cols_to_add = [col for col in orig_cols if col not in test_cols]
            for col in cols_to_add:
                if df_test_orig[col].dropna().isin([True, False, 0, 1]).all():
                    df_test[col] = 0
                else:
                    df_test[col] = pd.NA
            # Reorder columns to match original
            df_test = df_test[orig_cols]

    X_test = df_test.drop("PRESCRIBED", axis=1)
    y_test = df_test["PRESCRIBED"].to_numpy()

    y_pred = predict_proba(model, X_test)
    df_test["XGB_PRED"] = y_pred
    df_test.to_csv(f"{args.outdir}/xgb{suffix}_predictions_{current_datetime}.csv", index=False)
    print("Test predictions saved.")

    # Generate N random samples
    len_y_test = len(df_test)
    num_subsamples = 10
    f = 0.75
    ind_samples = []
    for _ in range(num_subsamples):
        ind_samples.append(np.random.choice(len_y_test, int(f * len_y_test), replace=False).tolist())

    positive_rate = np.mean(y_test)
    positive_threshold = positive_rate if args.testfileorig is None else df_test_orig["PRESCRIBED"].mean()
    print(f'Positive rate: {positive_rate:.3f}, positive threshold: {positive_threshold:.3f}')

    # Plot precision-recall curve
    auprcs = [average_precision_score(y_test[inds], y_pred[inds]) for inds in ind_samples]
    ax = plot_precision_recall_curve(y_pred, y_test, ind_samples, auprcs, positive_rate, suffix=plot_suffix)
    savefig(f"{args.outdir}/xgb{suffix}_precision_recall_curve_{current_datetime}.png", ax=ax)
    print("Precision-Recall curve saved.")

    # Plot ROC curve
    aucs = [roc_auc_score(y_test[inds], y_pred[inds]) for inds in ind_samples]
    ax = plot_roc_curve(y_pred, y_test, ind_samples, aucs, suffix=plot_suffix)
    savefig(f"{args.outdir}/xgb{suffix}_roc_curve_{current_datetime}.png", ax=ax)
    print("ROC curve saved.")

    # Confidence interval for AUPRC
    n_bootstraps = 1000
    indices = np.random.randint(0, len(y_pred), (len(y_pred), n_bootstraps))
    bootstrapped_auprcs = average_precision_score(y_test[indices], y_pred[indices], average=None)

    alpha = 0.05
    bootstrapped_auprcs.sort()
    confidence_lower_auprc = bootstrapped_auprcs[int(alpha / 2 * len(bootstrapped_auprcs))]
    confidence_upper_auprc = bootstrapped_auprcs[int((1 - alpha / 2) * len(bootstrapped_auprcs))]
    mean_auprc = np.mean(bootstrapped_auprcs)

    CI = int((1 - alpha) * 100)
    summary_list.extend(
        [
            ("mean_AUPRC", mean_auprc),
            (f"lower_CI_AUPRC_{CI}", confidence_lower_auprc),
            (f"upper_CI_AUPRC_{CI}", confidence_upper_auprc),
            ("positive_rate", positive_rate),
            ("positive_threshold", positive_threshold),
        ]
    )

    # Probability densities of predicted probabilities
    prob_class0 = y_pred[y_test == 0]
    prob_class1 = y_pred[y_test == 1]
    ax = plot_probability_density(prob_class0, prob_class1, positive_threshold, suffix=plot_suffix)
    savefig(f"{args.outdir}/xgb{suffix}_prob_density_{current_datetime}.png", ax=ax)
    print("Probability density plot saved.")

    # Confusion matrix
    y_pred_int = (y_pred > positive_threshold).astype(int)
    cm = confusion_matrix(y_test, y_pred_int)
    ax = plot_confusion_matrix(cm, suffix=plot_suffix)
    savefig(f"{args.outdir}/xgb{suffix}_confusion_matrix_{current_datetime}.png", ax=ax)
    print("Confusion matrix saved.")

    # Plot calibration curve
    ax = plot_calibration_curve(y_pred, y_test, suffix=plot_suffix)
    savefig(f"{args.outdir}/xgb{suffix}_calibration_curve_{current_datetime}.png", ax=ax)
    print("Calibration curve saved.")

    # Prediction bias
    pred_bias = np.mean(y_pred) - np.mean(y_test)
    print(f"Prediction bias: {pred_bias:.4f}\n")

    # Accuracy & F1 score
    accuracy = accuracy_score(y_test, y_pred_int)
    f1 = f1_score(y_test, y_pred_int)
    summary_list.extend([("prediction_bias", pred_bias), ("accuracy", accuracy), ("f1_score", f1)])

    # Shap values
    shap_values = None
    n_shap = None
    shap_max_display = 25
    if args.shap == 1:
        n_shap = get_shap_sample_size(len(X_test), args.shapdsize)
        X_test_shap = X_test.sample(n=n_shap, random_state=seed)
        explainer = shap.Explainer(model, X_test_shap, seed=seed)
        shap_values = explainer(X_test_shap)

        ax = plot_shap_bar(shap_values, max_display=shap_max_display, suffix=plot_suffix)
        savefig(f"{args.outdir}/xgb{suffix}_shap_bar_{current_datetime}.png", ax=ax)
        print("SHAP bar plot saved.")

        ax = plot_shap_beeswarm(shap_values, max_display=shap_max_display, suffix=plot_suffix)
        savefig(f"{args.outdir}/xgb{suffix}_shap_beeswarm_{current_datetime}.png", ax=ax)
        print("SHAP beeswarm plot saved.")

    # Save all plots to a PDF, each on a separate page
    save_plots_to_pdf(
        f"{args.outdir}/xgb{suffix}_plot_summary_{current_datetime}.pdf",
        y_pred,
        y_test,
        ind_samples,
        auprcs,
        aucs,
        positive_rate,
        positive_threshold,
        prob_class0,
        prob_class1,
        cm,
        shap_values,
        shap_max_display,
        args.shap,
        suffix=plot_suffix,
    )
    print("Plot summary saved.")

    print(f"Current date and time: {current_datetime}")

    execution_time = time() - start_time
    summary_list.extend([("exec_time_sec", execution_time), ("shap_sample_size", n_shap)])

    pd.DataFrame(summary_list, columns=["statistic", "value"]).to_csv(f"{args.outdir}/xgb{suffix}_summary_{current_datetime}.csv", index=False)
    print("Summary saved.")

    print(f"Script finished in {format_seconds_to_hms(execution_time)}.")


if __name__ == "__main__":
    train()
