import pandas as pd
import xgboost as xgb
import pickle
import sklearn
import argparse
from matplotlib import pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.metrics import average_precision_score, precision_recall_curve
from time import time
from datetime import datetime
import numpy as np
from argparse_utils import probability


def train():
    parser = argparse.ArgumentParser()
    parser.add_argument("--outdir", help="Path to the output directory (default=./).", type=str, default="./")
    parser.add_argument("--trainfile",help="Path to the file containing training samples.",type=str)
    parser.add_argument("--testfile",help="Path to the file containing test samples.",type=str)
    parser.add_argument("--balanced", help="1 if balanced class weights are used, 0 if not (default=1).", type=int, choices=[1,0], default=1)
    parser.add_argument("--dropna", help="1 if rows with missing values are dropped, 0 if not (default=0).", type=int, choices=[1,0], default=0)
    parser.add_argument("--valsize", help="Size of the validation set (default=0.2).", type=probability, default=0.2)
    parser.add_argument("--dsize", help="Proportion of the training data to be used (default=1).", type=probability, default=1)

    args = parser.parse_args()
    seed = 123

    df_train = pd.read_csv(args.trainfile).sample(frac=args.dsize, random_state=seed)
    df_test = pd.read_csv(args.testfile)
    df_train_len = len(df_train)
    df_test_len = len(df_test)

    if args.dropna == 1:
        df_train = df_train.dropna().reset_index(drop=True)
        df_test = df_test.dropna().reset_index(drop=True)

    print(f'Training set size: {len(df_train)}/{df_train_len}')
    print(f'Test set size: {len(df_test)}/{df_test_len}')

    X_train = df_train.drop('PRESCRIBED', axis=1)
    y_train = df_train['PRESCRIBED']
    X_train, X_val, y_train, y_val = train_test_split(X_train, y_train, test_size=args.valsize, random_state=seed, stratify=y_train)

    X_test = df_test.drop('PRESCRIBED', axis=1)
    y_test = df_test['PRESCRIBED']

    xgb_model = xgb.XGBClassifier(
        objective = 'binary:logistic',
        scale_pos_weight = sum(df_train["PRESCRIBED"] == 0) / sum(df_train["PRESCRIBED"]) if args.balanced == 1 else 1,
        eval_metric = 'auc' if args.balanced == 1 else 'logloss',
        max_delta_step = 1 - args.balanced,
        n_estimators = 1000,
        early_stopping_rounds = 10,
        random_state = seed,
    )
    xgb_model.fit(
        X_train, y_train,
        eval_set=[(X_val, y_val)],
        verbose = 10
    )

    current_datetime = datetime.now().strftime("%Y-%m-%d-%H%M")
    with open(f'{args.outdir}/xgb_model_{current_datetime}.pkl', 'wb') as f:
        pickle.dump(xgb_model, f)
        print(f'Model saved.')

    y_pred = xgb_model.predict_proba(X_test)[:, np.where(xgb_model.classes_ == 1)].flatten()
    df_test['XGB_PRED_PROA'] = y_pred
    with open(f'{args.outdir}/xgb_predictions_{current_datetime}.csv', 'w') as f:
        df_test.to_csv(f, index=False)
        print(f'Test predictions saved.')

    N = 10
    f = 0.75
    ind_samples = []
    for i in range(N):
        ind_samples.append(np.random.choice(len(y_test), size=int(f*len(y_test)), replace=False).tolist())

    auprcs = []
    rand_AUprc = round(np.sum(y_test)/len(y_test), 3)
    plt.plot(np.linspace(0,1), rand_AUprc*np.ones(50), '--k', label="random, auPRC=" + str(rand_AUprc))

    for inds in ind_samples:
        auprcs.append(average_precision_score(y_test[inds], y_pred[inds]))
        precision, recall, threshold = precision_recall_curve(y_test[inds], y_pred[inds])

        if len(auprcs) == len(ind_samples):
            plt.plot(recall, precision, linewidth=1, c='b', label="XGBoost, AUprc=" + str(round(np.mean(auprcs),3)) + " ± " + str(round(np.std(auprcs), 3)))
        else:
            plt.plot(recall,precision,linewidth=1,c='b')

    plt.xlabel("recall")
    plt.ylabel("precision")
    plt.legend()
    plt.tight_layout()
    plt.savefig(f"{args.outdir}/xgb_precision_recall_curve_{current_datetime}.png", dpi=300)
    plt.clf()
    print('Precision-Recall curve saved.')

    n_bootstraps = 2000
    bootstrapped_AUprcs = []

    rng = np.random.RandomState(seed)
    for i in range(n_bootstraps):
        # bootstrap by sampling with replacement on the prediction indices
        indices = rng.randint(0, len(y_pred), len(y_pred))
        if len(np.unique(y_test[indices])) < 2:
            # We need at least one positive and one negative sample for ROC AUC
            # to be defined: reject the sample
            continue

        score = average_precision_score(y_test[indices],y_pred[indices])
        bootstrapped_AUprcs.append(score)    

    sorted_auprcs = np.sort(bootstrapped_AUprcs)
    alpha = 0.05
    confidence_lower_AUC = sorted_auprcs[int(alpha / 2 * len(sorted_auprcs))]
    confidence_upper_AUC = sorted_auprcs[int((1 - alpha / 2) * len(sorted_auprcs))]

    plt.hist(bootstrapped_AUprcs, bins=50)
    # Show random auprc as a vertical line
    plt.axvline(rand_AUprc, color='k', linestyle='dashed', linewidth=2, label="random, auPRC=" + str(rand_AUprc))
    plt.xlabel("AUprc")
    plt.ylabel("Frequency")
    plt.tight_layout()
    plt.legend()
    plt.savefig(f"{args.outdir}/xgb_auprc_histogram_{current_datetime}.png", dpi=300)
    plt.clf()


if __name__ == "__main__":
    train()