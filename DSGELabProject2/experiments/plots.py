import matplotlib.pyplot as plt
import numpy as np
from sklearn.metrics import roc_curve, precision_recall_curve, ConfusionMatrixDisplay
import shap


def plot_precision_recall_curve(y_pred, y_test, ind_samples, auprcs, positive_rate, ax=None):
    if ax is None:
        _, ax = plt.subplots()
    ax.plot(np.linspace(0, 1), positive_rate * np.ones(50), "--k", label="random, AUPRC=" + str(positive_rate))
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
    ax.set_title("Precision-Recall Curve", fontsize=20)
    ax.set_xlabel("recall")
    ax.set_ylabel("precision")
    ax.legend()
    return ax


def plot_roc_curve(y_pred, y_test, ind_samples, aucs, ax=None):
    if ax is None:
        _, ax = plt.subplots()
    ax.plot(np.linspace(0, 1), np.linspace(0, 1), "--k", label="random, AUC=0.5")
    for i, inds in enumerate(ind_samples):
        fpr, tpr, _ = roc_curve(y_test[inds], y_pred[inds])
        if i == 0:
            ax.plot(fpr, tpr, linewidth=1, c="b", label="XGBoost, AUC=" + str(round(np.mean(aucs), 3)) + " ± " + str(round(np.std(aucs), 3)))
        else:
            ax.plot(fpr, tpr, linewidth=1, c="b")
    ax.set_title("ROC Curve", fontsize=20)
    ax.set_xlabel("False positive rate")
    ax.set_ylabel("True positive rate")
    ax.legend()
    return ax


def plot_probability_density(prob_class0, prob_class1, positive_rate, ax=None):
    if ax is None:
        _, ax = plt.subplots()
    ax.hist(prob_class0, bins=120, alpha=0.5, weights=np.ones(len(prob_class0)) / len(prob_class0), color="red", label="Negative Class (y=0)")
    ax.hist(prob_class1, bins=120, alpha=0.5, weights=np.ones(len(prob_class1)) / len(prob_class1), color="blue", label="Positive Class (y=1)")
    ax.axvline(positive_rate, color="k", linestyle="dashed", linewidth=2, label=f"Positive Rate={positive_rate}")
    ax.set_xlabel("Predicted Probability", fontsize=12)
    ax.set_ylabel("Density", fontsize=12)
    ax.set_title("Distribution of Predicted Probabilities by True Class", fontsize=20)
    ax.legend(loc="upper right")
    ax.grid(True, linestyle="--", alpha=0.6)
    return ax


def plot_confusion_matrix(cm, ax=None):
    disp = ConfusionMatrixDisplay(confusion_matrix=cm)
    if ax is None:
        _, ax = plt.subplots()
    disp.plot(cmap=plt.cm.Blues, values_format="d", ax=ax)
    ax.set_title("Confusion Matrix with Threshold=Positive Rate", fontsize=20)
    return ax


def plot_shap_bar(shap_values, max_display, ax=None):
    if ax is not None:
        plt.sca(ax)
    shap.plots.bar(shap_values, max_display=max_display, show=False)
    if ax is None:
        ax = plt.gca()
    ax.set_title("Mean Absolute SHAP Values by Feature", fontsize=20)
    return ax


def plot_shap_beeswarm(shap_values, max_display, ax=None):
    if ax is not None:
        plt.sca(ax)
    shap.plots.beeswarm(shap_values, max_display=max_display, show=False)
    if ax is None:
        ax = plt.gca()
    ax.set_title("SHAP Beeswarm Plot", fontsize=20)
    return ax
