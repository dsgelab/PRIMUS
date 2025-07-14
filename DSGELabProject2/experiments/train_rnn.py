import os
import argparse
import pandas as pd
import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score, average_precision_score, roc_curve, precision_recall_curve
import matplotlib.pyplot as plt
from time import time
from datetime import datetime
from argparse_utils import probability
from utils import format_seconds_to_hms
from torch.utils.data import Dataset, DataLoader
from plots import plot_roc_curve, plot_precision_recall_curve


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


class MLP(nn.Module):
    def __init__(self, input_dim, hidden_dim=64, output_dim=1):
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(input_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, output_dim),
        )

    def forward(self, x):
        return self.net(x)


class TabularDataset(Dataset):
    def __init__(self, X, y):
        self.X = X
        self.y = y

    def __len__(self):
        return self.X.shape[0]

    def __getitem__(self, idx):
        return self.X[idx], self.y[idx]


def train():
    start_time = time()
    parser = argparse.ArgumentParser()
    parser.add_argument("--outdir", type=str, default="./", help="Path to the output directory (default=./).")
    parser.add_argument("--trainfile", type=str, required=True, help="Path to the file containing training samples.")
    parser.add_argument("--testfile", type=str, required=True, help="Path to the file containing test samples.")
    parser.add_argument("--valsize", type=probability, default=0.1, help="Size of the validation set (default=0.1).")
    parser.add_argument("--train_size", type=probability, default=1, help="Proportion of the training data to be used (default=1).")
    parser.add_argument("--test_size", type=probability, default=1, help="Proportion of the test data to be used (default=1).")
    parser.add_argument("--epochs", type=int, default=50, help="Number of training epochs (default=50).")
    parser.add_argument("--batch_size", type=int, default=64, help="Batch size (default=64).")
    parser.add_argument("--lr", type=float, default=1e-3, help="Learning rate (default=1e-3).")
    parser.add_argument("--hidden_dim", type=int, default=64, help="Hidden layer size (default=64).")

    seed = 123
    args = parser.parse_args()
    torch.manual_seed(seed)
    np.random.seed(seed)
    # Data loading
    df_train = pd.read_csv(args.trainfile)
    df_test = pd.read_csv(args.testfile)
    train_len = len(df_train)
    test_len = len(df_test)
    df_train = df_train.sample(frac=args.train_size, random_state=seed).reset_index(drop=True)
    df_train = df_train.dropna().reset_index(drop=True)
    df_test = df_test.sample(frac=args.test_size, random_state=seed).reset_index(drop=True)
    df_test = df_test.dropna().reset_index(drop=True)
    print(f"Training set size: {len(df_train)}/{train_len} ({round(len(df_train) / train_len * 100, 2)}%)")
    print(f"Test set size: {len(df_test)}/{test_len} ({round(len(df_test) / test_len * 100, 2)}%)\n")
    X_train = df_train.drop("PRESCRIBED", axis=1).values.astype(np.float32)
    y_train = df_train["PRESCRIBED"].values.astype(np.float32)
    X_test = df_test.drop("PRESCRIBED", axis=1).values.astype(np.float32)
    y_test = df_test["PRESCRIBED"].values.astype(np.float32)
    X_train, X_val, y_train, y_val = train_test_split(X_train, y_train, test_size=args.valsize, random_state=seed, stratify=y_train)
    # Convert to torch tensors
    X_train = torch.tensor(X_train)
    y_train = torch.tensor(y_train).unsqueeze(1)
    X_val = torch.tensor(X_val)
    y_val = torch.tensor(y_val).unsqueeze(1)
    X_test = torch.tensor(X_test)
    y_test = torch.tensor(y_test).unsqueeze(1)

    # Create Datasets and DataLoaders
    train_dataset = TabularDataset(X_train, y_train)
    val_dataset = TabularDataset(X_val, y_val)

    train_loader = DataLoader(train_dataset, batch_size=args.batch_size, shuffle=True)
    val_loader = DataLoader(val_dataset, batch_size=args.batch_size, shuffle=False)

    # Model
    model = MLP(input_dim=X_train.shape[1], hidden_dim=args.hidden_dim)
    criterion = nn.BCEWithLogitsLoss()
    optimizer = optim.Adam(model.parameters(), lr=args.lr)
    # Training loop
    train_losses, val_losses = [], []
    for epoch in range(args.epochs):
        model.train()
        epoch_loss = 0
        for batch_x, batch_y in train_loader:
            optimizer.zero_grad()
            outputs = model(batch_x)
            loss = criterion(outputs, batch_y)
            loss.backward()
            optimizer.step()
            epoch_loss += loss.item() * batch_x.size(0)
        train_loss = epoch_loss / len(train_loader.dataset)
        train_losses.append(train_loss)
        # Validation
        model.eval()
        with torch.no_grad():
            val_loss = 0
            for val_x, val_y in val_loader:
                val_outputs = model(val_x)
                loss = criterion(val_outputs, val_y)
                val_loss += loss.item() * val_x.size(0)
            val_loss /= len(val_loader.dataset)
            val_losses.append(val_loss)

        print(f"Epoch {epoch + 1}/{args.epochs} - Train Loss: {train_loss:.4f} - Val Loss: {val_loss:.4f}")

    # Save model
    current_datetime = datetime.now().strftime("%Y-%m-%d-%H%M")
    model_path = os.path.join(args.outdir, f"mlp_model_{current_datetime}.pt")
    torch.save(model.state_dict(), model_path)
    print(f"Model saved to {model_path}")

    # Evaluate on test set
    model.eval()
    with torch.no_grad():
        test_logits = model(X_test)
        y_pred = torch.sigmoid(test_logits).cpu().numpy().flatten()
        y_test_np = y_test.cpu().numpy().flatten()
        auc = roc_auc_score(y_test_np, y_pred)
        auprc = average_precision_score(y_test_np, y_pred)
        print("X_test", X_test.shape)
        print("y_test", y_test.shape)
        print("test_probs", y_pred.shape)
        print("y_test_np", y_test_np)

    print(f"Test ROC AUC: {auc:.4f}")
    print(f"Test AUPRC: {auprc:.4f}")
    # Save predictions
    df_test["MLP_PRED"] = y_pred
    pred_path = f"{args.outdir}/mlp_predictions_{current_datetime}.csv"
    df_test.to_csv(pred_path, index=False)
    print(f"Test predictions saved to {pred_path}")

    # Save metrics
    summary_df = pd.DataFrame(
        {
            "statistic": [
                "trainfile",
                "train_size",
                "test_size",
                "Test_ROC_AUC",
                "Test_AUPRC",
            ],
            "value": [
                args.trainfile,
                args.train_size,
                args.test_size,
                auc,
                auprc,
            ],
        }
    )
    summary_path = f"{args.outdir}/mlp_summary_{current_datetime}.csv"
    summary_df.to_csv(summary_path, index=False)
    print(f"Summary saved to {summary_path}")

    # Plot loss curves
    plt.figure()
    plt.plot(train_losses, label="Train Loss")
    plt.plot(val_losses, label="Val Loss")
    plt.xlabel("Epoch")
    plt.ylabel("Loss")
    plt.title("Loss Curves")
    plt.legend()
    loss_curve_path = f"{args.outdir}/mlp_loss_curves_{current_datetime}.png"
    savefig(loss_curve_path)
    print(f"Loss curves saved to {loss_curve_path}")

    # Plot ROC and PR curves
    num_subsamples = 10
    f = 0.75
    ind_samples = []
    len_y_test = len(y_test)
    for _ in range(num_subsamples):
        ind_samples.append(np.random.choice(len_y_test, int(f * len_y_test), replace=False).tolist())

    positive_rate = round(np.mean(y_test), 3)

    # Plot precision-recall curve
    auprcs = [average_precision_score(y_test[inds], y_pred[inds]) for inds in ind_samples]
    ax = plot_precision_recall_curve(y_pred, y_test, ind_samples, auprcs, positive_rate)
    savefig(f"{args.outdir}/xgb_precision_recall_curve_{current_datetime}.png", ax=ax)
    print("Precision-Recall curve saved.")

    # Plot ROC curve
    aucs = [roc_auc_score(y_test[inds], y_pred[inds]) for inds in ind_samples]
    ax = plot_roc_curve(y_pred, y_test, ind_samples, aucs)
    savefig(f"{args.outdir}/xgb_roc_curve_{current_datetime}.png", ax=ax)
    print("ROC curve saved.")


if __name__ == "__main__":
    train()
