import pandas as pd
import numpy as np
from sklearn.preprocessing import LabelEncoder
from sklearn.impute import SimpleImputer
from sklearn.model_selection import StratifiedShuffleSplit
from sklearn.utils import resample
from lightgbm import LGBMClassifier

# ---------------------------
# Weighted G-Mean scorer
# ---------------------------
from sklearn.metrics import precision_score, recall_score

def weighted_gmean(y_true, y_pred):
    classes = np.unique(y_true)
    gmean_list, weights = [], []
    for cls in classes:
        mask = (y_true == cls)
        weights.append(mask.sum())
        precision = precision_score(mask, y_pred == cls, zero_division=0)
        recall = recall_score(mask, y_pred == cls, zero_division=0)
        gmean_list.append(np.sqrt(precision * recall))
    return np.average(gmean_list, weights=weights)

# ---------------------------
# Load data
# ---------------------------
train = pd.read_csv("training_tipologia.dsv", sep=";")
test = pd.read_csv("test_tipologia.dsv", sep=";")
test_ids = test["TRACE_ID"]

feature_cols = [
    'NUM_EVENTS', 'MAX_DURATION', 'MIN_DURATION', 'AVG_DURATION',
    'NUM_CLIENTS', 'NUM_DATANODES', 'READ_OPERATION', 'WRITE_OPERATION', 'RPC_OPERATION',
    'MAX_NODES_PER_LEVEL', 'MIN_NODES_PER_LEVEL', 'AVG_NODES_PER_LEVEL',
    'MAX_CHILDREN_PER_NODE', 'MIN_CHILDREN_PER_NODE', 'AVG_CHILDREN_PER_NODE'
]

X = train[feature_cols].copy()
y = train["ANOMALY_TYPE"].copy()
X_test = test[feature_cols].copy()

# ---------------------------
# Feature Engineering
# ---------------------------
for df in [X, X_test]:
    df['AVG_DURATION_PER_EVENT'] = df['AVG_DURATION'] / (df['NUM_EVENTS'] + 1e-6)
    df['READ_PER_DN'] = df['READ_OPERATION'] / (df['NUM_DATANODES'] + 1e-6)
    df['WRITE_PER_DN'] = df['WRITE_OPERATION'] / (df['NUM_DATANODES'] + 1e-6)
    df['RPC_PER_EVENT'] = df['RPC_OPERATION'] / (df['NUM_EVENTS'] + 1e-6)
    df['EVENTS_CLIENTS'] = df['NUM_EVENTS'] * df['NUM_CLIENTS']

# ---------------------------
# Preprocessing
# ---------------------------
imputer = SimpleImputer(strategy='median')
X = pd.DataFrame(imputer.fit_transform(X), columns=X.columns)
X_test = pd.DataFrame(imputer.transform(X_test), columns=X_test.columns)

# Encode labels
le = LabelEncoder()
y_encoded = le.fit_transform(y)
num_classes = len(le.classes_)

# ---------------------------
# Bagging with Oversampling
# ---------------------------
n_models = 5
subset_size = 45000  # ~1/3 of training data per model
sss = StratifiedShuffleSplit(n_splits=n_models, train_size=subset_size, random_state=42)

pred_probs = np.zeros((X_test.shape[0], num_classes))

for i, (train_idx, _) in enumerate(sss.split(X, y_encoded)):
    print(f"Training model {i+1}/{n_models}")

    X_subset = X.iloc[train_idx]
    y_subset = y_encoded[train_idx]

    # ---------------------------
    # Oversample rare classes in subset
    # ---------------------------
    df_subset = X_subset.copy()
    df_subset['target'] = y_subset
    max_count = df_subset['target'].value_counts().max()
    df_list = []

    for cls in df_subset['target'].unique():
        df_cls = df_subset[df_subset['target'] == cls]
        if len(df_cls) < max_count:
            df_cls_resampled = resample(df_cls, replace=True, n_samples=max_count, random_state=42+i)
            df_list.append(df_cls_resampled)
        else:
            df_list.append(df_cls)

    df_balanced = pd.concat(df_list)
    X_balanced = df_balanced.drop('target', axis=1)
    y_balanced = df_balanced['target']

    # ---------------------------
    # Train LightGBM
    # ---------------------------
    model = LGBMClassifier(
        n_estimators=300,
        learning_rate=0.05,
        num_leaves=32,
        class_weight='balanced',
        subsample=0.8,
        colsample_bytree=0.8,
        random_state=42+i,
        n_jobs=-1,
        verbose=-1
    )
    model.fit(X_balanced, y_balanced)

    # ---------------------------
    # Predict probabilities
    # ---------------------------
    pred_probs += model.predict_proba(X_test)

# Average probabilities (soft voting)
pred_probs /= n_models
y_test_pred = le.inverse_transform(np.argmax(pred_probs, axis=1))

# ---------------------------
# Save predictions
# ---------------------------
output = pd.DataFrame({"TRACE_ID": test_ids, "ANOMALY_TYPE": y_test_pred})
output_file = "predictions_tipologia_bagging_best.dsv"
output.to_csv(output_file, sep=';', index=False)
print(f"Predictions saved to {output_file}")
