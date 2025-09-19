import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, LabelEncoder
from sklearn.ensemble import RandomForestClassifier
from imblearn.metrics import geometric_mean_score

train = pd.read_csv("training_categoria.dsv", sep=";")
test = pd.read_csv("test_categoria.dsv", sep=";")

X = train.drop(["TRACE_ID", "ANOMALY", "ANOMALY_CATEGORY"], axis=1)
y = train["ANOMALY_CATEGORY"]

le = LabelEncoder()
y = le.fit_transform(y)

X_train, X_val, y_train, y_val = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y
)

scaler = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_val = scaler.transform(X_val)
X_test = scaler.transform(test.drop("TRACE_ID", axis=1))

clf = RandomForestClassifier(
    n_estimators=300, 
    class_weight="balanced", 
    random_state=42
)
clf.fit(X_train, y_train)

y_pred = clf.predict(X_val)

gmean = geometric_mean_score(y_val, y_pred, average='weighted')
print("G-Mean Weighted:", gmean)

y_test_pred = clf.predict(X_test)
y_test_pred_labels = le.inverse_transform(y_test_pred)

submission = pd.DataFrame({
    "TRACE_ID": test["TRACE_ID"],
    "ANOMALY_CATEGORY": y_test_pred_labels
})
submission.to_csv("submission.dsv", sep=";", index=False)
