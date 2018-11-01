from enum import Enum
import csv
import math
from sklearn import tree
from sklearn.ensemble import RandomForestClassifier


class LearningAlgorithms(Enum):
    cart = 0,
    random_forest = 1


class RandomForestsConfig:
    def __init__(self, n_tree, max_features):
        self.n_tree = n_tree
        self.max_features = max_features


def learn(feature_fields, learning_sets_file_names, rescale, min_samples_leaf,
          learning_algorithm, random_forest_config):
    features_learn = []
    classes_learn = []
    for path in learning_sets_file_names:
        (curr_features, curr_classes) = load_data(path, feature_fields, rescale)
        features_learn = features_learn + curr_features
        classes_learn = classes_learn + curr_classes
    if learning_algorithm is LearningAlgorithms.cart:
        clf = tree.DecisionTreeClassifier(min_samples_leaf=min_samples_leaf, criterion="gini")
    else:
        clf = RandomForestClassifier(n_estimators=random_forest_config.n_tree,
                                     max_features=random_forest_config.max_features,
                                     criterion="gini", bootstrap=True,
                                     min_samples_leaf=min_samples_leaf)
    clf = clf.fit(features_learn, classes_learn)
    return clf


def rescale_feature_values(features, n_features):
    maxima = list(map(lambda col: max(map(lambda row: row[col], features)), range(0, n_features)))
    minima = list(map(lambda col: min(map(lambda row: row[col], features)), range(0, n_features)))
    return list(map(
        lambda row: list(map(lambda col: (row[col] - minima[col]) / (maxima[col] - minima[col]), range(0, n_features))),
        features))


# result is sorted, starting from smallest speedup to largest
def load_data(csv_file_name, feature_fields, rescale):
    result = []
    with open(csv_file_name, newline='') as csvFile:
        content = csv.reader(csvFile, delimiter="\t", quotechar="\"")
        head = next(content)
        fields = {}
        for i in range(0, len(head)):
            fields[head[i]] = i
        for row in content:
            curr_result = []
            if row[fields["class"]] != "-":
                curr_feature_vals = []
                for field in feature_fields:
                    curr_feature_vals.append(float(row[fields[field]]))
                curr_result = curr_result + [float(row[fields["class"]])]
                curr_result = curr_result + curr_feature_vals
                result.append(curr_result)
    result = sorted(result, key=lambda r : r[1])
    features = list(map(lambda row : row[1:] , result))
    classes = list(map(lambda row : row[0] , result))
    if rescale:
        return rescale_feature_values(features, len(feature_fields)), classes
    return features, classes


