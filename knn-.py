import csv
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.neighbors       import KNeighborsClassifier
from sklearn.metrics         import accuracy_score, precision_score, recall_score
 # precision_score, recall_score
import warnings
warnings.filterwarnings('ignore')


def read_iris():
    with open('./iris/train.csv', 'r') as f:
        train = list(csv.reader(f))[1:]

    with open('./iris/test.csv', 'r') as f:
        test  = list(csv.reader(f))[1:]

    xs_train = [d[:-1] for d in train]
    ys_train = [d[-1]  for d in train]

    xs_test  = [d[:-1] for d in test ]
    ys_test  = [d[-1]  for d in test ]

    # print(len(xs_train))
    # print(len(ys_test))

    return xs_train, ys_train, xs_test, ys_test

def read_car():
    with open('./car/train.csv', 'r') as f:
        train = list(csv.reader(f))[1:]

    with open('./car/test.csv', 'r') as f:
        test  = list(csv.reader(f))[1:]

    xs_train = [d[1:-1] for d in train]
    ys_train = [d[-1]   for d in train]

    xs_test  = [d[1:-1] for d in test ]
    ys_test  = [d[-1]   for d in test ]
    #
    # print(len(xs_train))
    # print(len(ys_test))

    return xs_train, ys_train, xs_test, ys_test

def main():

    # xs_train, ys_train, xs_test, ys_test = read_iris()
    xs_train, ys_train, xs_test, ys_test = read_car()

    max_k = 10
    for k in range(1, max_k+1):
        knn = KNeighborsClassifier(n_neighbors=k)
        knn.fit(xs_train, ys_train)
        pd = knn.predict(xs_test)

        average = 'macro'
        # macro weighted

        ac = accuracy_score( pd, ys_test)
        pc = precision_score(pd, ys_test, average=average)
        # pc = precision_score(pd, ys_test, average='weighted')
        rc = recall_score(   pd, ys_test, average=average)
        # rc = recall_score(   pd, ys_test, average='weighted')

        print(f'{k:>2} | {ac:^8.2%} | {pc:^8.2%} | {rc:^8.2%}')

    # with open('./car/car_knn_predict.csv', 'w') as f:
    #     writer = csv.writer(f, lineterminator='\n')
    #     writer.writerows([[p] for p in ps])


if __name__ == '__main__': main()
