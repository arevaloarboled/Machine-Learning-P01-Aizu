import csv
import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.neighbors       import KNeighborsClassifier
from sklearn.metrics         import confusion_matrix, f1_score, accuracy_score
import matplotlib.pyplot as plt

#Overview dataset
cardata = pd.read_csv('./data/car.csv', delim_whitespace=0)
cardata.head(10)



def read_car(fname, format=None):
    with open(fname, 'r') as f:
        data = list(csv.reader(f))

    xs = [d[:-1] for d in data]
    ys = [d[-1]  for d in data]

    #
    if format == 'index':
        buying   = {'vhigh':0,  'high' :1,  'med'  :2,  'low'  :3}
        maint    = {'vhigh':0,  'high' :1,  'med'  :2,  'low'  :3}
        doors    = {'2'    :0,  '3'    :1,  '4'    :2,  '5more':3}
        persons  = {'2'    :0,  '4'    :1,  'more' :2}
        lug_boot = {'small':0,  'med'  :1,  'big'  :2}
        safety   = {'low'  :0,  'med'  :1,  'high' :2}
        attributes = [buying, maint, doors, persons, lug_boot, safety]

        xs = [[a[i] for i, a in zip(x, attributes)] for x in xs]

    #
    if format == 'onehot':
        buying   = {'vhigh':[1,0,0,0],  'high' :[0,1,0,0],  'med'  :[0,0,1,0],  'low'  :[0,0,0,1]}
        maint    = {'vhigh':[1,0,0,0],  'high' :[0,1,0,0],  'med'  :[0,0,1,0],  'low'  :[0,0,0,1]}
        doors    = {'2'    :[1,0,0,0],  '3'    :[0,1,0,0],  '4'    :[0,0,1,0],  '5more':[0,0,0,1]}
        persons  = {'2'    :[1,0,0]  ,  '4'    :[0,1,0]  ,  'more' :[0,0,2]  }
        lug_boot = {'small':[1,0,0]  ,  'med'  :[0,1,0]  ,  'big'  :[0,0,2]  }
        safety   = {'low'  :[1,0,0]  ,  'med'  :[0,1,0]  ,  'high' :[0,0,2]  }
        attributes = [buying, maint, doors, persons, lug_boot, safety]

        xs = [[a[i] for i, a in zip(x, attributes)] for x in xs]
        xs = [[j for i in x for j in i] for x in xs]

    # for x in xs[:5]: print(x)

    return xs, ys


def main():

    xs, ys = read_car('./data/car.csv', format='index')

    xs_train, xs_test, ys_train, ys_test = train_test_split(xs, ys, test_size=0.3)

    max_k = 5
    print(' k | accuracy | f1_score')
    print('---+----------+---------')
    for k in range(1, max_k+1):
        knn = KNeighborsClassifier(n_neighbors=k)
        knn.fit(xs_train, ys_train)
        ps = knn.predict(xs_test)

        ac = accuracy_score(ps, ys_test)
        f1 = f1_score(ps, ys_test, average='macro')
        cm = confusion_matrix(ps, ys_test)

        print(f'{k:>2} | {ac:^8.2%} | {f1:^8.2%}')
        print(cm)
        
    # Transform to df for easier plotting
        cm_df = pd.DataFrame(cm,
                     index = ['vhigh', 'high', 'med', 'low'], 
                     columns = ['vhigh', 'high', 'med', 'low'])

        plt.figure(figsize=(7,6))
        sns.heatmap(cm_df, annot=True)
        plt.title('KNN Calssification \nAccuracy:{0:.3f}'.format(accuracy_score(ps, ys_test)))
        plt.ylabel('True label')
        plt.xlabel('Predicted label')
        plt.show()



if __name__ == '__main__': main()
