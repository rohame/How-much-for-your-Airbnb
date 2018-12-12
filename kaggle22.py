import numpy as np
import pandas as pd
from keras.models import Sequential
from keras.callbacks import EarlyStopping, ModelCheckpoint
from keras.layers import Dense, Activation, Dropout
from keras import metrics
from sklearn import preprocessing
from sklearn.preprocessing import OneHotEncoder
from sklearn.preprocessing import LabelEncoder

training_set = pd.read_csv('/Users/huangyuqing/Downloads/train.csv')
# training_set=pd.get_dummies(training_set)

lbl = LabelEncoder()
lbl.fit(training_set['penalty'])
training_set['penalty'] = lbl.transform(training_set['penalty'])
oh = OneHotEncoder()
pp = oh.fit_transform(training_set[['penalty']]).toarray()
pp = pp[:, 1:]

# for f in training_set.columns:
#   if training_set[f].dtype == 'object':
#      lbl = LabelEncoder()
#     lbl.fit(list(training_set[f].values))
#    training_set[f] = lbl.transform(list(training_set[f].values))
training_set['n_jobs'].replace(-1, 8, inplace=True)
x1 = training_set.drop(['id', 'time'], axis=1)
y0 = training_set[['time']]
x0 = training_set.drop(['id', 'time'], axis=1)
y = training_set[['time']]
y1 = training_set[['time']].values
from sklearn.decomposition import PCA

# pca=PCA(n_components='mle',svd_solver='full')
# x2=pca.fit_transform(x0)
x2 = x0.drop(['scale', 'flip_y', 'n_informative'], axis=1)
# print (pca.explained_variance_ratio_)
# print (pca.explained_variance_)
# print (pca.n_components_)
ss = preprocessing.MinMaxScaler()
x = x2
print(x.columns)
x.iloc[:, [ 3, 4,6, 7, 8, 9]] = ss.fit_transform(x2.iloc[:, [ 3, 4, 6, 7, 8, 9]])
print(x.shape[1])
m = Sequential()
m.add(Dense(512, activation='relu', input_shape=(x.shape[1],)))
m.add(Dense(512, activation='relu'))
m.add(Dense(256, activation='relu'))
m.add(Dense(256, activation='relu'))
m.add(Dense(1, activation='relu'))

m.compile(loss='mean_squared_error',
          optimizer='Adam',
          metrics=[metrics.mae, metrics.categorical_accuracy])

m.fit(x,
      y,
      epochs=500, callbacks=[
        EarlyStopping(monitor='val_loss', patience=40),
        ModelCheckpoint(
            'best.model',
            monitor='val_loss',
            save_best_only=True,
            verbose=1
        )],
      verbose=2,
      validation_split=0.1, shuffle=True,

      )
# Load the best model
m.load_weights("best.model")

import pandas as pd
from sklearn.metrics import mean_squared_error

predict9 = m.predict(x)

print(mean_squared_error(y, predict9))

test = pd.read_csv('/Users/huangyuqing/Downloads/all/test.csv')
test['penalty'] = lbl.transform(test['penalty'])
# test=pd.get_dummies(test)
test['n_jobs'].replace(-1, 8, inplace=True)
# for f in test.columns:
#   if test[f].dtype == 'object':
#      lbl = LabelEncoder()
#     lbl.fit(list(test[f].values))
#    test[f] = lbl.transform(list(test[f].values))
xt = test.drop(['scale', 'flip_y', 'n_informative', 'id', ], axis=1)
# xt=pca.transform(test.drop(['id'],axis=1))
xt2 = xt
xt2.iloc[:, [ 3, 4, 6, 7, 8, 9]] = ss.transform(xt.iloc[:, [ 3, 4, 6, 7, 8, 9]])
yt = m.predict(xt2)
result = pd.DataFrame()
result['id'] = range(0, 100)
result['time'] = yt
result.to_csv("result.csv", index=False)
