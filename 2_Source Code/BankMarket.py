# -*- coding: utf-8 -*-
"""
Created on Fri Nov 10 23:01:51 2017

@author: Zhang Siyuan
"""

import pandas as pd
import numpy as np
import matplotlib.pylab as plt
from sklearn.cluster import DBSCAN
file = 'C:/Users/ADMIN/Desktop/bkmktg.xls'
df = pd.read_excel(file)





# one hot encoding---------------------------------------------------------------------------------
f = 'job marital education default housing loan contact poutcome y'.split()
def onehotencode_features(df, features):
    dfx = df.copy()
    for i in features:
        # Convert categorical variable into dummy/indicator variables
        dftmp = pd.get_dummies(dfx[i])
        col = dftmp.columns
        # drop the first column, e.g., the 'no' column, to keep only the other columns
        dftmp.drop(col[0], 1, inplace=True)
        col = dftmp.columns
        # rename the column categories
        dftmp.columns = [i + '_' + j for j in col]
        # drop the source column
        dfx.drop(i, 1, inplace=True)
        # cbind old dataframe with the new one-hot columns
        dfx = pd.concat([dfx, dftmp], 1)
    return dfx
# df is the source dataframe
# dfa is the processed one-hot-encoded dataframe
dfa = onehotencode_features(df, f)
# -------------------------------------------------------------------------------------------------





# PCA----------------------------------------------------------------------------------------------
f = 'date_c'
dfa[f] = dfa[f].apply(lambda x: x.toordinal())

def applyPCA(df, n=2):
    from sklearn.decomposition import PCA
    pca = PCA(n_components=n)
    pca.fit(df)
    return pca

def drawPCA(df, select):
    import matplotlib.pyplot as plt
    from sklearn.preprocessing import scale
    dfx = df.copy()
    # center-scale the numeric columns
    for i in range(37):
        dfx.iloc[:,i] = scale(dfx.iloc[:,i])
    
    # X_r is the PCA columns
    global X_r
    X_r = applyPCA(dfx.iloc[:,select]).transform(dfx.iloc[:,select])
    # the last column is the label column
    y = dfx.iloc[:,-1]
    
    plt.figure()
    colors = ['red', 'blue']
    target_names = ['no','yes']
    lw = 2
    for color, i, target_name in zip(colors, [0, 1], target_names):
        plt.scatter(X_r[y == i, 0], X_r[y == i, 1], color=color, alpha=.8, lw=lw, label=target_name)
    plt.legend(loc='best', shadow=False, scatterpoints=1)
    plt.title('PCA of Bank Marketing dataset')
    plt.show()
drawPCA(dfa, [0,1,2,3,4,5,6,7,9,31,14,13,30,10,36])
# -------------------------------------------------------------------------------------------------





# Density-based spatial clustering of applications with noise (DBSCAN)-----------------------------
#X_r is the dataset generated after PCA
bankdb = DBSCAN(eps=0.4, min_samples=10).fit(X_r)
core_samples_mask = np.zeros_like(bankdb.labels_, dtype=bool)
core_samples_mask[bankdb.core_sample_indices_] = True
labels = bankdb.labels_
n_clusters = len(set(labels)) - (1 if -1 in labels else 0)


y_list=df.iloc[:,17]
y_listb = y_list.values
y_list = y_listb.astype(bool)

# Black removed and is used for noise instead.
unique_labels = set(labels)
colors = [plt.cm.Spectral(each)
          for each in np.linspace(0, 1, len(unique_labels))]

for k, col, name in zip(unique_labels, colors, ['Cluster 1','Cluster 2','Noise']):
    if k == -1:
        col = [0,0,0,1]

    class_member_mask = (labels == k)

    xy_1 = X_r[class_member_mask & core_samples_mask] 
# y_list is the boolean array about original class y (yes: true, no: false)
    plt.plot(xy_1[:, 0], xy_1[:, 1], 'o', markerfacecolor=col, markeredgecolor='k', markersize=14, label=name+': %s' % np.count_nonzero(labels == k))

    xy_2 = X_r[class_member_mask & core_samples_mask & y_list] 
    plt.plot(xy_2[:, 0], xy_2[:, 1], '*', markerfacecolor='g', markeredgecolor='g', markersize=5, label=name+'-yes: %s' % np.count_nonzero(xy_2[:,0]))

    xy = X_r[class_member_mask & ~core_samples_mask]
    plt.plot(xy[xy[:,0] < 20, 0], xy[xy[:,0] < 20, 1], 'o', markerfacecolor=col, markeredgecolor='k', markersize=4)

plt.legend()
plt.title('DBScan Clustering of Bank Marketing dataset')
plt.show()

