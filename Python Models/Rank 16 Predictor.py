# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sb
import sklearn as sk
from sklearn.model_selection import train_test_split 
from sklearn.linear_model import LogisticRegression 
from sklearn.ensemble import RandomForestClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import VotingClassifier 
from sklearn.metrics import plot_confusion_matrix

os.chdir('J:\R Projects\Sem Proj\Sem Proj')

dataset = pd.read_csv('NCAA_cleaned.csv')
dataset = dataset[dataset['R32']==1]
datafeat = dataset[['Rk', 'Seed', 'W', 'L', 'AdjEM', 'AdjO', 'AdjO_R',
       'AdjD', 'AdjD_R', 'AdjT', 'AdjT_R', 'Luck', 'Luck_R', 'AdjEM2',
       'AdjEM_R', 'OppO', 'OppO_R', 'OppD', 'OppD_R', 'AdjEM3', 'AdjEM_R4']]
dataset.dtypes



#use a voting classifier for R32
x = datafeat
y = dataset[['R16']]

x_train, x_test, y_train, y_test = train_test_split(x,  
                                                    y,  
                                                    test_size = 0.30,  
                                                    random_state = 42) 




