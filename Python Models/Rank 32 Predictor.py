# -*- coding: utf-8 -*-
"""
Created on Sat Mar 13 16:55:18 2021

@author: Samuel
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

os.chdir('C:/Users/Samuel/Documents/DS1')

dataset = pd.read_csv('NCAA_cleaned.csv')
predset1 = pd.read_csv('Scored_64.csv')
predset = predset1[['Seed', 'W', 'L', 'AdjEM', 'AdjO', 'AdjO_R',
       'AdjD', 'AdjD_R', 'AdjT', 'AdjT_R', 'Luck', 'Luck_R', 'AdjEM2',
       'AdjEM_R', 'OppO', 'OppO_R', 'OppD', 'OppD_R', 'AdjEM3', 'AdjEM_R4']]


dataset.columns
datafeat = dataset[['Seed', 'W', 'L', 'AdjEM', 'AdjO', 'AdjO_R',
       'AdjD', 'AdjD_R', 'AdjT', 'AdjT_R', 'Luck', 'Luck_R', 'AdjEM2',
       'AdjEM_R', 'OppO', 'OppO_R', 'OppD', 'OppD_R', 'AdjEM3', 'AdjEM_R4']]
dataset.dtypes


#Basic EDA
correlfeature = dataset.corr()
heatmap = sb.heatmap(correlfeature, cmap="YlGnBu") 

sb.pairplot(datafeat[[ 'Seed', 'W', 'L']])
plt.show

sb.pairplot(datafeat[['AdjEM', 'AdjO', 'AdjO_R',
       'AdjD', 'AdjD_R', 'AdjT', 'AdjT_R', 'Luck', 'Luck_R', 'AdjEM2',
       'AdjEM_R', 'OppO', 'OppO_R', 'OppD', 'OppD_R', 'AdjEM3', 'AdjEM_R4']])   
plt.show

sb.pairplot(dataset, x_vars =[ 'Seed', 'Conf', 'W', 'L', 'AdjEM', 'AdjO', 'AdjO_R',
       'AdjD', 'AdjD_R', 'AdjT', 'AdjT_R', 'Luck', 'Luck_R', 'AdjEM2',
       'AdjEM_R', 'OppO', 'OppO_R', 'OppD', 'OppD_R', 'AdjEM3', 'AdjEM_R4'], y_vars=['R32'])
plt.show


#use a voting classifier for R32
x = datafeat
y = dataset[['R32']]





#Tune/change
x_train, x_test, y_train, y_test = train_test_split(x,  
                                                    y,  
                                                    test_size = 0.30,  
                                                    random_state = 42) 
  
estimator = [] 
estimator.append(('LR',  
                  LogisticRegression(solver ='lbfgs',  
                                     multi_class ='multinomial',  
                                     max_iter = 1000))) 
estimator.append(('KNN',KNeighborsClassifier())) 
estimator.append(('RFC', RandomForestClassifier())) 


hard_voter = VotingClassifier(estimators = estimator, voting ='soft') 
hard_voter.fit(x_train, y_train) 
y_pred = hard_voter.predict_proba(x_test) 
t_pred = hard_voter.predict(x_test)

sum(t_pred)

#New Model to Predict with full feed
soft_voter_final = VotingClassifier(estimators = estimator, voting ='soft') 
soft_voter_final.fit(x, y) 
y_pred = soft_voter_final.predict_proba(predset) 
z_pred = soft_voter_final.predict(predset) 


predset1['prob_loss_new'] = y_pred[:,0]
predset1['prob_win_new'] = y_pred[:,1]
predset1['Outcome_new'] = z_pred

save_csv_data = predset1.to_csv('Scored_64.csv',index = False)


#Model Sweet 16
dataset16 = pd.read_csv('NCAA_cleaned.csv')
dataset16 = dataset16[dataset16['R32']==1]
datafeat16 = dataset16[['Seed', 'W', 'L', 'AdjEM', 'AdjO', 'AdjO_R',
       'AdjD', 'AdjD_R', 'AdjT', 'AdjT_R', 'Luck', 'Luck_R', 'AdjEM2',
       'AdjEM_R', 'OppO', 'OppO_R', 'OppD', 'OppD_R', 'AdjEM3', 'AdjEM_R4']]

predset17 = pd.read_csv('Scored_64_estimated_32.csv')
predset16 = predset17[['Seed', 'W', 'L', 'AdjEM', 'AdjO', 'AdjO_R',
       'AdjD', 'AdjD_R', 'AdjT', 'AdjT_R', 'Luck', 'Luck_R', 'AdjEM2',
       'AdjEM_R', 'OppO', 'OppO_R', 'OppD', 'OppD_R', 'AdjEM3', 'AdjEM_R4']]



#use a voting classifier for R16
x_16 = datafeat16
y_16 = dataset16[['R16']]

soft_16_voter = VotingClassifier(estimators = estimator, voting ='soft') 
soft_16_voter.fit(x_16, y_16) 
y_pred_16 = soft_16_voter.predict_proba(predset16) 
z_pred_16 = soft_16_voter.predict(predset16) 


predset17['prob_loss_new_16'] = y_pred_16[:,0]
predset17['prob_win_new_16'] = y_pred_16[:,1]
predset17['Outcome_new_16'] = z_pred_16

save_csv_data_16 = predset17.to_csv('Scored_64_32.csv',index = False)



