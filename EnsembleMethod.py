# -*- coding: utf-8 -*-
"""
Created on Wed Jul 23 16:34:40 2014

@author: TRIDIB
"""

##Y_val is the predicted probability from myProgram_GBC.py
##avgTestProb is the predicted probability from DataPrepForRGF.py
avgProb = (Y_val + avgTestProb)/2

FClass = np.empty(xTest.shape[0],dtype = np.object)
count =0
for w in avgProb:
    if w >0.85:
        FClass[count] = 's'
    else:
        FClass[count] = 'b'
    count = count+1

rankOrder = np.argsort(avgProb) + 1

df = pd.DataFrame({"EventId":ids,"RankOrder":rankOrder,"Class":FClass})
df.to_csv("submissionEnsemble.csv", index = False, cols = ["EventId","RankOrder","Class"])
