# -*- coding: utf-8 -*-
"""
Created on Thu Jun 18 22:40:42 2020

path should be changed accordingly

@author: mmorenozam
"""

opath = 'C:/Users/mmorenozam/Documents/new_diseases/weather/'

import pandas as pd

df = pd.read_csv(opath+'weat4python_2.csv')

df['MESS_DATUM'] = pd.to_datetime(df['MESS_DATUM'])

df = df.set_index(['MESS_DATUM'])

df = df['1/7/2001':'12/25/2016']

def ecol(x):
    return df[[x]].dropna()

wind = ecol('FM')
prec = ecol('RSK')
snow = ecol('SHK_TAG')
pres = ecol('PM')
temp = ecol('TMK')
humi = ecol('UPM')
temm = ecol('TNK')
temx = ecol('TXK')

def resampler1(x): 
    return x.resample('7D').mean()

############wind###########

ww = wind.apply(resampler1)
ww['RSK'] = prec.apply(resampler1)
ww['SHK_TAG'] = snow.apply(resampler1)
ww['PM'] = pres.apply(resampler1)
ww['TMK'] = temp.apply(resampler1)
ww['UMP'] = humi.apply(resampler1)
ww['TNK'] = temm.apply(resampler1)
ww['TXK'] = temx.apply(resampler1)

ww.to_csv(opath+'wweather.csv',sep=',')
