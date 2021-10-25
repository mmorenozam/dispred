# -*- coding: utf-8 -*-
"""
Created on Thu Jun 18 22:40:42 2020

@author: mmorenozam
"""

opath = 'C:/Users/mmore/Documents/new_diseases/weather/'

import pandas as pd

df = pd.read_csv(opath+'weat4python_2.csv')

df['MESS_DATUM'] = pd.to_datetime(df['MESS_DATUM'])

df = df.set_index(['MESS_DATUM'])

def ecol(x):
    return df[[x]].dropna()

wind = ecol('FM')
prec = ecol('RSK')
snow = ecol('SHK_TAG')
pres = ecol('PM')
temp = ecol('TMK')
humi = ecol('UPM')
temx = ecol('TXK')
temm = ecol('TNK')

def resampler1(x): #resampler for sum of means for the previous files, lets see what happens if i chage the resampler
    if i==0:
        return x.resample('D').mean().rolling(window=1).mean()[0::1]
    else:
        return x.resample('D').mean().rolling(window=i+1).mean()[0::1]


############wind###########

dw={}
for i in range(0,61,1):
    dw[i] = wind.apply(resampler1)
    dw[i]['window'] = i
    
wind = pd.concat(dw.values(),ignore_index=False)

wind = pd.DataFrame(wind.to_records())

wind.index = wind['MESS_DATUM']

wind.to_csv(opath+'berlin_wind61_mom.csv',sep=',')

############prec###########

dp={}
for i in range(0,61,1):
    dp[i] = prec.apply(resampler1)
    dp[i]['window'] = i
    
prec = pd.concat(dp.values(),ignore_index=False)

prec = pd.DataFrame(prec.to_records())

prec.index = prec['MESS_DATUM']

prec.to_csv(opath+'berlin_prec61_mom.csv',sep=',')

############snow###########

ds={}
for i in range(0,61,1):
    ds[i] = snow.apply(resampler1)
    ds[i]['window'] = i
    
snow = pd.concat(ds.values(),ignore_index=False)

snow = pd.DataFrame(snow.to_records())

snow.index = snow['MESS_DATUM']

snow.to_csv(opath+'berlin_snow61_mom.csv',sep=',')

############pres###########

dps={}
for i in range(0,61,1):
    dps[i] = pres.apply(resampler1)
    dps[i]['window'] = i
    
pres = pd.concat(dps.values(),ignore_index=False)

pres = pd.DataFrame(pres.to_records())

pres.index = prec['MESS_DATUM']

pres.to_csv(opath+'berlin_pres61_mom.csv',sep=',')

############temp###########
dt={}
for i in range(0,61,1):
    dt[i] = temp.apply(resampler1)
    dt[i]['window'] = i
    
temp = pd.concat(dt.values(),ignore_index=False)

temp = pd.DataFrame(temp.to_records())

temp.index = temp['MESS_DATUM']
#print( dt[2][0:20])
temp.to_csv(opath+'berlin_temp61_mom.csv',sep=',')

############humi###########
dh={}
for i in range(0,61,1):
    dh[i] = humi.apply(resampler1)
    dh[i]['window'] = i
    
humi = pd.concat(dh.values(),ignore_index=False)

humi = pd.DataFrame(humi.to_records())

humi.index = temp['MESS_DATUM']
#print( dt[2][0:20])
humi.to_csv(opath+'berlin_humi61_mom.csv',sep=',')

#########max temperature#######
dm={}
for i in range(0,61,1):
    dm[i] = temx.apply(resampler1)
    dm[i]['window'] = i
    
temx = pd.concat(dm.values(),ignore_index=False)

temx = pd.DataFrame(temx.to_records())

temx.index = temp['MESS_DATUM']
#print( dt[2][0:20])
temx.to_csv(opath+'berlin_temx61_mom.csv',sep=',')

#########min temperature#######
dx={}
for i in range(0,61,1):
    dx[i] = temm.apply(resampler1)
    dx[i]['window'] = i
    
temm = pd.concat(dx.values(),ignore_index=False)

temm = pd.DataFrame(temm.to_records())

temm.index = temp['MESS_DATUM']
#print( dt[2][0:20])
temm.to_csv(opath+'berlin_temm61_mom.csv',sep=',')
