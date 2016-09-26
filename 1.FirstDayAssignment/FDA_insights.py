import pandas as pd
import ggplot as gg
from ggplot import aes
import statsmodels.formula.api as smf
import statsmodels as sm

filepath = "Dropbox/MSBA/CommunicatingData/1.FirstDayAssignment/1_FDA_AdData.csv"
df = pd.read_csv(filepath)

#transformations used for analysis later on
df['Sales'] = df['Sales'] * 1000

#not in appendix - shows sums of each column
df['AdSpending'] = df[["TV","Radio","Newspaper"]].sum(axis=1)

#Table 2: Summary Statistics
df[["TV","Radio","Newspaper","AdSpending"]].describe()

#Table 3: Sums of Advertising Spending
df[["TV","Radio","Newspaper","AdSpending","Market"]].groupby(["Market"]).sum()

#Figure 1: Histograms of Sales and Advertising Spending
gg.ggplot(df,aes(x='Sales')) +\
    geom_histogram() +\
    facet_grid('Market')
gg.ggplot(df,aes(x='AdSpending')) +\
    geom_histogram() +\
    facet_grid('Market')

#reshape df so each row is a single 
dfMelt = pd.melt(df, id_vars=['Market','Sales'], value_vars=['TV','Radio','Newspaper'])
dfMelt['Sales'] = dfMelt['Sales']/1000

#plot
gg.ggplot(dfMelt,gg.aes(x='value',y='Sales',color='Market')) +\
    gg.geom_point() +\
    gg.facet_grid('Market','variable',scales="free") +\
    gg.stat_smooth(color="black",se=False)

modRural = smf.ols('Sales ~ TV + Radio + Newspaper', data=df[df.Market == "Rural"])
resRural = mod.fit()
modSubUrban = smf.ols('Sales ~ TV + Radio + Newspaper', data=df[df.Market == "SubUrban"])
resSubUrban = mod.fit()
modUrban = smf.ols('Sales ~ TV + Radio + Newspaper', data=df[df.Market == "Urban"])
resUrban = mod.fit()

dfoutput = sm.summary_col([resRural,resSubUrban,resUrban],stars=True)
print(dfoutput)
