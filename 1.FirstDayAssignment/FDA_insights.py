import pandas as pd

filepath = "Dropbox/MSBA/CommunicatingData/1.FirstDayAssignment/1_FDA_AdData.csv"

df = pd.read_csv(filepath)
df['index'] = df.index
df['Sales'] = df['Sales'] * 1000

