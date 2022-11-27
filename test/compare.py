#!/bin/env python3

import pandas as pd
import sys

df1 = pd.read_csv('results1/ProtoPan/individuals-r-ProtoPan.csv')
df2 = pd.read_csv('results2/ProtoPan/individuals-r-ProtoPan.csv')
del df1['date-and-time']
del df2['date-and-time']
del df1['simulation-id']
del df2['simulation-id']
comp = df1.compare(df2)
print(comp)
if df1.compare(df2).empty:
    print("Equal")
    sys.exit(0)
else:
    print("Not equal")
    sys.exit(1)

