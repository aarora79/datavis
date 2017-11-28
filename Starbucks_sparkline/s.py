import os
import pandas as pd
arr = next(os.walk('.'))[2]
ts_files = [f for f in arr if '.csv' in f]
print(arr)
print(len(ts_files))

import plotly.plotly as py
from plotly.graph_objs import *
py.sign_in('aa1603', 'PHHAVAgjg4NQgXLqPTtb')

layout = {
  "autosize": False, 
  "height": 1000, 
  "showlegend": False, 
  "title": "<b>Timeseries for number Starbucks stores 2013-2016</b><br>Countries with the maximum percentage increase in number Starbucks stores. \
            <br>Clean data and code available <a href='http://aa1603.georgetown.domains/ANLY503/Portfolio/live/plotly_sparkline_starbucks/'>here</a>\
            <br><i>Only includes countries with at least 25 stores as of November 2016.</i>", 
  "width": 800 }

traces = []
count = 1
for f in ts_files:
    df = pd.read_csv(f)  
    trace = {
              "x": df['x'],
              "y": df['y'], 
              "fill": "tozeroy", 
              "line": {
                "shape": "spline", 
                "smoothing": 1.3, 
                "width": 0.5
              }, 
              "mode": "lines", 
              "name": df['x'][0],
              "type": "scatter", 
              "visible": True, 
              "xaxis": "x" + str(count),
              "yaxis": "y" + str(count)
            } 
    traces.append(trace)

    if count in [1,4,7,10,13]:
        x_domain = [0, 0.25]
    elif count in [2,5,8,11,14]:
        x_domain = [0.33, 0.6]
    else:
        x_domain =  [0.7, 1.0]       

    xaxis = {
    "anchor": "y" + str(count), 
    "autorange": True, 
    "domain": x_domain, 
    "mirror": False, 
    "showgrid": False, 
    "showline": False, 
    "showticklabels": False, 
    "showticksuffix": "none", 
    "title": df['country'][0], 
    "titlefont": {"size": 12}, 
    "zeroline": False
    }
    layout["xaxis" + str(count)] = xaxis

          
    y_domain = [[0.8, 0.95], [0.6, 0.75], [0.4, 0.55], [0.2, 0.35], [0.0, 0.15], 
                [0.8, 0.95], [0.6, 0.75], [0.4, 0.55], [0.2, 0.35], [0.0, 0.15], 
                [0.8, 0.95], [0.6, 0.75], [0.4, 0.55], [0.2, 0.35], [0.0, 0.15]
             ]
    yaxis = {
    "anchor": "x" + str(count), 
    "autorange": True, 
    "domain": y_domain[count-1], 
    "mirror": False, 
    "showgrid": False, 
    "showline": False, 
    "showticklabels": True, 
    "showticksuffix": "last", 
    "title": "", 
    "type": "linear", 
    "zeroline": False
    }
    layout["yaxis" + str(count)] = yaxis
    count += 1

data = Data(traces)
fig = Figure(data=data, layout=layout)
plot_url = py.plot(fig)
print(plot_url)




