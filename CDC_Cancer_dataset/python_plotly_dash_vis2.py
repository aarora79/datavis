"""
HW1, Python plotly desktop example
======
CDC cancer dataset 1999-2012, https://drive.google.com/file/d/0B4RXVYeUUKitZUdYeFdhd2t0d0U/view
"""
import dash
import dash_core_components as dcc
import dash_html_components as html
import plotly.graph_objs as go
import pandas as pd
import numpy as np

#globals
app = dash.Dash() #initialize the plotly dash app as a global variable

def read_cancer_data(race):
    """Reads the dataset into a pandas dataframe. Filters out the entries for 
       the race provided as input to this function. Groupts the data by 
       cancer and then age group to create a vis. The idea is to explore cancer
       type by age group in the context of any given Race.

       Parameters
       ---------- 
       race (string): the race for the cancer type by age group relationship
                      is to be visualized.

        No error checking is being done by this function.
    """
    # Load the example iris dataset
    cancer_df = pd.read_csv('USA_Cancer_Stats_1999_2012_CDC_orgSite_New.csv')

    #filter out columns of intrest
    cancer_df = cancer_df[['LeadingCancerSites', 'AgeGroupCode', 'Sex', 'Race', 'Count', 'State']]

    #filter out the rows specific to this race
    cancer_filtered_df = cancer_df[cancer_df['Race'] == race]

    #group by Race and Cancer Sites
    cancer_filtered_grouped_df = cancer_filtered_df.groupby(['LeadingCancerSites', 'AgeGroupCode'])

    #now convert to a format amenable to visualization
    #we want data as "Race, LeadingCancerSites, Count"
    cancer_filtered_summarized_df = cancer_filtered_grouped_df['Count'].sum()
    number_of_records = len(cancer_filtered_summarized_df.index)
    cancer   = []
    agegroup = []
    count    = []
    for i in range(number_of_records):
        c = cancer_filtered_summarized_df.index[i][0]
        a = cancer_filtered_summarized_df.index[i][1]

        cancer.append(c)
        agegroup.append(a)

        #create a group tuple as we want to normalize this aggregated count over 100,000
        g = (c, a)
        total_normalized_population_count = len(cancer_filtered_grouped_df.get_group(g))

        count.append(cancer_filtered_summarized_df.values[i]/total_normalized_population_count)

    #create a new dataframe now that we have individual columns as arrays
    cancer_filtered_grouped_df = pd.DataFrame({'cancer':cancer, 'AgeGroupCode': agegroup, 'count':count})
    print(cancer_filtered_grouped_df)
    return cancer_filtered_grouped_df 

def set_plotly_dashlayout(df):
    """Sets the plotly dash data structure for layout.
       This is needed before the plotly dash run function is called.
       The vis is a bubble chart showing Cancer Vs age group with the size of
       the bubble representing the number of people per N (typically 1000)
       in that race impacted by a type of cancer. The color of the bubble
       represents the type of the cancer.

       Parameters
       ---------- 
       df (pandas dataframe): dataframe containing data for a particular race. 
                              The race data is already assumed to be filtered by the
                              read_cancer_data function.

        No error checking is being done by this function.
    """
    title = '<b>Cancer data from 1999 to 2012 for ' + race + ' population</b>'
    subtitle = '<b>Source:</b> CDC Cancer dataset, https://drive.google.com/file/d/0B4RXVYeUUKitZUdYeFdhd2t0d0U/view'
    title += "<br>" + subtitle
    app.layout = html.Div([
        dcc.Graph(
            id='race-cancer-agegroup',
            figure={
                'data': [
                    go.Scatter(
                        x=df[df['cancer'] == i]['cancer'],
                        y=df[df['cancer'] == i]['AgeGroupCode'],
                        text=round(df[df['cancer'] == i]['count']),
                        mode='markers',
                        opacity=0.7,
                        marker=dict(
                            symbol='circle',
                            sizemode='diameter',
                            sizeref=1,
                            size=np.sqrt(df[df['cancer'] == i]['count']), #tried several scaling mechanism, 
                                                                          #found sqrt to work best for this dataset
                            line=dict(
                            width=2
                            ),
                        ),
                        name=i
                    ) for i in df.cancer.unique()
                ],
                'layout': go.Layout(
                    title = title,
                    titlefont=dict(
                family='Arial',
                size=11,
                color='#7f7f7f'
            ),
                    xaxis={'type': 'category', 'title': 'Race'},
                    yaxis={'type': 'category', 'title': 'Age Group'},
                    margin={'l': 60, 'b': 40, 't': 30, 'r': 10},
                    annotations=[
            dict(
                x=5,
                y=5.5,
                xref='paper',
                yref='paper',
                text='Source: CDC Cancer dataset, https://drive.google.com/file/d/0B4RXVYeUUKitZUdYeFdhd2t0d0U/view',
                showarrow=False,
                font=dict(
                    family='Courier New, monospace',
                    size=18,
                    color='#ffffff'
                ),
                align='center',
                xanchor='right',
                    yanchor='bottom',
                
                bordercolor='#c7c7c7',
                borderwidth=2,
                borderpad=4,
                bgcolor='#ff7f0e',
                opacity=0.8
            )
        ],
                    #legend={'orientation': 'h'},
                    hovermode='closest'
                )
            }
        )
    ])

if __name__ == '__main__':
    #read the data
    race = 'Black or African American'
    df = read_cancer_data(race)
    
    #set the plotly dash layout
    set_plotly_dashlayout(df)

    #ready to run the server (the vis in the browser at http://127.0.0.1:8050/)
    app.run_server()