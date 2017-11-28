"""
HW1, Python seaborn example
======
CDC cancer dataset 1999-2012, https://drive.google.com/file/d/0B4RXVYeUUKitZUdYeFdhd2t0d0U/view
"""
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import sys

#globals
SAMPLE_SIZE = 100000 #all data is age adjusted count over a population size of 100,000

def draw_vis():
    """Initializes seaborn, reads the dataset into a pandas DataFrame
       and creates the visualization.

       The visualization is a summarized representation of the original
       data showing count of various cancer sites grouped by race.

       No error checking done in this function!
    """
    #intiialize seaborn for visualization
    sns.set(style="whitegrid", palette="muted")

    # Load the cancer dataset
    cancer_df = pd.read_csv('USA_Cancer_Stats_1999_2012_CDC_orgSite_New.csv')
    print(len(cancer_df))
    #filter for a particular state, say California
    cancer_df = cancer_df[cancer_df['State'] == 'California']
    print(len(cancer_df))

    #filter out columns of intrest
    cancer_df = cancer_df[['LeadingCancerSites', 'AgeGroup', 'Sex', 'Race', 'Count']]

    #group by Race and Cancer Sites
    race_cancer_grouped = cancer_df.groupby(['Race', 'LeadingCancerSites'])
    
    #now convert to a format amenable to visualization
    #we want data as "Race, LeadingCancerSites, Count"

    race_cancer_summarized = race_cancer_grouped['Count'].sum()
    
    number_of_records = len(race_cancer_summarized.index)
    race   = []
    cancer = []
    count  = []
    for i in range(number_of_records):
        r = race_cancer_summarized.index[i][0]
        c = race_cancer_summarized.index[i][1]

        #now append it to the array so that we can use it to create a new dataframe
        race.append(r)
        cancer.append(c)

        #create a group tuple as we want to normalize this aggregated count over 100,000
        g = (r, c)
        total_normalized_population_count = len(race_cancer_grouped.get_group(g))
        count.append(race_cancer_summarized.values[i]/total_normalized_population_count)

    #create a new dataframe now that we have individual columns as arrays
    race_cancer_grouped = pd.DataFrame({'Race':race, 'Cancer':cancer, 'Count':count})
    print(race_cancer_grouped)

    # Draw a categorical barplot to show each observation
    p = sns.barplot(x="Race", y="Count", hue="Cancer", data=race_cancer_grouped)
    
    p.set(ylabel='Count (per 100,000)')
    p.figure.suptitle('Cancer type by Race, CDC data 1999-2012\n State of California', fontsize=14, fontweight='bold')
 
    plt.show()
    #p.figure(figsize=(16,12))
    p.figure.set_size_inches(13,6) 
    fname = "race_cancer_grouped.png"
    p.figure.savefig(fname, format='png', dpi=1200) #save the plot to a file
    print("saved plot to %s" %(fname))

if __name__ == '__main__':
    #call the function to create the visualization
    print("Going to create visualization using seaborn")
    draw_vis()
