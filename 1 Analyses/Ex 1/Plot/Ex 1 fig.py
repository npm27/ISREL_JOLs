####JOL reactivity bar charts####
##set up
##load libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

#read in the data
dat = pd.read_csv("Ex 1 plot.csv")

#make the 95% confidence intervals
dat['diff'] = dat['Upper'].sub(dat['Lower']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##split by encoding
ex1 = dat[dat['Encoding'] == 'IS']
ex2 = dat[dat['Encoding'] == 'RL']
ex3 = dat[dat['Encoding'] == 'READ']

##set up the plots
ex1_fig = plt.figure()
ex1_fig.set_size_inches(8,11)

##make the subplots
ax1 = ex1_fig.add_subplot(3, 1, 1)
ax2 = ex1_fig.add_subplot(3, 1, 2)
ax3 = ex1_fig.add_subplot(3, 1, 3)

##Subset BY TASK
#IS
j1 = ex1[ex1['Measure'] == 'JOL']
r1 = ex1[ex1['Measure'] == 'RECALL']

#RL
j2 = ex2[ex2['Measure'] == 'JOL']
r2 = ex2[ex2['Measure'] == 'RECALL']

#READ
j3 = ex3[ex3['Measure'] == 'JOL']
r3 = ex3[ex3['Measure'] == 'RECALL']

#separate out averages and conf interval
##IS
j1_average = j1['Mean']
r1_average = r1['Mean']

j1_conf = j1['diff2']
r1_conf = r1['diff2']

##RL
j2_average = j2['Mean']
r2_average = r2['Mean']

j2_conf = j2['diff2']
r2_conf = r2['diff2']

##READ
j3_average = j3['Mean']
r3_average = r3['Mean']

j3_conf = j3['diff2']
r3_conf = r3['diff2']

#bar width should add to 1.
#This allows the bars to be evenly spaced
ind = np.arange(len(j1_average)) # the x locations for the groups 
width = 0.30  #how wide the bars are. Should add to 1

##make the sub plots

#set the subplot spacing
ex1_fig.subplots_adjust(hspace = .55)

names = ["Forward", "Backward", "Symmetrical", "Unrelated"]

##make the first plot
##forward vs unrelated
rects1 = ax1.bar(ind - width/2 + .125, j1_average, width, yerr = j1_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL', align = 'center')

rects2 = ax1.bar(ind + width/2 + .125, r1_average, width, yerr = r1_conf, capsize = 3, color = 'gray', edgecolor = 'k',
                label = 'Recall')


##Add labels, legend, and set tick marks
ax1.set_title('Experiment 1: Item-Specific Group', fontsize = 16, pad = 15, fontweight = "bold")
ax1.set_ylabel('Mean % Recall', fontsize = 14, fontweight = "bold")
ax1.set_xlabel('Pair Type', fontsize = 14, fontweight = "bold")
ax1.xaxis.labelpad = 7.5
ax1.set_xticks(ind + .125) ##get the tick marks centered between the bars
ax1.tick_params(axis = 'x', which = 'major', pad = 2.5) #controls how far labels are from axis
ax1.set_xticklabels(names, fontsize = 12, )
ax1.legend(fontsize = 12)
ax1.legend(frameon=False)
#box = ax1.get_position()
#ax1.set_position([box.x0, box.y0, box.width * 0.8, box.height])
#ax1.legend(bbox_to_anchor = (1.04, 0.5), loc = "center left", borderaxespad = 0, fontsize = 12)
ax1.set_ylim([0,100])

##make the second plot
rects4 = ax2.bar(ind - width/2 + .125, j2_average, width, yerr = j2_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL')

rects5 = ax2.bar(ind + width/2 + .125, r2_average, width, yerr = r2_conf, capsize = 3, color = 'gray', edgecolor = 'k',
                label = 'Recall')


##Add labels, legend, and set tick marks
ax2.set_title('Experiment 1: Relational Group', fontsize = 16, pad = 15, fontweight = "bold")
ax2.set_ylabel('Mean % Recall', fontsize = 14, fontweight = "bold")
ax2.set_xlabel('Pair Type', fontsize = 14, fontweight = "bold")
ax2.xaxis.labelpad = 7.5
ax2.set_xticks(ind + .125) ##get the tick marks centered between wthe bars
ax2.tick_params(axis = 'x', which = 'major', pad = 2.5) #controls how far labels are from axis
ax2.set_xticklabels(('Forward', 'Backward', 'Symmetrical', 'Unrelated'), fontsize = 12)
ax2.legend(fontsize = 12)
ax2.legend(frameon=False)
#box = ax2.get_position()
#ax2.set_position([box.x0, box.y0, box.width * 0.8, box.height])
#ax2.legend(bbox_to_anchor = (1.04, 0.5), loc = "center left", borderaxespad = 0, fontsize = 12)
ax2.set_ylim([0,100])

##make the third plot
rects7 = ax3.bar(ind - width/2 + .125, j3_average, width, yerr = j3_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='JOL')

rects8 = ax3.bar(ind + width/2 + .125, r3_average, width, yerr = r3_conf, capsize = 3, color = 'gray', edgecolor = 'k',
                label = 'Recall')


##Add labels, legend, and set tick marks
ax3.set_title('Experiment 1: Read Group', fontsize = 16, pad = 15, fontweight = "bold")
ax3.set_ylabel('Mean % Recall', fontsize = 14, fontweight = "bold")
ax3.set_xlabel('Pair Type', fontsize = 14, fontweight = "bold")
ax3.xaxis.labelpad = 7.5
ax3.set_xticks(ind + .125) ##get the tick marks centered between the bars
ax3.tick_params(axis = 'x', which = 'major', pad = 2.5) #controls how far labels are from axis
ax3.set_xticklabels(('Forward', 'Backward', 'Symmetrical', 'Unrelated'), fontsize = 12)
ax3.legend(fontsize = 12)
ax3.legend(frameon=False)
#box = ax3.get_position()
#ax3.set_position([box.x0, box.y0, box.width * 0.8, box.height])
#ax3.legend(bbox_to_anchor = (1.04, 0.5), loc = "center left", borderaxespad = 0, fontsize = 12)
ax3.set_ylim([0,100])

ex1_fig.savefig('EX1.jpg', dip = 10000)