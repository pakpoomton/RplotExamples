##### 1. Load relevant library #####
library(ggplot2)
library(dplyr)



##### 2. Set working directory and load data #####
setwd("/Users/pakpoomton/Desktop/Career_Research/Project_สวก63zerg_Ongoing/dataAnalysis/")

# read csv file into an R's data frame
dataTable <- read.csv(file = 'MoinaGrowthData030321.csv')



##### 3. Select only a relevant subset of data #####
# remove treatment with PGPR without Foc
dataTable <- dataTable[ (dataTable$day %in% 
                                  c("0","2", "4", "7")), ]
dataTable <- dataTable[ (dataTable$feed %in% 
                           c("yeast")), ]



##### 4. Create plot setup #####
plotsetup =   theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 14)) +
  theme(axis.text.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 14)) +   
  theme(panel.background = element_rect(fill = "white", colour = NA)) +
  theme(axis.line.x = element_line(size=1,color = "black", linetype="solid")) +
  theme(axis.line.y = element_line(size=1,color = "black", linetype="solid")) +
  theme(axis.title.y = element_text(size=14,color = "black")) +  
  theme(panel.grid.major.y = element_line(size=0.2,color = "black", linetype="solid")) +
  theme(legend.title = element_blank()) 



##### 5. Generate a plot for raw data #####
ggplot() +
  geom_point(data = dataTable, 
             aes(x=day,y=weight.gram),
             alpha = 0.5, size=5, color = 'black', position=position_jitter(h=0.0,w=0.15)) +
  labs(x = "time (day)", y = "weight (g)",  title = dataTable$feed[1], size = 11) +
  plotsetup



##### 6. Calculate summary statistics #####
dataMeanSD   = dataTable %>% group_by(day) %>% summarise(meanVal = mean(weight.gram), 
                                                  sdVal = sd(weight.gram))



##### 7. Generate a plot for summary statistics ####
#>>> scatter plot
ggplot(data = dataMeanSD, aes(x=day, y=meanVal)) +
  geom_point(alpha = 0.5, size =5, color = 'black') + 
  geom_errorbar(aes(ymin=meanVal-sdVal,
                    ymax=meanVal+sdVal), width=.1) +
  plotsetup+
  labs(x = "time (day)", y="weight (g)", title = dataTable$feed[1]) 


#>>> bar plot
ggplot(data = dataMeanSD, aes(x=day, y=meanVal)) +
  geom_bar(stat = "identity", alpha = 0.5, color = 'black') + 
  geom_errorbar(aes(ymin=meanVal-sdVal,
                    ymax=meanVal+sdVal), width=.1) +
  plotsetup+
  labs(x = "time (day)", y="weight (g)", title = dataTable$feed[1]) 



