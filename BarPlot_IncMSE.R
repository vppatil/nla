#This file creates a bar plot of both IncMSE and the effect size
#for the top five predictor variables for each water quality parameter
require(ggplot2)
require(reshape)



#create a bar plot for the effect size
data = read.csv("partialPlotData.csv", header = TRUE)
setwd( "/Users/Samantha/Dropbox/nla analyses (1)/output")
pdf("RatioSummary.pdf")
ggplot(data, aes(x=Predictor_Variable, y=Ratio, fill=Response_Variable)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family="Helvetica")) +
  scale_fill_grey(start = 0.8, end = .1)
dev.off()

#-----------------#
#create a bar plot for %IncMSE
setwd("/Users/Samantha/Dropbox/nla analyses (1)/data")
data = read.csv("percentMSEIncData.csv", header = TRUE)
setwd( "/Users/Samantha/Dropbox/nla analyses (1)/output")
pdf("IncMSESummary.pdf")
ggplot(data, aes(x=Predictor_Variable, y=Percent_inc_MSE, fill=Response_Variable)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family="Helvetica")) +
  scale_fill_grey(start = 0.8, end = .1)
dev.off()
