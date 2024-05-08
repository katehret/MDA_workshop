####R commands for creating plots presented in Ehret and Taboada (2021). "Characterising online news comments: A multi-dimensional cruise through online registers".

####Adapted version used in the Workshop "How to do Multi-dimensional Analysis: Theoretical background and practical analyses in R" May 22 and 23, 2024, held at SFU Burnaby

#load packages
library(tidyverse)
library(ggrepel)
library(reshape2)
library(scales)
library(Hmisc)
library(hrbrthemes)

##plot factor loadings by feature

#load loadings.csv
loadings <- read.csv("data/loadings3.csv")

#use nice column labels
colnames(loadings) <- c("Feature", "Factor 1", "Factor 2", "Factor 3")

#create long data format
loadings.m <- melt(loadings, id="Feature")
names(loadings.m) <- c("Feature", "Factor", "Loading")

#plot feature loadings
load.plot <- ggplot(loadings.m, aes(reorder(Feature, Loading), abs(Loading), fill=Loading)) +
  
  facet_wrap(~ Factor, nrow=1) + 
  geom_bar(stat="identity") +
  coord_flip() +  
  scale_fill_gradient2(name = "Loading", 
                       high = rgb(0,0,0.4), mid = rgb(1,1,1), low = rgb(1,0,0), 
                       midpoint=0, guide="none") +
   scale_y_continuous(labels=c("0", ".25", ".5", ".75", "1")) +
   ylab("Loading strength") + 
   theme_bw() +
   theme(axis.title.y = element_blank(), axis.ticks.y = element_blank()) 

#ggsave("loadingsPlot.pdf", dpi=300, unit="mm", width=180)


##plot mean factor scores for registers by factor

#load data with factor scores per register
df <- read.csv("data/meanFactorScores.csv")

#add nice column names
colnames(df) <- c("Register", "Factor 1", "Factor 2", "Factor 3")

#use nice register labels
df$Register <- c("Advice", "Comments", "Description of a person", "Description for sale", "Interactive discussion", "Encyclopedia article", "FAQ", "Informational blog", "News report", "Opinion blog", "Personal blog", "Recipe", "Religious sermon", "Research article", "Review", "Sports report", "Travel blog")

#create long data format
df.m <- melt(df, id="Register")


#make bar plot
scoresPlot = ggplot(df.m, aes(x=Register, y=value, fill=value)) + 
facet_wrap(~ variable, nrow=1) + #place the factors in separate facets
geom_bar(stat="identity") + #make the bars
coord_flip() +
scale_fill_gradient2(name = "Factor score", 
	       high = rgb(0,0,0.4), mid =rgb(1,1,1), low =rgb(1,0,0), 
	       midpoint=0, guide=F) +
ylab("Mean factor score") + #improve y-axis label
theme_bw(base_size=12) + 
theme(axis.title.y = element_blank(), 
axis.ticks.y = element_blank()) 

#ggsave("registerDistribution.pdf", dpi=300, unit="mm", width=180)


#plot registers in 2-dimensional space

#reload data with factor scores per register

df1 <- read.csv("data/meanFactorScores.csv") 

dimensionScatter = ggplot(df1, aes(x=Factor1, y=Factor2)) + geom_point(size=3, col=rgb(1,0,0)) + xlab("Dimension 1: Involved vs. informational") +
ylab("Dimension 2: Informational-argumentative") +
geom_text_repel(aes(label=register), size=5) + theme_bw(base_size=12) +
theme(legend.position="none")


#ggsave("scatterplot.pdf", dpi=300, unit="mm", width=180)


#visualise data distributions with boxplots

#load factor scores

df2 = read.csv("data/factorScores.csv")


#use nice register labels

df2$register <- case_match(
  df2$register,
  "comments" ~ "Comments",
  "discussion" ~ "Discussion",
  "recipe" ~ "Recipe",
  "opinion_blog" ~ "Opinion blog",
  "advice" ~ "Advice",
  "religious_sermon" ~ "Religious sermon",
  "description_sale" ~ "Description for sale",
  "travel_blog" ~ "Travel blog",
  "sports" ~ "Sports report",
  "news" ~ "News report", 
  "faq" ~ "FAQ",
  "description_person" ~ "Description of a person",
  "informational_blog" ~ "Informational blog",
  "encyclopedia" ~ "Encyclopedia articles",
  "research"~ "Research article",
 .default = df2$register)


#add type to highlight specific register
df2 <- df2 |> mutate(type=ifelse(register=="Comments", "Highlighted", "Normal"))


#create simple boxplot Factor 1
p1 <- ggplot(df2, aes(x= register, y = Factor1, fill=type)) + 
	scale_x_discrete(guide = guide_axis(angle=90)) + #turn x-axis labels
	scale_fill_viridis_d(option="viridis") + #colour-blind friendly palette
	ylab("Involved-evaluative vs. informational") +
	theme_bw() + 
	theme(legend.position= "none") 

#plot and add mean factor scores
p1 + geom_boxplot() + stat_summary(fun=mean, colour="black", size=.5, shape=23, fill="black")


#ggsave("boxplot_F1.pdf", dpi=300, unit="mm", width=180)


#create boxplot with data points

p1 + geom_boxplot(outlier.shape=NA) + geom_jitter(width=0.2) + scale_x_discrete(guide = guide_axis(angle=90))




















