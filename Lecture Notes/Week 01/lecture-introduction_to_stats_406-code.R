### R code from vignette source '/home/mark/Documents/Teaching/sjtu/stats406-summer2021/lectures/week01/introduction_to_stats_406/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:121-126
###################################################
sample_max_mag <- function(n) {
    x <- rnorm(n)
    max(abs(x))
}
empirical_distrib <- replicate(100000, sample_max_mag(3))


###################################################
### code chunk number 3: lecture-core.Rnw:132-134
###################################################
# par(mar = c(4,4,2,2))
ggplot(data = data.frame(x = empirical_distrib), aes(x = x)) + geom_histogram() + labs(x = "Max Mag")# (empirical_distrib, breaks = 100, main = NA, xlab = "Harmonic Mean")


###################################################
### code chunk number 4: lecture-core.Rnw:141-142
###################################################
mean(empirical_distrib); var(empirical_distrib)


###################################################
### code chunk number 5: lecture-core.Rnw:146-147
###################################################
quantile(empirical_distrib, c(0.25, 0.5, 0.75))


###################################################
### code chunk number 6: lecture-core.Rnw:354-363
###################################################
# Create dataset
maxes <- c(rep(0.5, 10), rep(0.6, 30), rep(0.75, 14), rep(1, 6))
data=data.frame(
  individual= replicate(60, paste(sample(LETTERS, 6), collapse = "")),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value1 = 50 * c(runif(10, 0, 1), runif(30, 1.5, 2.5), runif(14, 0, 1), runif(6, 0, 1)),
  value2 = 50 * c(runif(10, 1, 2), runif(30, 0, 1), runif(14, 0, 1), runif(6, 0, 1)),
  value3 = 50 * c(runif(10, 0, 1), runif(30, 0, 1), runif(14, 0, 1), runif(6, 2, 3)))
data[c(1:2, 11:12, 55:56),]


###################################################
### code chunk number 7: lecture-core.Rnw:369-440
###################################################
# library
library(tidyverse)
library(viridis)
 
 
# Transform data in a tidy format (long format)
data = data %>% gather(key = "observation", value="value", -c(1,2)) 
 
# Set a number of 'empty bar' to add at the end of each group
empty_bar=2
nObsType=nlevels(as.factor(data$observation))
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar*nObsType )
data=rbind(data, to_add)
data=data %>% arrange(group, individual)
data$id=rep( seq(1, nrow(data)/nObsType) , each=nObsType)
 
# Get the name and the y position of each label
label_data= data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)
 
# prepare a data frame for base lines
base_data=data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
 
# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]
 
# Make the plot
p = ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
print(p)


