# MOOC-Drop-Rate
The report performs the exploratory data analysis on the white wine dataset which records the variables of the Portuguese "Vinho Verde" wine. We are provided with some objective tests results(e.g. PH values) and the quality of the white wine, graded by the experts. What we are interested in is the factors that influence the taste of the wine. There is no missing data in this dataset which makes it easier for us to explore.

```{r packageload}
library(prettydoc)
library(data.table)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(ggpubr)
library(tidyverse)
library(knitr)
library(rbokeh)
library(ggplot2)
library(latex2exp)
wine <- read.csv("wineQualityWhites.csv", row.names = 1)
names <- colnames(wine)
```

# 1. Univariate Plots Section

First of all, let's look at the shape of the white wine dataset.
```{r dimension}
dim(wine)
```
The white wine dataset contain `r ncol(wine)` variables and `r nrow(wine)` observations. The 12 variables are: `r names`. 

In this section, we are going to use univariate plots to analyze the data. What's their distribution? Do we need to apply transformation to make the data look more "normal"? Before all that, let's take a look at the variables' meaning and some basic statistics.

The first thing we need to understand is that the quality(our response variable) is scored between between 0 (very bad) and 10 (very excellent).

The table below shows the minimum, mean, median, maximum and variance of all variables, which gives us a basic idea of the data. 
```{r summary}
Qisummary <- function(x){
  c(minimum=min(x),
  median=median(x),
  mean=mean(x),
  maximum=max(x),
  variance=var(x))
}
stats <- apply(wine,2,Qisummary) %>% t(.)
kable(stats)
```

From the variables, I noticed that there are "free sulfur dioxide" and "total sulfur dioxide" which are obviously correlated since the free sulfur dioxide is a part(probably a big part) of the total sulfur dioxide. Therefore, I create a new variable called "nonfree.sulfur.dioxide" which equals the difference of total and free sulfur dioxide.
```{r echo=TRUE}
wine$total.sulfur.dioxide <- wine$total.sulfur.dioxide - wine$free.sulfur.dioxide
colnames(wine)[7] <- "nonfree.sulfur.dioxide"
names <- colnames(wine)
```


## 1.1 Box Plots
First of all, let's look at the boxplots.
```{r cache=FALSE}
boxplots <- apply(wine, 2,
  function(x){figure(title= NULL, 
                     xlab = colnames(x),ylab=NULL) %>%
            ly_boxplot(x,breaks = 40,
                       color="skyblue",alpha=0.9, 
                       freq = FALSE)})

grid_plot(boxplots, nrow=2)
```

```{r}
table(wine$quality)
```
* As we can see, most qualities are in 5,6 and 7 and there is no data in 1,2 and 10.

* Fixed Acidity, Volatile Acidity, Critic Acid, Chlorides, Free Sulfur Dioxide, Nonfree sulfur dioxide, pH and Sulphates have many outliers. Besides, most outliers are greater than the majority.

* The residual sugar is obviously not normal distributed even after the outlier removal.

## 1.2 Outlier Detection

First let's see the frequency of the different qualities. We observed that there are only 5 cases out of 4898 row that scored 9.
```{r  echo=FALSE,results="asis"}
tbl <- table(wine$quality) %>% as.data.frame() %>% t()
table <- rbind(quality=as.numeric(tbl[1,]),
               frequency=as.numeric(tbl[2,])) 
kable(table)
```


Therefore, we should not consider the "quality" variable in our ourlier analysis since obviously a "quality 9" wine would be considered an outlier. Besides, although there are many outliers, we don't want to remove too many points for the following reasons:

* The "outliers" may not actually be an outlier and may be associated with the rarer qualities. Considering there are only 5 observations that have quality 9, and they may have some features that are very different from "normal" classes like 5,6 or 7. Therefore, by removing too many outliers we are actually removing the whole class. We want to remove an outlier because it contains huge random error which makes it hard to see the truly useful information not simply because it is too different from others.

* There are only less than 5000 observations and if we remove the 2% outliers for all 12 variables, we may end up removing 22% of the data in the worst case. 

Therefore I choose to observe the outliers of volatile acidity, residual sugars, free sulfur dioxide, chlorides.

```{r}
grid.arrange(ggplot(wine, aes( x = 1, y = volatile.acidity ) ) + 
               geom_jitter(alpha = 0.1,color="lightsteelblue4") +
               geom_boxplot(alpha = 0.2,color="navyblue"),
             ggplot(wine, aes( x = 1, y = residual.sugar ) ) + 
               geom_jitter(alpha = 0.1,color="lightsteelblue4") +
               geom_boxplot(alpha = 0.2, color="navyblue"),
             ggplot(wine, aes( x = 1, y = free.sulfur.dioxide ) ) + 
               geom_jitter(alpha = 0.1,color="lightsteelblue4") +
               geom_boxplot(alpha = 0.2, color="navyblue"),
             ggplot(wine, aes( x = 1, y = chlorides ) ) + 
               geom_jitter(alpha = 0.1,color="lightsteelblue4") +
               geom_boxplot(alpha = 0.2, color="navyblue"),
             ncol=2)
```

After looking at this, I realized that there are many decide to remove the upper 1% and the lower 0.5% of the data because more outliers have greater values than the majority.

```{r}
####outliers (except quality)
outlierbound <- apply(wine[c("volatile.acidity","residual.sugar",
                       "free.sulfur.dioxide","chlorides")],2,
               function(x)quantile(x, probs=c(.01, .99))) %>% as.data.frame()
kable(outlierbound)

```

Next, we remove all the outliers.

```{r}
wine <- wine %>% filter(volatile.acidity>outlierbound[1,1] & volatile.acidity<outlierbound[2,1]) %>% 
                 filter(residual.sugar>outlierbound[1,2] & residual.sugar<outlierbound[2,2]) %>% 
                 filter(free.sulfur.dioxide>outlierbound[1,3] & free.sulfur.dioxide<outlierbound[2,3]) %>% 
                 filter(chlorides>outlierbound[1,4] & chlorides<outlierbound[2,4])
```

Now, there are `r nrow(wine)` left, which is acceptable.

## 1.3 Histgrams

Now we have removed all the outliers and we can observe the histgrams. Below are the 12 histgram plots of 12 variables.

```{r fig.align='center',cache=FALSE}
histgrams <- apply(wine, 2,
                   function(x){
                       figure(title= NULL, xlab = colnames(x), 
                              width = 400, height = 250) %>%
                       ly_hist(x,breaks = 40, freq = FALSE, 
                               color=brewer.pal(9, "GnBu")) %>%
                       ly_density(x)})

grid_plot(histgrams, nrow=6)
```

Still, as we have seen before, the residual sugar needs to be transformed because it is heavily right-skewed.
Thus, I decide to perform the log transformation on this variable.
```{r fig.align='center',cache=FALSE}
sugar.log <- wine$residual.sugar %>% log
figure(title= "Log transform of residual sugar", 
       xlab = "residual sugar") %>%
  ly_hist(sugar.log,breaks = 40, freq = FALSE, 
          color=brewer.pal(9, "PuRd")) %>%
  ly_density(sugar.log)
```

From the plot, we can see clearly that this is a Bimodal Distribution with 2 peaks, which is a mixture of data from 2 normal distributions.  

# 2.Univariate Analysis Section

* I created a new variable called "nonfree.sulfur.dioxide" due to the obvious dependency between "free.sulfur.dioxide" and "total.sulfur.dioxide"

* The most important variable is our target variable: the quality of the wine. There are totally 7 qualities that has appeared in this dataset: 3,4,5,6,7,8 and 9. About 92.5% data falls in 5,6,7. 

* Many data are right-skewed. I performed a log-transformation on the "residual.sugar" variable.

* Right now, there's no clear information about the importance of each variables except the quality.


# 3.Bivariate Plots Section

In this section, we are going to explore the relations between each variable. Especially the difference between each qualities.

## 3.1 Mean in Each Group

First of all, let's look at the mean of each variable within each group.

```{r mean}
group.mean <- wine %>% group_by(quality) %>%
  summarise_all(funs(mean)) %>% 
  t %>% round(2)  
kable(group.mean)
```

What I immediately noticed is that the mean values of alchohol, free.sulfur.dioxide and nonfree.sulfer.dioxide have a tendency of increase as the quality becomes better. Besides, the mean values of the density in different groups are always all the same.(we can also find this from the histgrams and boxplot sections) 

However, in most cases, the extreme value may lead to a extremely great result or a very bad one. For example, the mean value of fixed acidity is gradually decrease from quality 3 to quality 8. However, in quality 9, it suddenly changed to a very high value.

That means for most values, it's not simply "the more the better" or "the less the better". 

## 3.2 Correlation Plot

Below is a Pearson correlation Plot of all 12 variables. The darker square means there are stronger correlation between two variables.

```{r fig.align='center',cache=FALSE}
##cor plot
library(ggcorrplot)
corr <- round(cor(wine,method = "pearson"), 2)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_col = "black",
           outline.color = "white",
           colors = c("skyblue",  "white","skyblue"),
           lab_size = 3, 
           method="square", 
           show.legend = TRUE, legend.title = "correlation", 
           title="Correlarion Plot", 
           ggtheme=theme_bw)

```

From the plot we can see some strong correlation pairs.

* Density has a strong correlation with almost every variable, which is counterintuitive since its value seems to be almost all the same. This is probality because I rounded the mean to 2 digits.

* Fixed.acidity has a strong positive correlation with citric.acid and a strong negative correlation with pH.

* Alcohol has the strongest positive with quality and a strong negative correlation with density. 


## 3.3 Box Plots
From last section we have observed that residual sugar, alcohol, density and pH may have strong effect on the taste of the wine. Now let's take a deep look at it.
```{r}
color=c("#f1eef6","#d0d1e6","#a6bddb","#74a9cf","#3690c0","#0570b0","#034e7b")
```


```{r cache=FALSE}
bp1 <- ggplot(data=wine, aes(x=as.factor(quality),y=volatile.acidity)) +
  geom_boxplot(fill=color,alpha=0.5) +
  stat_summary(fun.y=mean,geom="point")+
  labs(x="quality")

bp2 <- ggplot(data=wine, aes(x=as.factor(quality),y=alcohol)) +
  geom_boxplot(fill=color,alpha=0.5) +
  stat_summary(fun.y=mean,geom="point")+
  labs(x="quality")

bp3 <- ggplot(data=wine, aes(x=as.factor(quality),y=density)) +
  geom_boxplot(fill=color,alpha=0.5) +
  stat_summary(fun.y=mean,geom="point")+
  labs(x="quality")

bp4 <- ggplot(data=wine, aes(x=as.factor(quality),y=nonfree.sulfur.dioxide)) +
  geom_boxplot(fill=color,alpha=0.5) +
  stat_summary(fun.y=mean,geom="point")+
  labs(x="quality")

grid.arrange(bp1,bp2,bp3,bp4, nrow=2)
```



# 4. Bivariate Analysis

From the correlation plot and the boxplots, we've found some strong relations between selected variables. Thus we do a regression analysis between them.

```{r echo=TRUE}
x <- c("residual.sugar","alcohol","fixed.acidity","fixed.acidity")
y <- c("density","density","citric.acid","pH")
model <- list()
for (i in 1:4){model[[i]] <- lm(wine[[y[i]]] ~ wine[[x[i]]])}
```

## 4.1

In the first plot, we can see clearly that the amount of sugar remaining after fermentation stops has a strong relation with the density. What's more, it seems that we can find two straight lines to separate the three different color of quality 5,6 and 7. It seems that the lower density and higher residual sugar can lead to better quality.(We will have to explore this separation in next section.)

Based on the $R^2$ the residual.sugar can explain 70% variance in density.

```{r echo=TRUE}
summary(model[[1]])
```

## 4.2

From the second plot, we see that there is a clear negative correlation between alcohol and density. This matches our intuition.

Admittedly, in most cases we cannot assume the cause and result between variables. However, as we all know, the density of the alcohol is smaller than the density of water. Thus, the increasing percent alcohol content of the wine can definately lead to a lower density. 

```{r echo=TRUE}
summary(model[[2]])
```

## 4.3

From the plot, it seems that there may be a positive correlation between fixed acidity and citric acid. The linear regression model does confirm my guess. With a P-value that is almost 0, there are definately some positive relation between these two variables.

```{r echo=TRUE}
summary(model[[3]])
```

## 4.4

There is a clearly negative correlation between fixed acidity and the pH value.

```{r echo=TRUE}
summary(model[[4]])
```

# 5.Multivariate Plots Section


First, let's look at some of the interesting scatter plots. The different colors represent different qualities.
```{r fig.align="center"}


sp1 <- ggplot(data=wine,aes(x = wine[[x[1]]],
                           y = wine[[y[1]]], 
                           color = factor(quality))) +
                       geom_point(alpha = 0.8, size = 1) +
                       geom_smooth(method = "lm", 
                                   se = FALSE,size=1)  +
                       scale_color_brewer(type='seq',palette = 'Blues',
                                          guide=guide_legend(title='Quality'))+
                       labs(x=x[1],y=y[1])+
                       theme_dark()
  
sp2 <- ggplot(data=wine,aes(x = wine[[x[2]]],
                                          y = wine[[y[2]]], 
                                          color = factor(quality))) +
                       geom_point(alpha = 0.8, size = 1) +
                       geom_smooth(method = "lm", 
                                   se = FALSE,size=1)  +
                       scale_color_brewer(type='seq',palette = 'GnBu',
                                          guide=guide_legend(title='Quality'))+
                       labs(x=x[2],y=y[2])+
                       theme_dark()
sp3 <- ggplot(data=wine,aes(x = wine[[x[3]]],
                          y = wine[[y[3]]], 
                          color = factor(quality))) +
                        geom_point(alpha = 0.8, size = 1) +
                        geom_smooth(method = "lm", 
                        se = FALSE,size=1)  +
                      scale_color_brewer(type='seq',palette = 'GnBu',
                       guide=guide_legend(title='Quality'))+
                      labs(x=x[3],y=y[3])+
                       theme_dark()
sp4 <- ggplot(data=wine,aes(x = wine[[x[4]]],
                          y = wine[[y[4]]], 
                          color = factor(quality))) +
              geom_point(alpha = 0.8, size = 1) +
              geom_smooth(method = "lm", 
               se = FALSE,size=1)  +
              scale_color_brewer(type='seq',palette = 'Blues',
                     guide=guide_legend(title='Quality'))+
              labs(x=x[4],y=y[4])+
                       theme_dark()
grid.arrange(sp1,sp2,sp3,sp4, nrow=2)
```


# 6.Multivariate Analysis

From the 3 plots above, we can see that there are relationships between them and the quality. So we can build a linear regression model to predict the quality of the wine.

```{r echo=TRUE}
model.new <- lm(quality~alcohol+density+ residual.sugar, data=wine)
summary(model.new)
```
Unfortunately, this is not a very satisfying result, the $R^2$ is only 0.2 meaning only 20% of the variance of the quality can be explained by the 3 variables.

# 7.Final Plots & Summary

There are three final plots that I have chosen which can show the most interesting things that I have found.

## 7.1 First Plot
```{r fig.align='center',cache=FALSE}

ggplot(data=wine,aes(x=log(residual.sugar)))+
  geom_histogram(fill="skyblue",color="white",alpha=0.9,binwidth = 0.05) +
  labs(x=TeX('Residual Sugar g/$dm^3$')) +
  ggtitle( "Log transform of residual sugar") +
  theme(plot.title = element_text(hjust = 0.5))+theme_dark()

```

The distribution of the log transformation of residual sugar turns out to be bimodal. From later analysis we find out that the mean residual sugar of the best and worst quality are relatively low and the those in the middle tend to have higher residual sugar. This indicates that the sweeter taste of white wine may be a "safe card".

## 7.2 Second Plot
```{r fig.align='center',cache=FALSE}
ggplot(data=wine, xlab="Quality", ylab="Alcohol [%]",
       aes(x=factor(quality),y=alcohol,fill=factor(quality))) +
  geom_boxplot(color="white",alpha=0.9) +
  labs(x="quality",y="Alcohol [%]") + 
  scale_fill_manual(values=color,
                    name="Quality") +
  ggtitle("Boxplot of Alcohol Concentration in Different Qualities") +
  theme(plot.title = element_text(hjust = 0.5))+theme_dark()
```

The second plot is the boxplot of the alcohol within each group. Apparently  the quality of the white wine improved significantly as the alcohol content becomes higher.

## 7.3 Third Plot
```{r fig.align='center',cache=FALSE}

ggplot(data=wine,aes(x = wine[,1],
                  y = wine[,2], 
                  color = factor(quality))) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", 
              se = FALSE,size=1)  +
  scale_color_brewer(type='seq',palette = 'Blues',
                     guide=guide_legend(title='Quality'))+
  xlab(TeX('Residual Sugar g/$dm^3$'))+
  ylab(TeX('Density g/$cm^3$'))+ggtitle("Scatter Plot of Residual Sugar and Density")+theme(plot.title = element_text(hjust = 0.5))+
  theme_dark()
```

The third plot is the scatter plot of the density against residual.sugar. Within the majority group(5,6 and 7), we see that the wine with better quality usally have higher alcohol content and lower density. 

## 8. Reflection

In this report, I first explored each variable individually. Then I tried to capture the relationships between the variables. In the end, I try to find the factors that have the most influence on the wine quality.

From my final plot 3, we can see that there is a clear linear relationship between density and the residual sugar. What's more important, we can see clearly that with higher residual sugar and lower density, the quality of the wine tends to increase. (I mainly ovbserve wines with quality 5,6,7 and 8, since there are not enough observations for wine with quality 3,4 and 9.)

Although it seems to be a classification problem, I believe it would be more appropriate to perform regression analysis on this dataset because the quality of the wine is actually nomial scale. However, the model may be a little bit more complicated than multivariate linear regression model. Therefore, I  plan to use some generilized linear models to further explore the relationships between the 11 features and the taste of the wine.

Besides, I also have made an interactive presentation file using R shiny for readers to explore the distribution of each variables and the correlation between varibles by themselves. Here's the [link]:https://louischoki.shinyapps.io/QiPresen/#1.

In the multivariate section, I originally wanted to plot the heatmap. However, because there are only 4000 data and they are not very concentrated, the heatmap looks very sparse and is hard for me to find any interesting information.
