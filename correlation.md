# Correlation

---

```{r heatmap-correlation-intro-image, echo=FALSE, fig.align='center', fig.cap="Heatmap Normalization", out.width = '50%'}
knitr::include_graphics("https://www.r-graph-gallery.com/215-the-heatmap-function_files/figure-html/thecode2-1.png")
```


## Bubble Plot

---

A [bubble plot](https://www.data-to-viz.com/graph/bubble.html) is a [scatter plot](https://www.r-graph-gallery.com/scatterplot.html) with a third numeric variable mapped to circle size. This page describes several methods to build one with R. 

#### A Bubble Chart is a Scatterplot

A bubble chart is basically a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) with a third numeric variable used for circle size. Thus, remember all the tips described in the [scatterplot section](https://www.r-graph-gallery.com/scatterplot.html) also apply here.

#### Step by Step with `ggplot2`

`ggplot2` allows to create bubble chart thanks to the `geom_point()` function. Next examples will lead you through the process step by step:

### Most Basic bubble Chart with `geom_point()`

A [bubble plot](https://www.r-graph-gallery.com/bubble-chart.html) is a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) where a third dimension is added: the value of an additional numeric variable is represented through the size of the dots. (source: [data-to-viz](https://www.data-to-viz.com/graph/bubble.html)).

With [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html), bubble chart are built thanks to the `geom_point()` function. At least three variable must be provided to `aes()`: **x**, **y** and **size**. The legend will automatically be built by ggplot2.

Here, the relationship between life expectancy (`y`) and gdp per capita (`x`) of world countries is represented. The population of each country is represented through circle size.

```{r bubble-plot-correlation-ggplot, echo=TRUE, message=FALSE, warning=FALSE}
# Libraries
library(ggplot2)
library(dplyr)
# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)
# Most basic bubble plot
ggplot(data, aes(x=gdpPercap, y=lifeExp, size = pop)) +
    geom_point(alpha=0.7)
```

### Control Circle Size with `scale_size()`

The first thing we need to improve on the previous chart is the bubble size. `scale_size()` allows to set the size of the smallest and the biggest circles using the `range` argument. Note that you can customize the legend name with `name`.

<u>Note</u>: circles often overlap. To avoid having big circles on top of the chart you have to reorder your dataset first, as in the code below.
 
<u>ToDo</u>: give more details about how to map a numeric variable to circle size. Use of `scale_radius`, `scale_size` and `scale_size_area`. [See this post](https://www.data-to-viz.com/caveat/radius_or_area.html).

```{r bubble-plot-correlation-ggplot-control-circle-size, echo=TRUE, message=FALSE, warning=FALSE}
# Libraries
library(ggplot2)
library(dplyr)
# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)
# Most basic bubble plot
data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size = pop)) +
    geom_point(alpha=0.5) +
    scale_size(range = c(.1, 24), name="Population (M)")
```

### Add a Fourth Dimension: Color

If you have one more variable in your dataset, why not showing it using circle color? Here, the continent of each country is used to control circle color:

```{r bubble-plot-correlation-ggplot-color-fourth-dimension, echo=TRUE, message=FALSE, warning=FALSE}
# Libraries
library(ggplot2)
library(dplyr)
# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)
# Most basic bubble plot
data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent)) +
    geom_point(alpha=0.5) +
    scale_size(range = c(.1, 24), name="Population (M)")
```

### Make it Pretty

A few classic improvement:

* Use of the `viridis` package for nice color palette.
* Use of `theme_ipsum()` of the `hrbrthemes` package.
* Custom axis titles with `xlab` and `ylab`.
* Add stroke to circle: change `shape` to 21 and specify `color` (stroke) and `fill`.

```{r bubble-plot-correlation-ggplot-improvements, echo=TRUE, message=FALSE, warning=FALSE}
# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)
# Most basic bubble plot
data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, fill=continent)) +
    geom_point(alpha=0.5, shape=21, color="black") +
    scale_size(range = c(.1, 24), name="Population (M)") +
    scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
    theme_ipsum() +
    theme(legend.position="bottom") +
    ylab("Life Expectancy") +
    xlab("Gdp per Capita") +
    theme(legend.position = "none")
```

### Interactive Version

Here is an interactive bubble chart built in R, thanks to the `ggplotly()` function of the `plotly` library. Try to hover circles to get a tooltip, or select an area of interest for zooming. Double click to reinitialize.

#### Interactive Bubble Chart

This section explains how to build an interactive [bubble chart](https://www.r-graph-gallery.com/bubble-chart.html) with R, using `ggplot2` and the `ggplotly()` function of the plotly package.

#### Most Basic Bubble Chart with `geom_point()`

This section follows the previous [step by step description](https://www.r-graph-gallery.com/320-the-basis-of-bubble-plot.html) of building [bubble chart](https://www.r-graph-gallery.com/bubble-chart.html) with [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html).

The idea is to turn the chart interactive:

* You can zoom by selecting an area of interest
* Hover a circle to get information about it
* Export to png
* Slide axis
* Double click to re-initialize.

This is done thanks to the `ggplotly()` function of the `plotly` package that turn any ggplot2 chart object interactive. Note the little trick to custom the tooltip content.

```{r bubble-plot-correlation-ggplot-interactive, echo=TRUE, message=FALSE, warning=FALSE}
# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)
# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)
# Interactive version
p <- data %>%
  mutate(gdpPercap=round(gdpPercap,0)) %>%
  mutate(pop=round(pop/1000000,2)) %>%
  mutate(lifeExp=round(lifeExp,1)) %>%
  
  # Reorder countries to having big bubbles on top
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  
  # prepare text for tooltip
  mutate(text = paste("Country: ", country, "\nPopulation (M): ", pop, "\nLife Expectancy: ", lifeExp, "\nGdp per capita: ", gdpPercap, sep="")) %>%
  
  # Classic ggplot
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent, text=text)) +
    geom_point(alpha=0.7) +
    scale_size(range = c(1.4, 19), name="Population (M)") +
    scale_color_viridis(discrete=TRUE, guide=FALSE) +
    theme_ipsum() +
    theme(legend.position="none")
# turn ggplot interactive with plotly
pp <- ggplotly(p, tooltip="text")
pp
# save the widget
# library(htmlwidgets)
# saveWidget(pp, file=paste0( getwd(), "/HtmlWidget/ggplotlyBubblechart.html"))
```

## Connected Scatterplot

---

Welcome to the [connected scatterplot](https://www.data-to-viz.com/graph/connectedscatter.html) section of the gallery. If you want to know more about this kind of chart, visit [data-to-viz.com](https://www.data-to-viz.com/graph/connectedscatter.html). If you're looking for a simple way to implement it in R and `ggplot2`, pick an example below.

### Connected Scatterplot with R and Ggplot2

This section explains how to build a basic [connected scatterplot](https://www.r-graph-gallery.com/connected-scatterplot.html) with R and [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html). It provides several reproducible examples with explanation and R code.

#### Most Basic Connected Scatterplot: `geom_point()` and `geom_line()`

A [connected scatterplot](https://www.r-graph-gallery.com/connected-scatterplot.html) is basically a hybrid between a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) and a line plot. Thus, you just have to add a `geom_point()` on top of the `geom_line()` to build it.

```r
# Libraries
library(ggplot2)
library(dplyr)
# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)
# Plot
data %>%
  tail(10) %>%
  ggplot( aes(x=date, y=value)) +
    geom_line() +
    geom_point()
```

![](https://www.r-graph-gallery.com/connected_scatterplot_ggplot2_files/figure-html/thecode-1.png)

### Customize the Connected Scatterplot

Custom the general theme with the `theme_ipsum()` function of the hrbrthemes package. Add a title with `ggtitle()`. Custom circle and line with arguments like `shape`, `size`, `color` and more.

```r
# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)
# Plot
data %>%
  tail(10) %>%
  ggplot( aes(x=date, y=value)) +
    geom_line( color="grey") +
    geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
    theme_ipsum() +
    ggtitle("Evolution of bitcoin price")
```

![](https://www.r-graph-gallery.com/connected_scatterplot_ggplot2_files/figure-html/thecode2-1.png)

### Connected Scatterplot to show an Evolution

The [connected scatterplot](https://www.r-graph-gallery.com/connected-scatter.html) can also be a powerfull technique to tell a story about the evolution of 2 variables. Let???s consider a dataset composed of 3 columns:

* Year
* Number of baby born called Amanda this year
* Number of baby born called Ashley

The scatterplot beside allows to understand the evolution of these 2 names. Note that the code is pretty different in this case. `geom_segment()` is used of `geom_line()`. This is because `geom_line()` automatically sort data points depending on their X position to link them.


```r
# Libraries
library(ggplot2)
library(dplyr)
library(babynames)
library(ggrepel)
library(tidyr)
# data
data <- babynames %>% 
  filter(name %in% c("Ashley", "Amanda")) %>%
  filter(sex=="F") %>%
  filter(year>1970) %>%
  dplyr::select(year, name, n) %>%
  spread(key = name, value=n, -1)
# plot
data %>% 
  ggplot(aes(x=Amanda, y=Ashley, label=year)) +
     geom_point() +
     geom_segment(aes(
                    xend=c(tail(Amanda, n=-1), NA), 
                    yend=c(tail(Ashley, n=-1), NA)
                  )
      ) 
```

<center>
  ![](https://www.r-graph-gallery.com/connected_scatterplot_ggplot2_files/figure-html/thecode3-1.png){width=75%}
</center>

It makes sense to add arrows and labels to guide the reader in the chart:

```r
# data
data <- babynames %>% 
  filter(name %in% c("Ashley", "Amanda")) %>%
  filter(sex=="F") %>%
  filter(year>1970) %>%
  dplyr::select(year, name, n) %>%
  spread(key = name, value=n, -1)
# Select a few date to label the chart
tmp_date <- data %>% sample_frac(0.3)
# plot 
data %>% 
  ggplot(aes(x=Amanda, y=Ashley, label=year)) +
     geom_point(color="#69b3a2") +
     geom_text_repel(data=tmp_date) +
     geom_segment(color="#69b3a2", 
                  aes(
                    xend=c(tail(Amanda, n=-1), NA), 
                    yend=c(tail(Ashley, n=-1), NA)
                  ),
                  arrow=arrow(length=unit(0.3,"cm"))
      ) +
      theme_ipsum()
```


<center>
  ![](https://www.r-graph-gallery.com/connected_scatterplot_ggplot2_files/figure-html/thecode4-1.png){width=75%}
</center>

### Connected Scatterplot for `Time Series`

Connected scatterplots are often used for [time series](https://www.r-graph-gallery.com/time-series.html). Remember the R graph gallery offers a dedicated section, with heaps of examples. For instance, here is an interactive chart made with the [dygraphs](https://www.r-graph-gallery.com/time-series.html) library.

```r
# Library
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
 
# Read the data (hosted on the gallery website)
data <- read.table("https://python-graph-gallery.com/wp-content/uploads/bike.csv", header=T, sep=",") %>% head(300)
# Check type of variable
# str(data)
 
# Since my time is currently a factor, I have to convert it to a date-time format!
data$datetime <- ymd_hms(data$datetime)
 
# Then you can create the xts necessary to use dygraph
don <- xts(x = data$count, order.by = data$datetime)
# Finally the plot
p <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)
# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/dygraphs318.html"))
```


<center>
```{r correlation-dygraphs318, echo=FALSE}
htmltools::tags$iframe(title = "My embedded document", src = "evolutionHtml/dygraphs318.html", height="400px", width = "100%")
```
</center>

### Connected Scatterplot using Base R

Basic R also allows to build connected scatterplot thanks to the `line()` function. You just need to use the `b` option of the `type` argument. See examples below.

#### Add a Legend to a Base R Chart
 
This section explains how to add a legend to a chart made with base R, using the `legend()` function. It provides several reproducible examples with explanation and `R` code. It is done using the `legend()` function. The main arguments are:

This page aims to explain how to add a legend to a plot made in base R. It is done using the `legend()` function. The main arguments are:

* `legend`: names to display
* `bty`: type of box around the legend. See [graph #73](https://www.r-graph-gallery.com/73-box-style-with-the-bty-function.html)
* `horiz`: legend in column or in row
* `col`: symbol color
* `pch`: symbol type. See [graph #6](https://www.r-graph-gallery.com/6-graph-parameters-reminder.html)
* `pt.cex`: symbol size
* `cex`: text size
* `text.col`: text color
* `topright`: legend position: `bottomright`, `bottom`, `bottomleft`, `left`, `topleft`, `top`, `topright`, `right`, `center`
* `inset`: % (from 0 to 1) to draw the legend away from x and y axis

You can also give the `X` and `Y` coordinate of the legend: `legend(3, 5, ...)`

Note that an equivalent page exist concerning [legends with ggplot2](https://www.r-graph-gallery.com/239-custom-layout-legend-ggplot2.html).

```r
# Create data:
a=c(1:5)
b=c(5,3,4,5,5)
c=c(4,5,4,3,1)
 
# Make a basic graph
plot( b~a , type="b" , bty="l" , xlab="value of a" , ylab="value of b" , col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17 , ylim=c(1,5) )
lines(c ~a , col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )
 
# Add a legend
legend("bottomleft", 
  legend = c("Group 1", "Group 2"), 
  col = c(rgb(0.2,0.4,0.1,0.7), 
  rgb(0.8,0.4,0.1,0.7)), 
  pch = c(17,19), 
  bty = "n", 
  pt.cex = 2, 
  cex = 1.2, 
  text.col = "black", 
  horiz = F , 
  inset = c(0.1, 0.1))
```

![](https://www.r-graph-gallery.com/119-add-a-legend-to-a-plot_files/figure-html/thecode-1.png)

### Manage Dates Data with Base R

This section explains how to deal with date data in base R. It takes a [connected scatterplot](https://www.r-graph-gallery.com/connected-scatterplot.html) as an example and display several options to deal with dates. 

#### Important note about the `lubridate()` library.

I strongly advise to have a look to the `lubridate()` library. It allows to easily manipulate the date format, and is very powerful in conjunction with [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html). Have a look to the [time series section](https://www.r-graph-gallery.com/time-series.html) of the gallery.

##### Is your date recognized as a date?

R offers a special data type for dates. It is important to use it since it will make the creation of charts lot easier. The `str()` function allows to check the type of each column. In the example beside, the `date` column is recognized as a factor.

```{r manage-dates-data-base-r, echo=TRUE, message=FALSE, warning=FALSE}
# Create data
set.seed(124)
date <- paste(   "2015/03/" , sample(seq(1,31),6) , sep="")
value <- sample(seq(1,100) , 6)
data <- data.frame(date,value)
# Date and time are recognized as factor:
str(data)
```

#### Why it Matters

The issue is that your plot is gonna be very disappointing if the date is not recognized properly, as shown beside

```{r manage-dates-data-base-r-two, echo=TRUE, message=FALSE, warning=FALSE}
# Create data
set.seed(124)
date <- paste("2015/03/" , sample(seq(1,31),6) , sep="")
value <- sample(seq(1,100) , 6)
data <- data.frame(date,value)
# Date and time are recognized as factor:
str(data)
```


### Switch to Date Format

You can use the `as.Date()` function to specify that a column is at the date format. Now, with a bit of customization, we can get a nice [connected scatterplot](https://www.r-graph-gallery.com/connected-scatterplot.html) from our data:

```{r switch-dates-format-correlation, echo=TRUE, message=FALSE, warning=FALSE}
# Create data
set.seed(124)
date <- paste(   "2015/03/" , sample(seq(1,31),6) , sep="")
value <- sample(seq(1,100) , 6)
data <- data.frame(date,value)
# Let's change the date to the "date" format:
data$date <- as.Date(data$date)
 
# So we can sort the table:
data <- data[order(data$date) , ]
 
# Easy to make it better now:
plot(data$value~data$date , type="b" , lwd=3 , col=rgb(0.1,0.7,0.1,0.8) , ylab="value of ..." , xlab="date" , bty="l" , pch=20 , cex=4)
abline(h=seq(0,100,10) , col="grey", lwd=0.8)
```

### Base R Graph Parameters: Cheatsheet

This section aims to remind the options offered to customize a graph in base R. Understand in a sec how to use `lwd`, `pch`, `type`, `lty`, `cex`, and more. Base R offers many option to customize the chart appearance. 

Basically everthing is double with those few options:

* `cex`: shape size
* `lwd`: line width
* `col`: control colors
* `lty`: line type
* `pch`: marker shape
* `type`: link between dots

<u>Note</u>: visit the cheatsheet section for more.

```{r base-r-parameters, echo=TRUE, message=FALSE, warning=FALSE}
# initialization
par(mar=c(3,3,3,3))
num <- 0 ; 
num1 <- 0
plot(0,0 , xlim=c(0,21) , ylim=c(0.5,6.5), col="white" , yaxt="n" , ylab="" , xlab="")
 
#fill the graph
for (i in seq(1,20)){
  points(i,1 , pch=i , cex=3)
  points(i,2 , col=i , pch=16 , cex=3)
  points(i,3 , col="black" , pch=16 , cex=i*0.25)
  
  #lty
  if(i %in% c(seq(1,18,3))){
        num=num+1
    points(c(i,i+2), c(4,4) , col="black" , lty=num , type="l" , lwd=2)
        text(i+1.1 , 4.15 , num)
        }
  
  #type and lwd 
  if(i %in% c(seq(1,20,5))){
    num1=num1+1
    points(c(i,i+1,i+2,i+3), c(5,5,5,5) , col="black"  , type=c("p","l","b","o")[num1] , lwd=2)
    text(i+1.1 , 5.2 , c("p","l","b","o")[num1] )
    points(c(i,i+1,i+2,i+3), c(6,6,6,6) , col="black"  , type="l",  lwd=num1)
    text(i+1.1 , 6.2 , num1 )
 
    }
  }
 
#add axis
axis(2, at = c(1,2,3,4,5,6), labels = c("pch" , "col" , "cex" , "lty", "type" , "lwd" ), 
     tick = TRUE, col = "black", las = 1, cex.axis = 0.8)
```

## Density 2D

---

A [2D density chart](https://www.data-to-viz.com/graph/density2d.html) displays the relationship between 2 numeric variables. One is represented on the X axis, the other on the Y axis, like for a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html). Then, the number of observations within a particular area of the 2D space is counted and represented by a color gradient. Several types of 2d density chart exist:

#### 2d Histogram with `geom_bin2d()`

This is the two dimension version of the classic [histogram](https://www.r-graph-gallery.com/histogram.html). The plot area is split in a multitude of small squares, the number of points in each square is represented by its color.

### The Issue with `geom_point()`

A 2d density plot is useful to study the relationship between 2 numeric variables if you have a huge number of points. To avoid overlapping (as in the scatterplot beside), it divides the plot area in a multitude of small fragment and represents the number of points in this fragment. There are several types of 2d density plots. Each has its proper ggplot2 function. This section describes all of them.

```r
# Library
library(tidyverse)
 
# Data
a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
data <- rbind(a,b,c)
 
 
# Basic scatterplot
ggplot(data, aes(x=x, y=y) ) +
  geom_point()# 2d histogram with default option
ggplot(data, aes(x=x, y=y) ) +
  geom_bin2d() +
  theme_bw()
 
```

![](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2_files/figure-html/thecode-1.png)

### 2d Histogram with `geom_bin2d()`

This is the two dimension version of the classic [histogram](https://www.r-graph-gallery.com/histogram.html). The plot area is split in a multitude of small squares, the number of points in each square is represented by its color.

For 2d histogram, the plot area is divided in a multitude of squares. (It is a 2d version of the classic histogram). It is called using the `geom_bin_2d()` function. This function offers a bins argument that controls the number of `bins` you want to display.

<u>Note</u>: If you're not convinced about the importance of the `bins` option, read this.

```r
# 2d histogram with default option
ggplot(data, aes(x=x, y=y) ) +
  geom_bin2d() +
  theme_bw()
```

![](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2_files/figure-html/thecode2-1.png)

```r
# Bin size control + color palette
ggplot(data, aes(x=x, y=y) ) +
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()
```

![](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2_files/figure-html/thecode2-2.png)

### Hexbin Chart with `geom_hex()`

Another alternative is to divide the plot area in a multitude of hexagons: it is thus called a hexbin chart, and is made using the `geom_hex()` function.

This function provides the `bins` argument as well, to control the number of division per axis.

```r
# Hexbin chart with default option
ggplot(data, aes(x=x, y=y) ) +
  geom_hex() +
  theme_bw()
```

![](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2_files/figure-html/thecode3-1.png)

```r
# Bin size control + color palette
ggplot(data, aes(x=x, y=y) ) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()
```

![](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2_files/figure-html/thecode3-2.png)

### 2d Distribution with `geom_density_2d` or `stat_density_2d`

As you can plot a [density chart](https://www.r-graph-gallery.com/density-plot.html) instead of a [histogram](https://www.r-graph-gallery.com/histogram.html), it is possible to compute a 2d density and represent it. Several possibilities are offered by `ggplot2`: you can show the contour of the distribution, or the area, or use the `raster` function:

```r
# Show the contour only
ggplot(data, aes(x=x, y=y) ) +
  geom_density_2d()
```

![](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2_files/figure-html/thecode4-1.png)

```r
# Show the area only
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")
```

![](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2_files/figure-html/thecode4-2.png)

```r
# Area + contour
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")
```

![](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2_files/figure-html/thecode4-3.png)

```r
# Using raster
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )
```

![](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2_files/figure-html/thecode4-4.png)

### Customize the Color Palette

Whatever you use a 2d histogram, a hexbin chart or a 2d distribution, you can and should custom the colour of your chart. Here is a suggestion using the `scale_fill_distiller()` function. You can see other methods in the [ggplot2 section](https://www.r-graph-gallery.com/ggplot2-package.html) of the gallery.

```r 
# Call the palette with a number
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=-1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )
```

![](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2_files/figure-html/thecode5-1.png)

```r
# The direction argument allows to reverse the palette
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )
```

![](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2_files/figure-html/thecode5-2.png)

```r
# You can also call the palette using a name.
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )
```

![](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2_files/figure-html/thecode5-3.png)

### Hexbin Chart with the Hexbin Package
 
This section explains how to build a hexbin chart with R using the `hexbin` package. Hexbin chart is a [2d density chart](https://www.r-graph-gallery.com/2d-density-chart.html), allowing to visualize the relationship between 2 numeric variables.

[Scatterplots](https://www.r-graph-gallery.com/scatterplot.html) can get very hard to interpret when displaying large datasets, as points inevitably overplot and can't be individually discerned.

Binning can be though of as a two-dimensional [histogram](https://www.r-graph-gallery.com/histogram.html), where shades of the bins take the place of the heights of the bars. This technique is computed in the `hexbin` package.

This example has been published by [Myles Harrison](http://www.everydayanalytics.ca/2014/09/5-ways-to-do-2d-histograms-in-r.html) on R-bloggers.

```r
# Packages
library(hexbin)
library(RColorBrewer)
 
# Create data
x <- rnorm(mean=1.5, 5000)
y <- rnorm(mean=1.6, 5000)
 
# Make the plot
bin<-hexbin(x, y, xbins=40)
my_colors=colorRampPalette(rev(brewer.pal(11,'Spectral')))
plot(bin, main="" , colramp=my_colors , legend=F ) 
```
<center>
  ![](https://www.r-graph-gallery.com/100-high-density-scatterplot-with-binning_files/figure-html/thecode-1.png){width=75%}
</center>
### Hexbin Chart and Scatterplot with Ggplot2

This section explains how to build a hexbin chart with a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) on top using R and [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html). It is an addition to the page about [2d density plot with ggplot2](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2.html). 

This plot extends the concepts described in the [2d density chart with ggplot2](https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2.html) document. It simply illustrates that a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) can be added on top of the 2d density chart.

Thanks [Christian Jacob](http://chrisk91.me/) for this submission.

```{r hexbin-correlation-scatterplot-ggplot, echo=TRUE, message=FALSE, warning=FALSE}
# library
library(ggplot2) 
# data
sample_data <- data.frame(x_values = 1:100 + rnorm(100,sd=20), y_values = 1:100 + rnorm(100,sd=27)) 
#plot
ggplot(sample_data, aes(x_values, y_values)) +
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
  geom_point(colour = "white")
```


## Scatterplot

---

A [Scatterplot](https://www.r-graph-gallery.com/scatterplot.html) displays the relationship between 2 numeric variables. Each dot represents an observation. Their position on the X (horizontal) and Y (vertical) axis represents the values of the 2 variables.

#### Using the `ggplot2` Package

Scatterplots are built with [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html) thanks to the `geom_point()` function. Discover a basic use case in [graph #272](https://www.r-graph-gallery.com/272-basic-scatterplot-with-ggplot2.html), and learn how to custom it with next examples below. Basic scatterplot with R and ggplot2. A scatterplot displays the values of two variables along two axes. It shows the relationship between them, eventually revealing a correlation.

A [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) displays the values of two variables along two axes. It shows the relationship between them, eventually revealing a correlation.

Here the relationship between Sepal width and Sepal length of several plants is shown.

It illustrates the basic utilization of `ggplot2` for scatterplots:

1. Provide a dataframe.
2. Tell which variable to show on x and y axis.
3. Add a `geom_point()` to show points.

```{r scatterplot-correlation-ggplot, echo=TRUE, message=FALSE, warning=FALSE}
# library
library(ggplot2)
 
# The iris dataset is provided natively by R
#head(iris)
 
# basic scatterplot
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
    geom_point()
```

### Custom `ggplot2` Scatterplot

This post is dedicated to customization you can apply to a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) built with [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html).


This post follows the previous [basic scatterplot](https://www.r-graph-gallery.com/272-basic-scatterplot-with-ggplot2.html) with `ggplot2`. It shows the kind of customization you can apply to circles thanks to the `geom_point()` options:

* `color`: the stroke color, the circle outline
* `stroke`: the stroke width
fill: color of the circle inner part
* `shape`: shape of the marker. See list in the ggplot2 section
* `alpha`: circle transparency, [0->1], 0 is fully transparent
color: the stroke color, the circle outline
* `size`: circle size

<u>Note</u>: These options will be uniform among markers if you put it in the `geom_point()` call. You can also map them to a variable if put inside the `aes()` part of the code.


```{r scatterplot-correlation-ggplot-custom, echo=TRUE, message=FALSE, warning=FALSE}
# library
library(ggplot2)
 
# Iris dataset is natively provided by R
#head(iris)
 
# use options!
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
    geom_point(
        color="orange",
        fill="#69b3a2",
        shape=21,
        alpha=0.5,
        size=6,
        stroke = 2
        )
```


### Using `theme_ipsum`

Note that applying the `theme_ipsum` of the `hrbrthemes` package is always a good option.


```{r scatterplot-correlation-ggplot-ipsum, echo=TRUE, message=FALSE, warning=FALSE}
# library
library(ggplot2)
library(hrbrthemes)
# Iris dataset is natively provided by R
#head(iris)
 
# use options!
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
    geom_point(
        color="black",
        fill="#69b3a2",
        shape=22,
        alpha=0.5,
        size=6,
        stroke = 1
        ) +
    theme_ipsum()
```


### Map a Variable to Marker Feature in `ggplot2` Scatterplot

`ggplot2` allows to easily map a variable to marker features of a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html). This section explaines how it works through several examples, with explanation and code.

#### Basic Example

Here is the magic of [ggplot2](https://www.r-graph-gallery.com/ggplot2): the ability to map a variable to marker features. Here, the marker `color` depends on its value in the field called `Species` in the input data frame.

Note that the legend is built automatically.

```{r scatterplot-correlation-map-variable, echo=TRUE, message=FALSE, warning=FALSE}
# load ggplot2
library(ggplot2)
library(hrbrthemes)
# mtcars dataset is natively available in R
# head(mtcars)
 
# A basic scatterplot with color depending on Species
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + 
    geom_point(size=6) +
    theme_ipsum()
```

### Works with any Aesthetics

Map variables to any marker features. For instance, specie is represente below using transparency (left), shape (middle) and size (right).

```{r scatterplot-correlation-aesthetics, echo=TRUE, message=FALSE, warning=FALSE}
# load ggplot2
library(ggplot2)
library(hrbrthemes)
# Transparency
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, alpha=Species)) + 
    geom_point(size=6, color="#69b3a2") +
    theme_ipsum()
# Shape
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, shape=Species)) + 
    geom_point(size=6) +
    theme_ipsum()
# Size
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, shape=Species)) + 
    geom_point(size=6) +
    theme_ipsum()
```

### Mapping to Several Features

Last but not least, note that you can map one or several variables to one or several features. Here, shape, transparency, size and color all depends on the marker `Species` value.

```{r scatterplot-correlation-mapping-several-features, echo=TRUE, message=FALSE, warning=FALSE}
# load ggplot2
library(ggplot2)
library(hrbrthemes)
# A basic scatterplot with color depending on Species
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, shape=Species, alpha=Species, size=Species, color=Species)) + 
    geom_point() +
    theme_ipsum()
```

### Add Text Labels with `ggplot2`

This document is dedicated to text annotation with [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html). It provides several examples with reproducible code showing how to use function like `geom_label` and `geom_text`.

#### Adding Text with `geom_text()`

This example demonstrates how to use `geom_text()` to add text as markers. It works pretty much the same as `geom_point()`, but add text instead of circles. A few arguments must be provided:

* `label`: What text you want to display.
* `nudge_x` and `nudge_y`: Shifts the text along X and Y axis.
* `check_overlap`: Tries to avoid text overlap. Note that a package called `ggrepel` extends this concept further.

```{r scatterplot-correlation-add-text-labels, echo=TRUE, message=FALSE, warning=FALSE}
# library
library(ggplot2)
 
# Keep 30 first rows in the mtcars natively available dataset
data=head(mtcars, 30)
 
# 1/ add text with geom_text, use nudge to nudge the text
ggplot(data, aes(x=wt, y=mpg)) +
  geom_point() + # Show dots
  geom_text(
    label=rownames(data), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  )
```

### Add Labels with `geom_label()`

`geom_label()` works pretty much the same way as `geom_text()`. However, text is wrapped in a rectangle that you can customize (see next example).

```{r scatterplot-correlation-add-labels-geom, echo=TRUE, message=FALSE, warning=FALSE}
# library
library(ggplot2)
 
# Keep 30 first rows in the mtcars natively available dataset
data=head(mtcars, 30)
 
# 1/ add text with geom_text, use nudge to nudge the text
ggplot(data, aes(x=wt, y=mpg)) +
  geom_point() + # Show dots
  geom_label(
    label=rownames(data), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  )
```

### Add One Text Label Only

Of course, you don't have to label all dots on the chart. You can also add a piece of text on a specific position. Since we're here, note that you can custom the annotation of `geom_label` with `label.padding`, `label.size`, `color` and `fill` as described below:

```{r scatterplot-correlation-add-one-text-only, echo=TRUE, message=FALSE, warning=FALSE}
# library
library(ggplot2)
 
# Keep 30 first rows in the mtcars natively available dataset
data=head(mtcars, 30)
 
# Add one annotation
ggplot(data, aes(x=wt, y=mpg)) +
  geom_point() + # Show dots
  geom_label(
    label="Look at this!", 
    x=4.1,
    y=20,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="#69b3a2"
  )
```

### Add Labels for a Selection of Marker

Last but not least, you can also select a group of marker and annotate them only. Here, only car with `mpg` > 20 and `wt` > 3 are annotated thanks to a data filtering in the `geom_label()` call.

```{r scatterplot-correlation-add-labels-selection-marker, echo=TRUE, message=FALSE, warning=FALSE}
# library
library(ggplot2)
library(dplyr)
library(tibble)
# Keep 30 first rows in the mtcars natively available dataset
data=head(mtcars, 30)
# Change data rownames as a real column called 'carName'
data <- data %>%
  rownames_to_column(var="carName")
  
# Plot
ggplot(data, aes(x=wt, y=mpg)) +
  geom_point() + 
  geom_label( 
    data=data %>% filter(mpg>20 & wt>3), # Filter data first
    aes(label=carName)
  )
```

### Ggplot2 Scatterplot with Rug

This section demonstrates how to build a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) with `rug` with `R` and `ggplot2`. Adding rug gives insight about variable distribution and is especially helpful when markers overlap.

#### Adding Rug with `geom_rug()`

A [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) displays the relationship between 2 numeric variables. You can easily add rug on X and Y axis thanks to the `geom_rug()` function to illustrate the distribution of dots.

Note you can as well add [marginal plots](https://www.r-graph-gallery.com/277-marginal-histogram-for-ggplot2.html) to show these distributions.

```{r scatterplot-correlation-ggplot-scatter-rug, echo=TRUE, message=FALSE, warning=FALSE}
# library
library(ggplot2)
# Iris dataset
head(iris)
# plot
ggplot(data=iris, aes(x=Sepal.Length, Petal.Length)) +
  geom_point() +
  geom_rug(col="steelblue",alpha=0.1, size=1.5)
```

### Marginal Distribution with `ggplot2` and `ggExtra`

This section explains how to add marginal distributions to the X and Y axis of a `ggplot2` [scatterplot](https://www.r-graph-gallery.com/scatterplot.html). It can be done using [histogram](https://www.r-graph-gallery.com/histogram.html), [boxplot](https://www.r-graph-gallery.com/boxplot.html) or [density plot](https://www.r-graph-gallery.com/density-plot.html) using the `ggExtra` library.

#### Basic use of `ggMarginal()`

Here are 3 examples of marginal distribution added on X and Y axis of a scatterplot. The ggExtra library makes it a breeze thanks to the `ggMarginal()` function. Three main types of distribution are available: [histogram](https://www.r-graph-gallery.com/histogram.html), [density](https://www.r-graph-gallery.com/density-plot.html) and [boxplot](https://www.r-graph-gallery.com/boxplot.html).

Three additional examples to show possible customization:

* Change marginal plot size with `size`.
* Custom marginal plot appearance with all usual parameters.
show only one marginal plot with `margins = 'x'` or `margins = 'y'`.

```{r scatterplot-correlation-ggplot-marginal-distribution, echo=TRUE, message=FALSE, warning=FALSE}
# library
library(ggplot2)
library(ggExtra)
 
# The mtcars dataset is proposed in R
head(mtcars)
 
# classic plot :
p <- ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, size=cyl)) +
      geom_point() +
      theme(legend.position="none")
 
# with marginal histogram
p1 <- ggMarginal(p, type="histogram")
 
# marginal density
p2 <- ggMarginal(p, type="density")
 
# marginal boxplot
p3 <- ggMarginal(p, type="boxplot")
```

### More Customization

Three additional examples to show possible customization:

* Change marginal plot size with size
* Custom marginal plot appearance with all usual parameters
* Show only one marginal plot with margins = 'x' or margins = 'y'

```{r scatterplot-correlation-ggplot-more-customizations, echo=TRUE, message=FALSE, warning=FALSE}
# library
library(ggplot2)
library(ggExtra)
 
# The mtcars dataset is proposed in R
head(mtcars)
 
# classic plot :
p <- ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, size=cyl)) +
      geom_point() +
      theme(legend.position="none")
 
# Set relative size of marginal plots (main plot 10x bigger than marginals)
p1 <- ggMarginal(p, type="histogram", size=10)
 
# Custom marginal plots:
p2 <- ggMarginal(p, type="histogram", fill = "slateblue", xparams = list(  bins=10))
 
# Show only marginal plot for x axis
p3 <- ggMarginal(p, margins = 'x', color="purple", size=4)
```

```{r scatterplot-correlation-ggplot-more-customizations-one-two-three, echo=TRUE, message=FALSE, warning=FALSE}
p1
p2
p3
```

### Linear Model and Confidence Interval in `ggplot2`

Display the result of a linear model and its confidence interval on top of a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html). A [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html) implementation with reproducible code.

#### Linear Trend

Adding a linear trend to a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) helps the reader in seeing patterns. `ggplot2` provides the `geom_smooth()` function that allows to add the linear trend and the confidence interval around it if needed (option `se=TRUE`).

<u>Note</u>: The `method` argument allows to apply different smoothing method like `glm`, `loess` and more. See the [doc](https://ggplot2.tidyverse.org/reference/geom_smooth.html) for more.

```{r scatterplot-correlation-ggplot-linear-model-confidence-interval, echo=TRUE, message=FALSE, warning=FALSE}
# Library
library(ggplot2)
library(hrbrthemes)
# Create dummy data
data <- data.frame(
  cond = rep(c("condition_1", "condition_2"), each=10), 
  my_x = 1:100 + rnorm(100,sd=9), 
  my_y = 1:100 + rnorm(100,sd=16) 
)
# Basic scatter plot.
p1 <- ggplot(data, aes(x=my_x, y=my_y)) + 
  geom_point( color="#69b3a2") +
  theme_ipsum()
 
# with linear trend
p2 <- ggplot(data, aes(x=my_x, y=my_y)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()
# linear trend + confidence interval
p3 <- ggplot(data, aes(x=my_x, y=my_y)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()
```

```{r scatterplot-correlation-basic-scatterplot, message=FALSE, warning=FALSE}
p1
```

```{r scatterplot-correlation-linear-trend, message=FALSE, warning=FALSE}
p2
```

```{r scatterplot-correlation-linear-trend-confidence-interval, message=FALSE, warning=FALSE}
p3
```


### Using Base R

Base R is also a good option to build a scatterplot, using the `plot()` function. The [chart #13](https://www.r-graph-gallery.com/13-scatter-plot.html) below will guide you through its basic usage. Following examples allow a greater level of customization.

#### Basic Scatterplot in Base R

A very basic [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) built with base R and the `plot()` function. Explanation and code provided. 

#### Most Basic Scatterplot

The `plot()` function of R allows to build a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html). Both numeric variables of the input dataframe must be specified in the `x` and `y` argument.


```{r scatterplot-correlation-base-r-basic, echo=TRUE, message=FALSE, warning=FALSE}
# Create data
data = data.frame(
  x=seq(1:100) + 0.1*seq(1:100)*sample(c(1:10) , 100 , replace=T),
  y=seq(1:100) + 0.2*seq(1:100)*sample(c(1:10) , 100 , replace=T)
)
# Basic scatterplot
plot(x=data$x, y=data$y)
```

#### Customizations

Here is a description of the most common customization:

* `cex`: circle size
* `xlim` and `ylim`: limits of the X and Y axis
* `pch`: shape of markers. See all here.
* `xlab` and `ylab`: X and Y axis labels
* `col`: marker color
* `main`: chart title

```{r scatterplot-correlation-base-r-customizations-two, echo=TRUE, message=FALSE, warning=FALSE}
# Create data
data = data.frame(
  x=seq(1:100) + 0.1*seq(1:100)*sample(c(1:10) , 100 , replace=T),
  y=seq(1:100) + 0.2*seq(1:100)*sample(c(1:10) , 100 , replace=T)
)
# Basic scatterplot
plot(data$x, data$y,
     xlim=c(0,250) , ylim=c(0,250), 
     pch=18, 
     cex=2, 
     col="#69b3a2",
     xlab="value of X", ylab="value of Y",
     main="A simple scatterplot"
     )
```

### Map the Marker Color to a Categorical Variable

```r 
# the iris dataset is provided by R natively
# Create a color palette
library(paletteer)
colors <- paletteer_c(package = "ggthemes", palette = "Green-Blue-White", n = 3)
# Scatterplot with categoric color scale
plot(
  x = iris$Petal.Length, 
  y = iris$Petal.Width,
  bg = colors[ unclass(iris$Species) ],
  cex = 3,
  pch=21
)
```

<center>
  ![](https://www.r-graph-gallery.com/13-scatter-plot_files/figure-html/thecode3-1.png){width=75%}
</center>

### Map the Marker Color to a Numeric Variable

```r
# the iris dataset is provided by R natively
# Create a color palette
library(paletteer)
nColor <- 20
colors <- paletteer_c(package = "viridis", palette = "inferno", n = nColor)
# Transform the numeric variable in bins
rank <- as.factor( as.numeric( cut(iris$Petal.Width, nColor)))
# Scatterplot with color gradient
plot(
  x = iris$Petal.Length, 
  y = iris$Petal.Width,
  bg = colors[ rank ],
  cex = 3,
  pch=21
)
```
<center>
  ![](https://www.r-graph-gallery.com/13-scatter-plot_files/figure-html/thecode4-1.png){width=75%}
</center>



### Scatterplot with Polynomial Curve Fitting

This example describes how to build a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) with a polynomial curve drawn on top of it. First of all, a scatterplot is built using the native R `plot()` function. Then, a polynomial model is fit thanks to the `lm()` function. 

First of all, a scatterplot is built using the native R `plot()` function. Then, a polynomial model is fit thanks to the `lm()` function. It is possible to have the estimated Y value for each step of the X axis using the `predict()` function, and plot it with `line()`.

It is a good practice to add the equation of the model with `text()`.

<u>Note</u>: You can also add a confidence interval around the model as described in [chart #45](https://www.r-graph-gallery.com/45-confidence-interval-around-polynomial-curve-fitting).
 
```{r scatterplot-correlation-base-r-polynomial-curve, echo=TRUE, message=FALSE, warning=FALSE}
x <- runif(300,  min=-10, max=10) 
y <- 0.1*x^3 - 0.5 * x^2 - x + 10 + rnorm(length(x),0,8) 
 
# plot of x and y :
plot(x,y,col=rgb(0.4,0.4,0.8,0.6),pch=16 , cex=1.3) 
 
# Can we find a polynome that fit this function ?
model <- lm(y ~ x + I(x^2) + I(x^3))
 
#Features of this model :
#summary(model)
#model$coefficients
#summary(model)$adj.r.squared
 
# For each value of x, I can get the value of y estimated by the model, and add it to the current plot !
myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col=2, lwd=2 )  
# Add the features of the model to the plot
coeff <- round(model$coefficients , 2)
text(3, -70 , paste("Model : ",coeff[1] , " + " , coeff[2] , "*x"  , "+" , coeff[3] , "*x^2" , "+" , coeff[4] , "*x^3" , "\n\n" , "P-value adjusted = ",round(summary(model)$adj.r.squared,2)))
```

### Polynomial Curve Fitting and Confidence Interval

This example follows the previous [scatterplot with polynomial curve](https://www.r-graph-gallery.com/44-polynomial-curve-fitting.html). It extends this example, adding a confidence interval.This example follows the previous [chart #44](https://www.r-graph-gallery.com/44-polynomial-curve-fitting.html) that explained how to add polynomial curve on top of a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) in base R.

Here, a confidence interval is added using the `polygon()` function.

```{r scatterplot-correlation-base-r-polynomial-curve-fitting-ci, echo=TRUE, message=FALSE, warning=FALSE}
# We create 2 vectors x and y. It is a polynomial function.
x <- runif(300, min=-30, max=30) 
y <- -1.2*x^3 + 1.1 * x^2 - x + 10 + rnorm(length(x),0,100*abs(x)) 
# Basic plot of x and y :
plot(x,y,col=rgb(0.4,0.4,0.8,0.6), pch=16 , cex=1.3 , xlab="" , ylab="") 
# Can we find a polynome that fit this function ?
model <- lm(y ~ x + I(x^2) + I(x^3))
# I can get the features of this model :
#summary(model)
#model$coefficients
#summary(model)$adj.r.squared
#For each value of x, I can get the value of y estimated by the model, and the confidence interval around this value.
myPredict <- predict( model , interval="predict" )
#Finally, I can add it to the plot using the line and the polygon function with transparency.
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix , 1], col=2, lwd=2 )
polygon(c(rev(x[ix]), x[ix]), c(rev(myPredict[ ix,3]), myPredict[ ix,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
```

### Lattice XY Plot Function

The `xyplot()` function of the lattice package allows to build a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) for several categories automatically. The lattice library offers the `xyplot()` function. It builds a scatterplot for each levels of a factor automatically.

The lattice library offers the `xyplot()` function. It builds a [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) for each levels of a factor automatically.

It is actually the ancestor of the `geom_wrap()` function of [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html) than you can see in action [here](https://www.r-graph-gallery.com/223-faceting-with-ggplot2.html).

```{r scatterplot-correlation-lattice-package, echo=TRUE, message=FALSE, warning=FALSE}
# Library
library(lattice)
# create data :
sample <- paste(rep("sample_",40) , seq(1,40) , sep="")
specie <- c(rep("carot" , 10) , rep("cumcumber" , 10) , rep("wheat" , 10) , rep("Potatoe" , 10) )
gene1 <- c( seq(5,14)+rnorm(10 , 4 , 1) , seq(5,14)+rnorm(10 , 4 , 1) , seq(5,14)+rnorm(10 , 4 , 1) , seq(5,14)+rnorm(10 , 4 , 1) )
gene2 <- c( seq(5,14)+rnorm(10 , 4 , 1) , seq(5,14)+rnorm(10 , 2 , 0.2) , seq(5,14)+rnorm(10 , 4 , 4) , seq(5,14)+rnorm(10 , 4 , 3) )
data <- data.frame(sample,specie,gene1,gene2)
 
# Make the graph
xyplot(gene1 ~ gene2 | specie , data=data , pch=20 , cex=3 , col=rgb(0.2,0.4,0.8,0.5) )
```

### Correlation between Discrete Variable

Studying the relationship between 2 discrete variables is complicated since an usual [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) suffers overplotting. Here is a workaround using base R.

#### Scatterplot with Variable Size

An usual [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) would suffer over plotting when used for discrete variables: dots would be drawn on top of each other, making the chart unreadable. The workaround suggested here makes dot size proportional to the number of data points behind it. On top of that, the exact number can be represented in the bubble thanks to the `text()` function.

```{r scatterplot-correlation-scatterplot-variable-size, echo=TRUE, message=FALSE, warning=FALSE}
#Let's create 2 discrete variables 
a <- c(1,1,3,4,5,5,1,1,2,3,4,1,3,2,1,1,5,1,4,3,2,3,1,0,2)
b <- c(1,2,3,5,5,5,2,1,1,3,4,3,3,4,1,1,4,1,4,2,2,3,0,0,1)
 
#I count the occurence of each couple of values. Eg : number of time a=1 and b=1, number of time a=1 and b=2 etc...
AA <- xyTable(a,b)
 
#Now I can plot this ! I represent the dots as big as the couple occurs often
coeff_bigger <- 2
plot(AA$x , AA$y , cex=AA$number*coeff_bigger  , pch=16 , col=rgb(0,0,1,0.5) , xlab= "value of a" , ylab="value of b" , xlim=c(0,6) , ylim=c(0,6) )
text(AA$x , AA$y , AA$number )
 
#Note : It's easy to make a function that will compute this kind of plot automaticaly :
represent_discrete_variable <- function(var1, var2 , coeff_bigger){
  AA=xyTable(var1,var2)
  plot(AA$x , AA$y , cex=AA$number*coeff_bigger  , pch=16 , col="chocolate1" , xlab= "value of a" , ylab="value of b" )
  text (AA$x , AA$y , AA$number )
}
```

#### Other Workarounds

Other workarounds could be considered in this situation:

* [Heatmap](https://www.r-graph-gallery.com/heatmap.html)
* [2d density chart](https://www.r-graph-gallery.com/2d-density-chart.html)
* Jittering
* [Boxplot](https://www.r-graph-gallery.com/boxplot.html)

### Use `mtext()` to Write Text in Margin

This document describes how to use the `mtext()` function to add text in the plot margin. Usefull to add title on a multi chart. 

The `mtext()` function allows to write text in one of the four margins of the current figure region or one of the outer margins of the device region.

Here, the figure is first split thanks to `par(mfrow())`. Then, only one title is added and centered using `mtext()`.

```{r scatterplot-correlation-write-text-margin, echo=TRUE, message=FALSE, warning=FALSE}
#Dummy data 
Ixos <- rnorm(4000,100,30)
Primadur <- Ixos+rnorm(4000 , 0 , 30)
 
#Divide the screen in 1 line and 2 columns
par(
  mfrow=c(1,2), 
  oma = c(0, 0, 2, 0)
) 
 
#Make the margin around each graph a bit smaller
par(mar=c(4,2,2,2))
 
# Histogram and Scatterplot
hist(Ixos,  main="" , breaks=30 , col=rgb(0.3,0.5,1,0.4) , xlab="height" , ylab="nbr of plants")
plot(Ixos , Primadur,  main="" , pch=20 , cex=0.4 , col=rgb(0.3,0.5,1,0.4)  , xlab="primadur" , ylab="Ixos" )
 
#And I add only ONE title :
mtext("Primadur : Distribution and correlation with Ixos", outer = TRUE, cex = 1.5, font=4, col=rgb(0.1,0.3,0.5,0.5) )
```

### Customizations

Here is a description of the most common customization:

* `cex`: circle size
* `xlim` and `ylim`: limits of the X and Y axis
* `pch`: shape of markers. See all here.
* `xlab` and `ylab`: X and Y axis labels
* `col`: marker color
* `main`: chart title

```{r scatterplot-correlation-base-r-common-customizations, echo=TRUE, message=FALSE, warning=FALSE}
# Create data
data = data.frame(
  x=seq(1:100) + 0.1*seq(1:100)*sample(c(1:10) , 100 , replace=T),
  y=seq(1:100) + 0.2*seq(1:100)*sample(c(1:10) , 100 , replace=T)
)
# Basic scatterplot
plot(data$x, data$y,
     xlim=c(0,250) , ylim=c(0,250), 
     pch=18, 
     cex=2, 
     col="#69b3a2",
     xlab="value of X", ylab="value of Y",
     main="A simple scatterplot"
     )
```

### The `split_screen()` Function of R

This document explains how to use the `split_screen()` function of R to divide your device in several parts, one for each chart. 

The `split_screen()` function allows to divide the window in several chart sections.

However, 

* The [mfrow method](https://www.r-graph-gallery.com/71-split-screen-with-par-mfrow.html) is more adapted for simple layouts.
* The [layout function](https://www.r-graph-gallery.com/75-split-screen-with-layout.html) is better for complex layouts.

```{r scatterplot-correlation-split-screen, echo=TRUE, message=FALSE, warning=FALSE}
#Create data
a <- seq(1,29)+4*runif(29,0.4)
b <- seq(1,29)^2+runif(29,0.98)
 
# I divide the screen in 2 line and 1 column only
my_screen_step1 <- split.screen(c(2, 1))
 
# I add one graph on the screen number 1 which is on top :
screen(my_screen_step1[1])
plot( a,b , pch=20 , xlab="value of a" , cex=3 , col=rgb(0.4,0.9,0.8,0.5) )
 
 
# I divide the second screen in 2 columns :
my_screen_step2 <- split.screen(c(1, 2), screen = my_screen_step1[2])
screen(my_screen_step2[1])
hist(a, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="" , xlab="distribution of a")
screen(my_screen_step2[2])
hist(b, border=F , col=rgb(0.8,0.2,0.8,0.7) , main="" ,  xlab="distribution of b")
```

### Use par mfrow to Split Screen

The `par()` function allows to set the `mfrow()` parameters to cut the charting window in several section. 

#### Most Basic Scatterplot

The `par()` function allows to set parameters to the plot. The `mfrow()` parameter allows to split the screen in several panels. Subsequent charts will be drawn in panels. 

You have to provide a vector of length 2 to `mfrow()`: number of rows and number of columns.
 
<u>Note</u>: `mfcol()` does the same job but draws figure by columns instead of by row.

Alternative: See the [layout()](https://www.r-graph-gallery.com/75-split-screen-with-layout.html) function for more complex layout creation.

```{r scatterplot-correlation-mfrow-split-screen, echo=TRUE, message=FALSE, warning=FALSE}
#Create data
a <- seq(1,29)+4*runif(29,0.4)
b <- seq(1,29)^2+runif(29,0.98)
 
#Divide the screen in 2 columns and 2 lines
par(mfrow=c(2,2))
 
#Add a plot in each sub-screen !
plot( a,b , pch=20)
plot(a-b , pch=18)
hist(a, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="")
boxplot(a , col="grey" , xlab="a")
```

### Base R Graph Parameters: A Cheatsheet.

This section aims to remind the options offered to customize a graph in base R. Understand in a sec how to use `lwd`, `pch`, `type`, `lty`, `cex`, and more. Base R offers many option to customize the chart appearance. Basically everthing is doable with those few options:

Base R offers many option to customize the chart appearance. Basically everthing is doable with those few options:

* `cex`: shape size
* `lwd`: line width
* `col`: control colors
* `lty`: line type
* `pch`: marker shape
* `type`: link between dots

<u>Note</u>: visit the [cheatsheet section](https://www.r-graph-gallery.com/cheatsheet.html) for more.

```{r scatterplot-correlation-base-r-parameters, echo=TRUE, message=FALSE, warning=FALSE}
par(mar=c(3,3,3,3))
num <- 0 ; 
num1 <- 0
plot(0,0 , xlim=c(0,21) , ylim=c(0.5,6.5), col="white" , yaxt="n" , ylab="" , xlab="")
 
#fill the graph
for (i in seq(1,20)){
  points(i,1 , pch=i , cex=3)
  points(i,2 , col=i , pch=16 , cex=3)
  points(i,3 , col="black" , pch=16 , cex=i*0.25)
  
  #lty
  if(i %in% c(seq(1,18,3))){
        num=num+1
    points(c(i,i+2), c(4,4) , col="black" , lty=num , type="l" , lwd=2)
        text(i+1.1 , 4.15 , num)
        }
  
  #type and lwd 
  if(i %in% c(seq(1,20,5))){
    num1=num1+1
    points(c(i,i+1,i+2,i+3), c(5,5,5,5) , col="black"  , type=c("p","l","b","o")[num1] , lwd=2)
    text(i+1.1 , 5.2 , c("p","l","b","o")[num1] )
    points(c(i,i+1,i+2,i+3), c(6,6,6,6) , col="black"  , type="l",  lwd=num1)
    text(i+1.1 , 6.2 , num1 )
 
    }
  }
 
#add axis
axis(2, at = c(1,2,3,4,5,6), labels = c("pch" , "col" , "cex" , "lty", "type" , "lwd" ), 
     tick = TRUE, col = "black", las = 1, cex.axis = 0.8)
```

### Special Use Case: Manhattan Plots

A [Manhattan plot](https://www.r-graph-gallery.com/101_Manhattan_plot.html) is a particular type of scatterplot used in genomics. The X axis displays the position of a genetic variant on the genome. Each chromosome is usually represented using a different color. The Y axis shows p-value of the association test with a phenotypic trait.

> A [Manhattan plot](https://en.wikipedia.org/wiki/Manhattan_plot) is a specific type of [scatter plot](https://www.r-graph-gallery.com/scatterplot/) widely used in genomics to study [GWAS](https://en.wikipedia.org/wiki/Genome-wide_association_study) results (Genome Wide Association Study). Each point represents a genetic variant. The X axis shows its position on a chromosome, the Y axis tells how much it is associated with a trait. This page reviews how to make a Manhattan plot with R, and displays a couple of variations.
Basic The [manhattan function](https://www.rdocumentation.org/packages/qqman/versions/0.1.2/topics/manhattan) is straightforward: it just needs to have 4 columns identified properly, and does a proper job.

```{r manhattan-plots-correlation, echo=TRUE, message=FALSE, warning=FALSE}
# Load the library
library(qqman)
# Make the Manhattan plot on the gwasResults dataset
manhattan(gwasResults, chr="CHR", bp="BP", snp="SNP", p="P" )
```

#### SNP of Interest

A common task is to highlight a group of SNP on the Manhattan plot. For example it is handy to show which SNP are part of the clumping result. This is an easy task with qqman once you have identified the SNPs of interest.

```{r manhattan-plots-correlation-snp-interest, echo=TRUE, message=FALSE, warning=FALSE}
# A list of SNP of interest is provided with the library:
snpsOfInterest
# Let's highlight them, with a bit of customization on the plot
manhattan(gwasResults, highlight = snpsOfInterest)
```

#### Annotate 

You probably want to know the name of the SNP of interest: the ones with a high pvalue. You can automatically annotate them using the `annotatePval` argument:

```{r manhattan-plots-correlation-annotate, echo=TRUE, message=FALSE, warning=FALSE}
manhattan(gwasResults, annotatePval = 0.01)
```

#### Qqplot 

It is a good practice to draw a qqplot from the output of a GWAS. It allows to compare the distribution of the pvalue with an expected distribution by chance. Its realisation is straightforward thanks to the qq function:

```{r qqplot-correlation, echo=TRUE, message=FALSE, warning=FALSE}
qq(gwasResults$P)
```

### Highly Customizable with `ggplot2`

If you want to access a maximum level of customization it is sometimes good to build your plot from scratch. Here is an example using `dplyr` and [ggplot2](https://www.r-graph-gallery.com/portfolio/ggplot2-package/).

#### Basic

First of all, we need to compute the cumulative position of SNP.

```{r qqplot-correlation-highly-custom, echo=TRUE, message=FALSE, warning=FALSE}
don <- gwasResults %>% 
  
  # Compute chromosome size
  group_by(CHR) %>% 
  summarise(chr_len=max(BP)) %>% 
  
  # Calculate cumulative position of each chromosome
  mutate(tot=cumsum(chr_len)-chr_len) %>%
  dplyr::select(-chr_len) %>%
  
  # Add this info to the initial dataset
  left_join(gwasResults, ., by=c("CHR"="CHR")) %>%
  
  # Add a cumulative position of each SNP
  arrange(CHR, BP) %>%
  mutate( BPcum=BP+tot)
```

Then we need to prepare the X axis. Indeed we do not want to display the cumulative position of SNP in bp, but just show the chromosome name instead.

```{r qqplot-correlation-prepare-x-axis, echo=TRUE, message=FALSE, warning=FALSE}
axisdf = don %>% group_by(CHR) %>% summarize(center=( max(BPcum) + min(BPcum) ) / 2 )
```

Ready to make the plot using ggplot2:

```{r qqplot-correlation-using-ggplot, echo=TRUE, message=FALSE, warning=FALSE}
ggplot(don, aes(x=BPcum, y=-log10(P))) +
    
    # Show all points
    geom_point( aes(color=as.factor(CHR)), alpha=0.8, size=1.3) +
    scale_color_manual(values = rep(c("grey", "skyblue"), 22 )) +
    
    # custom X axis:
    scale_x_continuous( label = axisdf$CHR, breaks= axisdf$center ) +
    scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
  
    # Custom the theme:
    theme_bw() +
    theme( 
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
```

#### Highlight SNPs 

Let's suppose the you have a group of SNP that you want to highlight on the plot. This can be done following almost the same procedure. We just need to add them a flag in the dataframe, and use the flag for the color:

```{r snp-highlight-correlation, echo=TRUE, message=FALSE, warning=FALSE}
# List of SNPs to highlight are in the snpsOfInterest object
# We will use ggrepel for the annotation
library(ggrepel)
# Prepare the dataset
don <- gwasResults %>% 
  
  # Compute chromosome size
  group_by(CHR) %>% 
  summarise(chr_len=max(BP)) %>% 
  
  # Calculate cumulative position of each chromosome
  mutate(tot=cumsum(chr_len)-chr_len) %>%
  dplyr::select(-chr_len) %>%
  
  # Add this info to the initial dataset
  left_join(gwasResults, ., by=c("CHR"="CHR")) %>%
  
  # Add a cumulative position of each SNP
  arrange(CHR, BP) %>%
  mutate( BPcum=BP+tot) %>%
  # Add highlight and annotation information
  mutate( is_highlight=ifelse(SNP %in% snpsOfInterest, "yes", "no")) %>%
  mutate( is_annotate=ifelse(-log10(P)>4, "yes", "no")) 
# Prepare X axis
axisdf <- don %>% group_by(CHR) %>% summarize(center=( max(BPcum) + min(BPcum) ) / 2 )
# Make the plot
ggplot(don, aes(x=BPcum, y=-log10(P))) +
    
    # Show all points
    geom_point( aes(color=as.factor(CHR)), alpha=0.8, size=1.3) +
    scale_color_manual(values = rep(c("grey", "skyblue"), 22 )) +
    
    # custom X axis:
    scale_x_continuous( label = axisdf$CHR, breaks= axisdf$center ) +
    scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
    # Add highlighted points
    geom_point(data=subset(don, is_highlight=="yes"), color="orange", size=2) +
  
    # Add label using ggrepel to avoid overlapping
    geom_label_repel( data=subset(don, is_annotate=="yes"), aes(label=SNP), size=2) +
    # Custom the theme:
    theme_bw() +
    theme( 
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
```

#### A Note about Speed 

A common problem in genomics is the high number of data points. It is not unusual to make a GWAS with millions of SNPs, which makes the plot very long to build. However, it is important to notice that the vast majority of these SNPs have a high p-value and thus do not interest us for the analysis.

A quick work around is thus to plot only SNP with a p-value below a given threshold (~0.05). The graphic will be as informative, but made in seconds. The filtering is straightforward. For example with dplyr:

```r
gwasResults %>% 
  filter(-log10(P)>1)
```

Decreasing the number of data points has another interest: it allows to switch to an interactive version.

### Switch to an Interactive Version with `plotly`

`plotly` is an [HTML widget](http://www.htmlwidgets.org/): an R library that allows to call javascript under the hood to create [interactive visualizations](https://www.r-graph-gallery.com/interactive-charts/). The good thing with `plotly` is that it can transform a [ggplot2 graphic](https://www.r-graph-gallery.com/portfolio/ggplot2-package/) in an interactive version using the [ggplotly](https://www.r-graph-gallery.com/get-the-best-from-ggplotly/) function. Let's apply it to our manhattan plot.

* <u>Note 1</u>: You probably want to filter your data before doing an interactive version. Having thousands of points will slow down the graphic, and you surely don't care about SNP with a high p-value.

* <u>Note 2</u>: the [Manhattanly library](https://github.com/sahirbhatnagar/manhattanly) is another good way to make an interactive manhattan plot. It wraps the plotly library, so you will have less code to type than the example below, but less customization available.

* <u>Note 3</u>: Interactivity allows to: zoom on a specific region of the graphic, hover a SNP, move axis, export figure as png.

```{r manhattan-correlation-plotly, echo=TRUE, message=FALSE, warning=FALSE}
library(plotly)
# Prepare the dataset
don <- gwasResults %>% 
  
  # Compute chromosome size
  group_by(CHR) %>% 
  summarise(chr_len=max(BP)) %>% 
  
  # Calculate cumulative position of each chromosome
  mutate(tot=cumsum(chr_len)-chr_len) %>%
  dplyr::select(-chr_len) %>%
  
  # Add this info to the initial dataset
  left_join(gwasResults, ., by=c("CHR"="CHR")) %>%
  
  # Add a cumulative position of each SNP
  arrange(CHR, BP) %>%
  mutate( BPcum=BP+tot) %>%
  # Add highlight and annotation information
  mutate( is_highlight=ifelse(SNP %in% snpsOfInterest, "yes", "no")) %>%
  # Filter SNP to make the plot lighter
  filter(-log10(P)>0.5)
  
# Prepare X axis
axisdf <- don %>% group_by(CHR) %>% summarize(center=( max(BPcum) + min(BPcum) ) / 2 )
# Prepare text description for each SNP:
don$text <- paste("SNP: ", don$SNP, "\nPosition: ", don$BP, "\nChromosome: ", don$CHR, "\nLOD score:", -log10(don$P) %>% round(2), "\nWhat else do you wanna know", sep="")
# Make the plot
p <- ggplot(don, aes(x=BPcum, y=-log10(P), text=text)) +
    
    # Show all points
    geom_point( aes(color=as.factor(CHR)), alpha=0.8, size=1.3) +
    scale_color_manual(values = rep(c("grey", "skyblue"), 22 )) +
    
    # custom X axis:
    scale_x_continuous( label = axisdf$CHR, breaks= axisdf$center ) +
    scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
    ylim(0,9) +
    # Add highlighted points
    geom_point(data=subset(don, is_highlight=="yes"), color="orange", size=2) +
  
    # Custom the theme:
    theme_bw() +
    theme( 
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
ggplotly(p, tooltip="text")
```

### Circular Version with CMplot

The [CMplot library](https://github.com/YinLiLin/R-CMplot) by Lilin Yin is a good choice if you want to make a circular version of your manhattanplot. I believe than doing a circular version makes sense: it gives less space to all the non significant SNPs that do not interest us, and gives more space for the significant association. Moreover, the CMplot makes their realization straightforward.

```r 
library(CMplot)      #install.packages("CMplot")
CMplot(gwasResults, plot.type="c", r=1.6, cir.legend=TRUE,
        outward=TRUE, cir.legend.col="black", cir.chr.h=.1 ,chr.den.col="orange", file="jpg",
        memo="", dpi=300, chr.labels=seq(1,22))
```
<center>
  ![](correlationHtml/Circular-Manhattan.P.jpg){width=75%}
</center>


```r
CMplot(pig60K,type="p",plot.type="d",bin.size=1e6,chr.den.col=c("darkgreen", "yellow", "red"),file="jpg",memo="",dpi=300,
    file.output=TRUE,verbose=TRUE,width=9,height=6)
```
<center>
  ![](correlationHtml/SNP-Density.trait1.trait2.trait3.jpg){width=75%}
</center>


This section can be located at [YinLiLin Github](https://github.com/YinLiLin/CMplot/blob/master/README.md).

#### A high-quality drawing tool designed for Manhattan plot of genomic analysis

#### Installation

**CMplot** is available on CRAN, so it can be installed with the following R code:

```r
#install.packages("CMplot")
library("CMplot")
# if you want to use the latest version on GitHub:
source("https://raw.githubusercontent.com/YinLiLin/CMplot/master/R/CMplot.r")
```

---

There are two example datasets attached in **CMplot**, users can export and view the details by following R code:

```r
data(pig60K)   #calculated p-values by MLM
data(cattle50K)   #calculated SNP effects by rrblup
head(pig60K)
          SNP Chromosome Position    trait1     trait2     trait3
1 ALGA0000009          1    52297 0.7738187 0.51194318 0.51194318
2 ALGA0000014          1    79763 0.7738187 0.51194318 0.51194318
3 ALGA0000021          1   209568 0.7583016 0.98405289 0.98405289
4 ALGA0000022          1   292758 0.7200305 0.48887140 0.48887140
5 ALGA0000046          1   747831 0.9736840 0.22096836 0.22096836
6 ALGA0000047          1   761957 0.9174565 0.05753712 0.05753712
head(cattle50K)
   SNP chr    pos Somatic cell score  Milk yield Fat percentage
1 SNP1   1  59082        0.000244361 0.000484255    0.001379210
2 SNP2   1 118164        0.000532272 0.000039800    0.000598951
3 SNP3   1 177246        0.001633058 0.000311645    0.000279427
4 SNP4   1 236328        0.001412865 0.000909370    0.001040161
5 SNP5   1 295410        0.000090700 0.002202973    0.000351394
6 SNP6   1 354493        0.000110681 0.000342628    0.000105792
```
As the example datasets, the first three columns are names, chromosome, position of SNPs respectively, the rest of columns are the pvalues of GWAS or effects of GS/GP for traits,  the number of traits is unlimited.
Note: if plotting SNP_Density, only the first three columns are needed.

Now **CMplot** could handle not only Genome-wide association study results, but also SNP effects, Fst, tajima's D and so on.

---

Total 50~ parameters are available in **CMplot**, typing ```?CMplot``` can get the detail function of all parameters.



---
### SNP-Density Plot

```r
CMplot(pig60K,type="p",plot.type="d",bin.size=1e6,chr.den.col=c("darkgreen", "yellow", "red"),file="jpg",memo="",dpi=300,
    file.output=TRUE,verbose=TRUE,width=9,height=6)
# users can personally set the windowsize and the min/max of legend by:
# bin.size=1e6
# bin.range=c(min, max)
# memo: add a character to the output file name
# chr.labels: change the chromosome names
```

<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/illumilla_60K.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/illumilla_60K.jpg" height="460px" width="680px">
</a>
</p>

---

### Circular-Manhattan Plot

#### (1) Genome-Wide Association Study(GWAS)

```r
CMplot(pig60K,type="p",plot.type="c",chr.labels=paste("Chr",c(1:18,"X","Y"),sep=""),r=0.4,cir.legend=TRUE,
        outward=FALSE,cir.legend.col="black",cir.chr.h=1.3,chr.den.col="black",file="jpg",
        memo="",dpi=300,file.output=TRUE,verbose=TRUE,width=10,height=10)
```

<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/9.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/9.jpg" height="480px" width="480px">
</a>
</p>

```r
CMplot(pig60K,type="p",plot.type="c",r=0.4,col=c("grey30","grey60"),chr.labels=paste("Chr",c(1:18,"X","Y"),sep=""),
      threshold=c(1e-6,1e-4),cir.chr.h=1.5,amplify=TRUE,threshold.lty=c(1,2),threshold.col=c("red",
      "blue"),signal.line=1,signal.col=c("red","green"),chr.den.col=c("darkgreen","yellow","red"),
      bin.size=1e6,outward=FALSE,file="jpg",memo="",dpi=300,file.output=TRUE,verbose=TRUE,width=10,height=10)
#Note:
1. if signal.line=NULL, the lines that crosse circles won't be added.
2. if the length of parameter 'chr.den.col' is not equal to 1, SNP density that counts 
   the number of SNP within given size('bin.size') will be plotted around the circle.
```
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/10.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/10.jpg" height="480px" width="480px">
</a>
</p>
#### (2) Genomic Selection/Prediction(GS/GP)
```r
CMplot(cattle50K,type="p",plot.type="c",LOG10=FALSE,outward=TRUE,col=matrix(c("#4DAF4A",NA,NA,"dodgerblue4",
         "deepskyblue",NA,"dodgerblue1", "olivedrab3", "darkgoldenrod1"), nrow=3, byrow=TRUE),
         chr.labels=paste("Chr",c(1:29),sep=""),threshold=NULL,r=1.2,cir.chr.h=1.5,cir.legend.cex=0.5,
         cir.band=1,file="jpg", memo="",dpi=300,chr.den.col="black",file.output=TRUE,verbose=TRUE,
         width=10,height=10)
        
#Note: parameter 'col' can be either vector or matrix, if a matrix, each trait can be plotted in different colors.
```
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/11.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/11.jpg" height="480px" width="480px">
</a>
</p>
---
### Single_track Rectangular-Manhattan Plot
#### Genome-Wide Association Study(GWAS)
```r
CMplot(pig60K,type="p",plot.type="m",LOG10=TRUE,threshold=NULL,file="jpg",memo="",dpi=300,
    file.output=TRUE,verbose=TRUE,width=14,height=6,chr.labels.angle=45)
# 'chr.labels.angle': adjust the angle of labels of x-axis (-90 < chr.labels.angle < 90).
```
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/1.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/1.jpg" height="385px" width="900px">
</a>
</p>
#### Amplify Signals on `pch`, `cex` and `col`
```r
CMplot(pig60K, plot.type="m", col=c("grey30","grey60"), LOG10=TRUE, ylim=c(2,12), threshold=c(1e-6,1e-4),
        threshold.lty=c(1,2), threshold.lwd=c(1,1), threshold.col=c("black","grey"), amplify=TRUE,
        chr.den.col=NULL, signal.col=c("red","green"), signal.cex=c(1.5,1.5),signal.pch=c(19,19),
        file="jpg",memo="",dpi=300,file.output=TRUE,verbose=TRUE,width=14,height=6)
#Note: if the ylim is setted, then CMplot will only plot the points among this interval,
#       ylim can be vector or list, if it is a list, different traits can be assigned with
#       different range at y-axis.
```
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/2.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/2.jpg" height="370px" width="900px">
</a>
</p>
#### Attach Chromosome Density on the bottom of Manhattan Plot
```r
CMplot(pig60K, plot.type="m", LOG10=TRUE, ylim=NULL, threshold=c(1e-6,1e-4),threshold.lty=c(1,2),
        threshold.lwd=c(1,1), threshold.col=c("black","grey"), amplify=TRUE,bin.size=1e6,
        chr.den.col=c("darkgreen", "yellow", "red"),signal.col=c("red","green"),signal.cex=c(1.5,1.5),
        signal.pch=c(19,19),file="jpg",memo="",dpi=300,file.output=TRUE,verbose=TRUE,
        width=14,height=6)
        
#Note: if the length of parameter 'chr.den.col' is bigger than 1, SNP density that counts 
   the number of SNP within given size('bin.size') will be plotted.
```
</p>
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/2_2.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/2_2.jpg" height="385px" width="900px">
</a>
#### Highlight a Group of SNPs on `pch`, `cex`, `type`, and `col`
```r
signal <- pig60K$Position[which.min(pig60K$trait2)]
SNPs <- pig60K$SNP[pig60K$Chromosome==13 & 
        pig60K$Position<(signal+1000000)&pig60K$Position>(signal-1000000)]
CMplot(pig60K, plot.type="m",LOG10=TRUE,col=c("grey30","grey60"),highlight=SNPs,
        highlight.col="green",highlight.cex=1,highlight.pch=19,file="jpg",memo="",
        chr.border=TRUE,dpi=300,file.output=TRUE,verbose=TRUE,width=14,height=6)
#Note:
'highlight' could be vector or list, if it is a vector, all traits will use the same highlighted SNPs index, 
if it is a list, the length of the list should equal to the number of traits.
highlight.col, highlight.cex, highlight.pch can be value or vector, if its length equals to the length of highlighted SNPs,
each SNPs have its special colour, size and shape.
```
</p>
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/2-3.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/2-3.jpg" height="385px" width="900px">
</a>
```r
SNPs <-  pig60K[pig60K$trait2 < 1e-4, 1]
CMplot(pig60K,type="h",plot.type="m",LOG10=TRUE,highlight=SNPs,highlight.type="p",
        highlight.col=NULL,highlight.cex=1.2,highlight.pch=19,file="jpg",memo="",
        dpi=300,file.output=TRUE,verbose=TRUE,width=14,height=6,band=0.6)
```
</p>
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/2_6.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/2_6.jpg" height="385px" width="900px">
</a>
```r
SNPs <-  pig60K[pig60K$trait2 < 1e-4, 1]
CMplot(pig60K,type="p",plot.type="m",LOG10=TRUE,highlight=SNPs,highlight.type="h",
        col=c("grey30","grey60"),highlight.col="darkgreen",highlight.cex=1.2,highlight.pch=19,
        file="jpg",dpi=300,file.output=TRUE,verbose=TRUE,width=14,height=6)
```
</p>
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/2_6_1.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/2_6_1.jpg" height="385px" width="900px">
</a>
	
#### Visualize only oOne Chromosome
```r
CMplot(pig60K[pig60K$Chromosome==13, ], plot.type="m",LOG10=TRUE,col=c("grey60"),highlight=SNPs,
        highlight.col="green",highlight.cex=1,highlight.pch=19,file="jpg",memo="", 
        threshold=c(1e-6,1e-4),threshold.lty=c(1,2),threshold.lwd=c(1,2), width=9,height=6,
        threshold.col=c("red","blue"),amplify=FALSE,dpi=300,file.output=TRUE,verbose=TRUE)
```
</p>
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/2_4.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/2_4.jpg" height="460px" width="680px">
</a>
#### Add Genes or SNP Names around the Highlighted SNPs
```r
SNPs <- pig60K[pig60K[,5] < (0.05 / nrow(pig60K)), 1]
genes <- paste("GENE", 1:length(SNPs), sep="_")
set.seed(666666)
CMplot(pig60K[,c(1:3,5)], plot.type="m",LOG10=TRUE,col=c("grey30","grey60"),highlight=SNPs,
        highlight.col=c("red","blue","green"),highlight.cex=1,highlight.pch=c(15:17), highlight.text=genes,      
        highlight.text.col=c("red","blue","green"),threshold=0.05/nrow(pig60K),threshold.lty=2,   
        amplify=FALSE,file="jpg",memo="",dpi=300,file.output=TRUE,verbose=TRUE,width=14,height=6)
#Note:
'highlight', 'highlight.text', 'highlight.text.xadj', 'highlight.text.yadj' could be vector or list, if it is a vector, 
all traits will use the same highlighted SNPs index and text, if it is a list, the length of the list should equal to the number of traits.
the order of 'highlight.text' must be consistent with 'highlight'
highlight.text.cex: value or vecter, control the size of added text
highlight.text.font: value or vecter, control the font of added text
highlight.text.xadj: value or vecter or list for multiple traits, -1, 0, 1 limited, control the position of text around the highlighted SNPs,
                     -1(left), 0(center), 1(right)
highlight.text.yadj: value or vector or list for multiple traits, same as above, -1(down), 0(center), 1(up)
```
</p>
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/2-5.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/2-5.jpg" height="385px" width="900px">
</a>
 
#### Genomic
Selection/Prediction(GS/GP) or other none p-values
```r
CMplot(cattle50K, plot.type="m", band=0.5, LOG10=FALSE, ylab="SNP effect",threshold=0.015,
        threshold.lty=2, threshold.lwd=1, threshold.col="red", amplify=TRUE, width=14,height=6,
        signal.col=NULL, chr.den.col=NULL, file="jpg",memo="",dpi=300,file.output=TRUE,
        verbose=TRUE,cex=0.8)
#Note: if signal.col=NULL, the significant SNPs will be plotted with original colors.
```
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/3.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/3.jpg" height="385px" width="900px">
</a>
</p>
```r
cattle50K[,4:ncol(cattle50K)] <- apply(cattle50K[,4:ncol(cattle50K)], 2, 
         function(x) x*sample(c(1,-1), length(x), rep=TRUE))
CMplot(cattle50K, type="h",plot.type="m", band=0.5, LOG10=FALSE, ylab="SNP effect",ylim=c(-0.02,0.02),
        threshold.lty=2, threshold.lwd=1, threshold.col="red", amplify=FALSE,cex=0.6,
        chr.den.col=NULL, file="jpg",memo="",dpi=300,file.output=TRUE,verbose=TRUE)
#Note: Positive and negative values are acceptable.
```
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/PN_1.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/PN_1.jpg" height="385px" width="900px">
</a>
</p>
### Multi_tracks Rectangular-Manhattan Plot
```r
SNPs <- list(
	pig60K$SNP[pig60K$trait1<1e-6],
	pig60K$SNP[pig60K$trait2<1e-6],
	pig60K$SNP[pig60K$trait3<1e-6]
)
CMplot(pig60K, plot.type="m",multracks=TRUE,threshold=c(1e-6,1e-4),threshold.lty=c(1,2), 
        threshold.lwd=c(1,1), threshold.col=c("black","grey"), amplify=TRUE,bin.size=1e6,
        chr.den.col=c("darkgreen", "yellow", "red"), signal.col=c("red","green","blue"),
        signal.cex=1, file="jpg",memo="",dpi=300,file.output=TRUE,verbose=TRUE,
        highlight=SNPs, highlight.text=SNPs, highlight.text.cex=1.4)
#Note: if you are not supposed to change the color of signal, 
          please set signal.col=NULL and highlight.col=NULL.
```
#### a. All Traits in One Axes
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/4_1.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/4_1.jpg" height="385px" width="900px">
</a>
</p>
#### b. All Traits in Separated Axes:
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/5_new.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/5_new.jpg" height="900px" width="640px">
</a>
</p>
---
### Single_track Q-Q Plot
```r
CMplot(pig60K,plot.type="q",box=FALSE,file="jpg",memo="",dpi=300,
    conf.int=TRUE,conf.int.col=NULL,threshold.col="red",threshold.lty=2,
    file.output=TRUE,verbose=TRUE,width=5,height=5)
```
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/6.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/6.jpg" height="450px" width="450px">
</a>
</p>
### Multi_tracks Q-Q plot
```r
pig60K$trait1[sample(1:nrow(pig60K), round(nrow(pig60K)*0.80))] <- NA
pig60K$trait2[sample(1:nrow(pig60K), round(nrow(pig60K)*0.25))] <- NA
CMplot(pig60K,plot.type="q",col=c("dodgerblue1", "olivedrab3", "darkgoldenrod1"),threshold=1e-6,
        ylab.pos=2,signal.pch=c(19,6,4),signal.cex=1.2,signal.col="red",conf.int=TRUE,box=FALSE,multracks=
        TRUE,cex.axis=2,file="jpg",memo="",dpi=300,file.output=TRUE,verbose=TRUE,ylim=c(0,8),width=5,height=5)
```
#### a. All Traits in a Axes
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/8.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/8.jpg" height="450px" width="450px">
</a>
</p>
#### b. all traits in separated axes:
<p align="center">
<a href="https://raw.githubusercontent.com/YinLiLin/R-CMplot/master/Figure/7.jpg">
<img src="https://github.com/YinLiLin/CMplot/raw/master/Figure/7.jpg" height="280px" width="1400px">
</a>
</p>
---
## Heatmap
---
A [heatmap](https://www.data-to-viz.com/graph/heatmap.html) is a graphical representation of data where the individual values contained in a matrix are represented as colors. This page displays many examples built with R, both static and interactive.
#### Using THE `heatmap()` Function
The `heatmap()` function is natively provided in R. It produces high quality matrix and offers statistical tools to normalize input data, run clustering algorithm and visualize the result with dendrograms. It is one of the very rare case where I prefer `base R` to `ggplot2`.
#### Most Basic Heatmap
<u>How to do it</u>: below is the most basic [heatmap](https://www.r-graph-gallery.com/heatmap.html) you can build in base R, using the `heatmap()` function with no parameters. Note that it takes as input a matrix. If you have a data frame, you can convert it to a matrix with `as.matrix()`, but you need numeric variables only. 
<u>How to read it</u>: each column is a variable. Each observation is a row. Each square is a value, the closer to yellow the higher. You can transpose the matrix with `t(data)` to swap X and Y axis.
<u>Note</u>: as you can see this heatmap is not very insightful: all the variation is absorbed by the `hp` and `disp` variables that have very high values compared to the others. 
```{r heatmap-correlation-base-r, echo=TRUE, message=FALSE, warning=FALSE}
# The mtcars dataset:
data <- as.matrix(mtcars)
# Default Heatmap
heatmap(data)
```
### Normalization
Normalizing the matrix is done using the `scale` argument of the `heatmap()` function. It can be applied to `row` or to `column`. Here the `column` option is chosen, since we need to absorb the variation between column.
```{r heatmap-correlation-base-r-normailization, echo=TRUE, message=FALSE, warning=FALSE}
# Use 'scale' to normalize
heatmap(data, scale="column")
```
### Dendrogram and Reordering
You may have noticed that order of both rows and columns is different compare to the native `mtcar` matrix. This is because `heatmap()` reorders both variables and observations using a clustering algorithm: it computes the distance between each pair of rows and columns and try to order them by similarity.
Moreover, the corresponding `dendrograms` are provided beside the heatmap. We can avoid it and just visualize the raw matrix: use the `Rowv` and `Colv` arguments as follow.
```{r heatmap-correlation-base-r-dendrogram-reordering, echo=TRUE, message=FALSE, warning=FALSE}
# No dendrogram nor reordering for neither column or row
heatmap(data, Colv = NA, Rowv = NA, scale="column")
```
### Color Palette
There are several ways to custom the color palette:
* Use the native palettes of R: `terrain.color()`, `rainbow()`, `heat.colors()`, `topo.colors()` or `cm.colors()`.
* Use the palettes proposed by RColorBrewer. See list of available palettes [here]().
```{r heatmap-correlation-color-palette, echo=TRUE, message=FALSE, warning=FALSE}
# 1: native palette from R
heatmap(data, scale="column", col = cm.colors(256))
heatmap(data, scale="column", col = terrain.colors(256))
 
# 2: Rcolorbrewer palette
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(data, scale="column", col = coul)
```
### Custom Layout
You can custom title & axis titles with the usual main and `xlab`/`ylab` arguments (left).
You can also change labels with `labRow`/`colRow` and their size with `cexRow`/`cexCol`
```{r heatmap-correlation-base-r-custom-layout, echo=TRUE, message=FALSE, warning=FALSE}
# Add classic arguments like main title and axis title
heatmap(data, Colv = NA, Rowv = NA, scale="column", col = coul, xlab="variable", ylab="car", main="heatmap")
 
# Custom x and y labels with cexRow and labRow (col respectively)
heatmap(data, scale="column", cexRow=1.5, labRow=paste("new_", rownames(data),sep=""), col= colorRampPalette(brewer.pal(8, "Blues"))(25))
```
### Add Color beside Heatmap
Often, heatmap intends to compare the observed structure with an expected one.
You can add a vector of color beside the heatmap to represents the expected structure using the `RowSideColors` argument.
```{r heatmap-correlation-base-r-add-color, echo=TRUE, message=FALSE, warning=FALSE}
# Example: grouping from the first letter:
my_group <- as.numeric(as.factor(substr(rownames(data), 1 , 1)))
colSide <- brewer.pal(9, "Set1")[my_group]
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(data, Colv = NA, Rowv = NA, scale="column" , RowSideColors=colSide, col=colMain   )
```
### Using `geom_title()` from `ggplot2`
`ggplot2` also allows to build heatmaps thanks to `geom_tile()`. However, I personally prefer the `heatmap()` function above since only it offers option for normalization, clustering and Dendrogram.
### `ggplot2` Heatmap
This document provides several examples of [heatmaps](https://www.r-graph-gallery.com/heatmap.html) built with `R` and [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html). It describes the main customization you can apply, with explanation and reproducible code. 
<u>Note</u>: The native [heatmap() function](https://www.r-graph-gallery.com/215-the-heatmap-function.html) provides more options for data normalization and clustering. Consider it as a valuable option.
#### Most Basic Heatmap with `ggplot2`
This is the most basic heatmap you can build with `R` and `ggplot2`, using the `geom_tile()` function. Input data must be a long format where each row provides an observation. At least 3 variables are needed per observation:
* `x`: position on the X axis.
* `y`: position on the Y axis.
* `fill`: the numeric value that will be translated in a color.
```{r heatmap-correlation-ggplot, echo=TRUE, message=FALSE, warning=FALSE}
# Library
library(ggplot2)
# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)
 
# Heatmap 
ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile()
```
### Control Color Palette
Color palette can be changed like in any [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html) chart. Above are 3 examples using different methods:
* `scale_fill_gradient()` to provide extreme colors of the palette.
* `scale_fill_distiller()` to provide a [ColorBrewer](https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html) palette.
* `scale_fill_viridis()` to use Viridis. Do not forget `discrete=FALSE` for a continuous variable.
```{r heatmap-correlation-ggplot-color-control, echo=TRUE, message=FALSE, warning=FALSE}
# Library
library(ggplot2)
library(hrbrthemes)
# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)
 
# Give extreme colors:
ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  theme_ipsum()
# Color Brewer palette
ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile() +
  scale_fill_distiller(palette = "RdPu") +
  theme_ipsum()
# Color Brewer palette
library(viridis)
ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  theme_ipsum()
```
### From Wide Input Format
It is a common issue to have a wide matrix as input, as for the `volcano` dataset. In this case, you need to tidy it with the `gather()` function of the `tidyr` package to visualize it with [ggplot](https://www.r-graph-gallery.com/ggplot2-package.html).
```{r heatmap-correlation-ggplot-wide-input-format, echo=TRUE, message=FALSE, warning=FALSE}
# Library
library(ggplot2)
library(tidyr)
library(tibble)
library(hrbrthemes)
library(dplyr)
# Volcano dataset
#volcano
# Heatmap 
volcano %>%
  
  # Data wrangling
  as_tibble() %>%
  rowid_to_column(var="X") %>%
  gather(key="Y", value="Z", -1) %>%
  
  # Change Y to numeric
  mutate(Y=as.numeric(gsub("V","",Y))) %>%
  # Viz
  ggplot(aes(X, Y, fill= Z)) + 
    geom_tile() +
    theme_ipsum() +
    theme(legend.position="none")
```
### Turn it Interactive with `plotly`
One of the nice feature of [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html) is that charts can be turned interactive in seconds thanks to `plotly`. You just need to wrap your chart in an object and call it in the `ggplotly()` function. 
Often, it is a good practice to custom the text available in the tooltip.
 
<u>Note</u>: try to hover cells to see the tooltip, select an area to zoom in.
```{r heatmap-correlation-ggplot-interactivity, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', fig.height=5, fig.width=5}
# Library
library(ggplot2)
library(hrbrthemes)
library(plotly)
# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)
 
# new column: text for tooltip:
data <- data %>%
  mutate(text = paste0("x: ", x, "\n", "y: ", y, "\n", "Value: ",round(Z,2), "\n", "What else?"))
# classic ggplot, with text in aes
p <- ggplot(data, aes(X, Y, fill= Z, text=text)) + 
  geom_tile() +
  theme_ipsum()
ggplotly(p, tooltip="text")
# save the widget
# library(htmlwidgets)
# saveWidget(pp, file=paste0( getwd(), "/HtmlWidget/ggplotlyHeatmap.html"))
```
### Interactive Heatmaps from R
Three options exist to build an interactive heatmap from R:
* `plotly`: As described above, plotly allows to turn any [heatmap](https://www.r-graph-gallery.com/heatmap.html) made with `ggplot2` interactive.
* `d3heatmap`: A package that uses the same syntax as the base R [heatmap()](https://www.r-graph-gallery.com/heatmap.html) function to make interactive version.
* `heatmaply`: The most flexible option, allowing many different kind of customization. See the code of the chart beside [here](https://www.data-to-viz.com/graph/heatmap.html).
#### Definition
A `heatmap` is a graphical representation of data where the individual values contained in a matrix are represented as colors. It is a bit like looking a data table from above.
Here is an example showing 8 general features like population or life expectancy for about 30 countries in 2015. Data come from the French National Institute of [Demographic Studies](https://www.ined.fr/en/everything_about_population/data/all-countries/?lst_continent=908&lst_pays=926).
```{r heatmap-correlation-interactive, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', fig.height=5, fig.width=5}
# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)
library(d3heatmap)
# Load data 
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/multivariate.csv", header=T, sep=";")
colnames(data) <- gsub("\\.", " ", colnames(data))
# Select a few country
data <- data %>% 
  filter(Country %in% c("France", "Sweden", "Italy", "Spain", "England", "Portugal", "Greece", "Peru", "Chile", "Brazil", "Argentina", "Bolivia", "Venezuela", "Australia", "New Zealand", "Fiji", "China", "India", "Thailand", "Afghanistan", "Bangladesh", "United States of America", "Canada", "Burundi", "Angola", "Kenya", "Togo")) %>%
  arrange(Country) %>%
  mutate(Country = factor(Country, Country))
# Matrix format
mat <- data
rownames(mat) <- mat[,1]
mat <- mat %>% dplyr::select(-Country, -Group, -Continent)
mat <- as.matrix(mat)
# Heatmap
#d3heatmap(mat, scale="column", dendrogram = "none", width="800px", height="80Opx", colors = "Blues")
library(heatmaply)
p <- heatmaply(mat, 
        dendrogram = "none",
        xlab = "", ylab = "", 
        main = "",
        scale = "column",
        margins = c(60,100,40,20),
        grid_color = "white",
        grid_width = 0.00001,
        titleX = FALSE,
        hide_colorbar = TRUE,
        branches_lwd = 0.1,
        label_names = c("Country", "Feature:", "Value"),
        fontsize_row = 5, fontsize_col = 5,
        labCol = colnames(mat),
        labRow = rownames(mat),
        heatmap_layers = theme(axis.line=element_blank())
        )
p
```
#### What for
Heatmap is really useful to display a `general view` of numerical data, not to extract specific data point. In the graphic above, the huge population size of China and India pops out for example.
Heatmap is also useful to display the result of `hierarchical clustering`. Basically, clustering checks what countries tend to have the same features on their numeric variables, what countries are similar. The usual way to represent the result is to use [dendrogram](https://www.data-to-viz.com/graph/dendrogram.html). This type of chart can be drawn on top of the heatmap:
```{r heatmap-correlation-heatmaply, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', fig.height=5, fig.width=5}
p <- heatmaply(mat, 
        #dendrogram = "row",
        xlab = "", ylab = "", 
        main = "",
        scale = "column",
        margins = c(60,100,40,20),
        grid_color = "white",
        grid_width = 0.00001,
        titleX = FALSE,
        hide_colorbar = TRUE,
        branches_lwd = 0.1,
        label_names = c("Country", "Feature:", "Value"),
        fontsize_row = 5, fontsize_col = 5,
        labCol = colnames(mat),
        labRow = rownames(mat),
        heatmap_layers = theme(axis.line=element_blank())
        )
p
# save the widget
# library(htmlwidgets)
# saveWidget(p, file= "~/Desktop/R-graph-gallery/HtmlWidget/heatmapInter.html")
```
### Heatmap for Time Series
Heatmaps can be a very good alternative to visualize [time series](https://www.r-graph-gallery.com/time-series.html), especially when the time frame you study is repeating, like weeks. Here is a customized example, but visit the [time series section](https://www.r-graph-gallery.com/time-series.html) for more.
A submission by [John MacKintosh](https://johnmackintosh.com/) who visualized meteorological data using a heatmap built with ggplot2. Initial code is stored on [github](https://gist.github.com/johnmackintosh/520643a1f82a0c7df00cf949ba98a4e9) and displayed below:
### The Hourly Heatmap
 
A [heatmap](https://www.r-graph-gallery.com/heatmap.html) used to display time series with R and [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html). A submission by [John MacKintosh](http://johnmackintosh.com/) with reproducible code.
A submission by [John MacKintosh](http://johnmackintosh.com/) who visualized meteorological data using a heatmap built with [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html). Initial code is stored on [github](https://gist.github.com/johnmackintosh/520643a1f82a0c7df00cf949ba98a4e9) and displayed below:
```r
library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 
 
 
data <- data(Trentino_hourly_T,package = "Interpol.T")
 
names(h_d_t)[1:5]<- c("stationid","date","hour","temp","flag")
df <- tbl_df(h_d_t) %>%
  filter(stationid =="T0001")
 
df <- df %>% mutate(year = year(date),
                  month = month(date, label=TRUE),
                  day = day(date))
  
df$date<-ymd(df$date) # not necessary for plot but 
#useful if you want to do further work with the data
 
#cleanup
rm(list=c("h_d_t","mo_bias","Tn","Tx",
          "Th_int_list","calibration_l",
          "calibration_shape","Tm_list"))
 
 
#create plotting df
df <-df %>% dplyr::select(stationid,day,hour,month,year,temp)%>%
        fill(temp) #optional - see note below
 
# Re: use of fill
# This code is for demonstrating a visualisation technique
# There are 5 missing hourly values in the dataframe.
 
# see the original plot here (from my ggplot demo earlier this year) to see the white spaces where the missing values occcur:
# https://github.com/johnmackintosh/ggplotdemo/blob/master/temp8.png 
 
# I used 'fill' from  tidyr to take the prior value for each missing value and replace the NA
# This is a quick fix for the blog section only - _do not_ do this with your real world data
 
# Should really use either use replace_NA or complete(with fill)in tidyr 
# OR 
# Look into more specialist way of replacing these missing values -e.g. imputation.
 
 
 
statno <-unique(df$stationid)
 
 
 
######## Plotting starts here#####################
p <-ggplot(df,aes(day,hour,fill=temp))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Hrly Temps C",option ="C")
p <-p + facet_grid(year~month)
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(df$hour))
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Hourly Temps - Station",statno), x="Day", y="Hour Commencing")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
 
# you will want to expand your plot screen before this bit!
p #awesomeness
```
<center>
  ![](https://www.r-graph-gallery.com/283-the-hourly-heatmap_files/figure-html/thecode-1.png)
</center>
### Using the `levelplot()` Function of `Lattice`
This document explains how to use the `levelplot()` function of the lattice R package to build [heatmaps](https://www.r-graph-gallery.com/heatmap.html). 
#### Basis use of `levelplot()`
The `lattice` package allows to build [heatmaps](https://www.r-graph-gallery.com/heatmap.html) thanks to the `levelplot()` function. 
<u>Input data</u>: here input is a data frame with 3 columns prividing the X and Y coordinate of the cell and its value. (<u>Long</u> format).
```{r heatmap-correlation-lattice-levelplot, echo=TRUE, message=FALSE, warning=FALSE}
# Load the lattice package
library("lattice")
 
# Dummy data
x <- seq(1,10, length.out=20)
y <- seq(1,10, length.out=20)
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)
## Try it out
levelplot(Z ~ X*Y, data=data  ,xlab="X",
          main="")
```
### From Wide Input Matrix
Previous example of this document was based on a data frame at the long format. Here, a square matrix is used instead. It is the second format understood by the `levelplot()` function.
<u>Note</u>: here row and column order isn't respected in the heatmap.
```{r heatmap-correlation-wide-input-matrix, echo=TRUE, message=FALSE, warning=FALSE}
# Load the library
library("lattice")
 
# Dummy data
data <- matrix(runif(100, 0, 5) , 10 , 10)
colnames(data) <- letters[c(1:10)]
rownames(data) <- paste( rep("row",10) , c(1:10) , sep=" ")
 
# plot it flipping the axis
levelplot(data)
```

### Flip and Reorder Axis

The `t()` function of R allows to transpose the input matrix, and thus to flip X and Y coordinates. 

Moreover, you can reverse matrix order as shown below to reverse order in the heatmap as well. Now the heatmap is organized exactly as the input matrix.

```{r heatmap-correlation-flip-reorder-axis, echo=TRUE, message=FALSE, warning=FALSE}
# Load the library
library("lattice")
 
# Dummy data
data <- matrix(runif(100, 0, 5) , 10 , 10)
colnames(data) <- letters[c(1:10)]
rownames(data) <- paste( rep("row",10) , c(1:10) , sep=" ")
 
# plot it flipping the axis
levelplot( t(data[c(nrow(data):1) , ]),
           col.regions=heat.colors(100))
```

### Custom Colors

There are several ways to custom the color palette:

* Native palettes of R: `terrain.color()`, `rainbow()`, `heat.colors()`, `topo.colors()` or `cm.colors()`
* Palettes of RColorBrewer. See list of available palettes [here](https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html).
* Palettes of Viridis: **viridis**, **magma**, **inferno**, **plasma**.

```{r heatmap-correlation-lattice-custom-colors, echo=TRUE, message=FALSE, warning=FALSE}
# Lattice package
require(lattice)
# The volcano dataset is provided, it looks like that:
#head(volcano)
# 1: native palette from R
levelplot(volcano, col.regions = terrain.colors(100)) # try cm.colors() or terrain.colors()
# 2: Rcolorbrewer palette
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
levelplot(volcano, col.regions = coul) # try cm.colors() or terrain.colors()
# 3: Viridis
library(viridisLite)
coul <- viridis(100)
levelplot(volcano, col.regions = coul) 
#levelplot(volcano, col.regions = magma(100)) 
```

### Heatmap with Smoothing - LatticeExtra

The `latticeExtra` package allows to build a [heatmap](https://www.r-graph-gallery.com/heatmap.html) with smoothing. Here is how with reproducible code. 

The levelplot function of lattice allows to deal with 3 numeric variables as shown in the heatmap section of the gallery.

Here, the idea is to plot data points as circles using `panel.levelplot.points` to get a scatterplot. Then, a layer is added using `panel.2dsmoother` to show the general distribution of the third numeric variable.

```{r heatmap-correlation-lattice-extra-smoothing, echo=TRUE, message=FALSE, warning=FALSE}
# library
library(latticeExtra) 
 
# create data
set.seed(1) 
data <- data.frame(x = rnorm(100), y = rnorm(100)) 
data$z <- with(data, x * y + rnorm(100, sd = 1)) 
 
# showing data points on the same color scale 
levelplot(z ~ x * y, data, 
          panel = panel.levelplot.points, cex = 1.2
    ) + 
    layer_(panel.2dsmoother(..., n = 200))
```

## Correlogram

---

A [correlogram](https://www.data-to-viz.com/graph/correlogram.html) or correlation matrix allows to analyse the relationship between each pair of numeric variables in a dataset. It gives a quick overview of the whole dataset. It is more used for <u>exploratory</u> purpose than <u>explanatory</u>.

#### Using the `ggally` Package

The `GGally` package offers great options to build [correlograms](https://www.data-to-viz.com/graph/correlogram.html). The `ggpairs()` function build a [classic correlogram](https://www.r-graph-gallery.com/199-correlation-matrix-with-ggally.html) with scatterplot, correlation coefficient and variable distribution. On top of that, it is possible to inject [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html) code, for instance to [color categories](https://www.r-graph-gallery.com/199-correlation-matrix-with-ggally.html#category).

#### Correlation Matrix with Ggally

This section explains how to build a [correlogram](https://www.r-graph-gallery.com/correlogram.html) with the `ggally` R package. It provides several reproducible examples with explanation and `R` code.

#### Scatterplot Matrix with `Ggpairs()`

The `ggpairs()` function of the GGally package allows to build a great [scatterplot matrix](https://www.r-graph-gallery.com/correlogram.html). 

[Scatterplots](https://www.r-graph-gallery.com/scatterplot.html) of each pair of numeric variable are drawn on the left part of the figure. Pearson correlation is displayed on the right. Variable distribution is available on the diagonal.


```r
# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
library(GGally)
 
# Create data 
data <- data.frame( var1 = 1:100 + rnorm(100,sd=20), v2 = 1:100 + rnorm(100,sd=27), v3 = rep(1, 100) + rnorm(100, sd = 1)) 
data$v4 = data$var1 ** 2 
data$v5 = -(data$var1 ** 2) 
 
# Check correlations (as scatterplots), distribution and print corrleation coefficient 
ggpairs(data, title="correlogram with ggpairs()") 
```
![](https://www.r-graph-gallery.com/199-correlation-matrix-with-ggally_files/figure-html/thecode-1.png)

### Visualize Correlation with `ggcorr()`

The `ggcorr()` function allows to visualize the correlation of each pair of variable as a square. Note that the `method` argument allows to pick the correlation type you desire.

```r
# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
library(GGally)
 
# Create data 
data <- data.frame( var1 = 1:100 + rnorm(100,sd=20), v2 = 1:100 + rnorm(100,sd=27), v3 = rep(1, 100) + rnorm(100, sd = 1)) 
data$v4 = data$var1 ** 2 
data$v5 = -(data$var1 ** 2) 
 
# Check correlation between variables
#cor(data) 
 
# Nice visualization of correlations
ggcorr(data, method = c("everything", "pearson")) 
```

![](https://www.r-graph-gallery.com/199-correlation-matrix-with-ggally_files/figure-html/thecode2-1.png)

### Change Plot Types

Change the type of plot used on each part of the [correlogram](https://www.r-graph-gallery.com/correlogram.html). This is done with the `upper` and `lower` argument.

```r
# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
library(GGally)
 
# From the help page:
data(tips, package = "reshape")
ggpairs(
  tips[, c(1, 3, 4, 2)],
  upper = list(continuous = "density", combo = "box_no_facet"),
  lower = list(continuous = "points", combo = "dot_no_facet")
)
```
![](https://www.r-graph-gallery.com/199-correlation-matrix-with-ggally_files/figure-html/thecode4-1.png)

### Split by Group

It is possible to use [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html) aesthetics on the chart, for instance to color each category.

```r
# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
library(GGally)
 
# From the help page:
data(flea)
ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species)) 
```

![](https://www.r-graph-gallery.com/199-correlation-matrix-with-ggally_files/figure-html/thecode3-1.png)

### Using the `corrgram` Package

The `corrgram` is another great alternative to build [correlograms](https://www.data-to-viz.com/graph/correlogram.html). You can choose what to display in the upper, lower and diagonal part of the figure: scatterplot, pie chart, text, ellipse and more.

#### Correlogram with the `corrgram` Library

This section explains how to build a [correlogram](https://www.r-graph-gallery.com/correlogram.html) with the `corrgram` R package. It provides several reproducible examples with explanation and `R` code.

#### Scatterplot Matrix with `ggpairs()`

The `corrgram` package allows to build [correlogram](https://www.r-graph-gallery.com/correlogram.html). The output allows to check the relationship between each pair of a set of numeric variable.

Relationship can be visualized with different methods:

* `panel.ellipse` to display ellipses
* `panel.shade` for colored squares
* `panel.pie` for pie charts
* `panel.pts` for scatterplots

```r
# Corrgram library
library(corrgram)
# mtcars dataset is natively available in R
# head(mtcars)
# First
corrgram(mtcars, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="Car Milage Data in PC2/PC1 Order") 
```

![](https://www.r-graph-gallery.com/199-correlation-matrix-with-ggally_files/figure-html/thecode4-1.png)

```r
# Second
corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse, upper.panel=panel.pts, text.panel=panel.txt, diag.panel=panel.minmax, main="Car Milage Data in PC2/PC1 Order") 
```

![](https://www.r-graph-gallery.com/115-study-correlations-with-a-correlogram_files/figure-html/thecode-2.png)

```r
# Third
corrgram(mtcars, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt, main="Car Milage Data (unsorted)")
```

![](https://www.r-graph-gallery.com/115-study-correlations-with-a-correlogram_files/figure-html/thecode-3.png)

### Visualize Correlation with `ggcorr()`

The `gcorr()` function allows to visualize the correlation of each pair of variable as a square. Note that the `method` argument allows to pick the correlation type you desire.

```r
# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables
library(GGally)
# Create data
data <- data.frame( var1 = 1:100 + rnorm(100,sd=20), v2 = 1:100 + rnorm(100,sd=27), v3 = rep(1, 100) + rnorm(100, sd = 1))
data$v4 = data$var1 ** 2
data$v5 = -(data$var1 ** 2)
# Check correlation between variables
#cor(data)
# Nice visualization of correlations
ggcorr(data, method = c("everything", "pearson"))
```

![](https://www.r-graph-gallery.com/115-study-correlations-with-a-correlogram_files/figure-html/thecode2-1.png)

### Split by Group

It is possible to use [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html) aesthetics on the chart, for instance to color each category.

```r
# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables
library(GGally)
# From the help page:
data(flea)
ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species))
```
![](https://www.r-graph-gallery.com/115-study-correlations-with-a-correlogram_files/figure-html/thecode3-1.png)

### Change plot types

Change the type of plot used on each part of the [correlogram](https://www.r-graph-gallery.com/correlogram.html). This is done with the `upper` and `lower` argument.

```r
# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables
library(GGally)
# From the help page:
data(tips, package = "reshape")
ggpairs(
  tips[, c(1, 3, 4, 2)],
  upper = list(continuous = "density", combo = "box_no_facet"),
  lower = list(continuous = "points", combo = "dot_no_facet")
)
```

![](https://www.r-graph-gallery.com/115-study-correlations-with-a-correlogram_files/figure-html/thecode4-1.png)

### Other Methods

Lesser known ways to build correlogram with R, like the `ellipse` package, the `plot()` function and the car package.

#### Correlogram with the Ellipse Package

This section explains how to build a [correlogram](https://www.r-graph-gallery.com/correlogram.html) based on `ellipses` with the ellipse R package. It provides several reproducible examples with explanation and `R` code.

#### Scatterplot Matrix with `ggpairs()`

The `ellipse` package allows to build a correlogram thanks to the `plotcorr()` function.

First of all, you have to compute the correlation matrix of your dataset using the `cor()` function of R. Each correlation will be represented as an ellipse by the `plotcorr()` function. Color, shape and orientation depend on the correlation value.

```r
# Libraries
library(ellipse)
library(RColorBrewer)
 
# Use of the mtcars data proposed by R
data <- cor(mtcars)
 
# Build a Pannel of 100 colors with Rcolor Brewer
my_colors <- brewer.pal(5, "Spectral")
my_colors <- colorRampPalette(my_colors)(100)
 
# Order the correlation matrix
ord <- order(data[1, ])
data_ord <- data[ord, ord]
plotcorr(data_ord , col=my_colors[data_ord*50+50] , mar=c(1,1,1,1)  )
```
![](https://www.r-graph-gallery.com/97-correlation-ellipses_files/figure-html/thecode-1.png)

### Basic Scatterplot Matrix

This section explains how to build a [scatterplot matrix](https://www.r-graph-gallery.com/correlogram.html) with base R, without any packages. It provides several reproducible examples with explanation and R code


#### Scatterplot Matrix with the Native `plot()` Function

For a set of data variables (dimensions) $X_1, X_2, ... , X_k$, the scatter plot matrix shows all the pairwise [scatterplots](https://www.r-graph-gallery.com/scatterplot.html) of the variables on a single view with multiple scatterplots in a matrix format.

The native `plot()` function does the job pretty well as long as you just need to display scatterplots. For more option, check the [correlogram section](https://www.r-graph-gallery.com/correlogram.html).


```r
# Data: numeric variables of the native mtcars dataset
data <- mtcars[ , c(1,3:6)]
 
# Plot
plot(data , pch=20 , cex=1.5 , col="#69b3a2")
```

![](https://www.r-graph-gallery.com/98-basic-scatterplot-matrix_files/figure-html/thecode-1.png)

### Correlogram with the `car` Package
 
This section explains how to build a [scatterplot matrix](https://www.r-graph-gallery.com/correlogram.html) with the car package. It provides several reproducible examples with explanation and R code.


#### Scatterplot Matrix with the Native `plot()` Function

This is a scatterplot matrix built with the `scatterplotMatrix()` function of the [car](https://cran.r-project.org/web/packages/car/index.html) package.

See more correlogram examples in the [dedicated section](https://www.r-graph-gallery.com/correlogram.html).
 
Note the `|cyl` syntax: it means that categories available in the `cyl` variable must be represented distinctly (`color, shape, size..`).

```r
# Packages
library(car)
library(RColorBrewer) # for the color palette
# Let's use the car dataset natively available in R
data <- mtcars
# Make the plot
my_colors <- brewer.pal(nlevels(as.factor(data$cyl)), "Set2")
scatterplotMatrix(~mpg+disp+drat|cyl, data=data , 
      reg.line="" , smoother="", col=my_colors , 
      smoother.args=list(col="grey") , cex=1.5 , 
      pch=c(15,16,17) , 
      main="Scatter plot with Three Cylinder Options"
      )
```

![](https://www.r-graph-gallery.com/99-scatterplot-matrix-car-package_files/figure-html/thecode-1.png)
