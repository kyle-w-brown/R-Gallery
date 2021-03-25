# R-Gallery


  
Welcome to the R Gallery Book, a complete guide to the [R Graph Gallery](https://www.r-graph-gallery.com/) website. This book is taken directly from R Gallery with careful detail in reproducing plots and completing ideas. This is a collection of plots, graphs, diagrams, etc., using R programming language and was combined into one single reading collection of updated R gallery plots and graphs.  


## **Check out the book [here](https://kyle-w-brown.github.io/R-Gallery/)**

<br>

<p align="center"> 
<img src="images/rgallery.png" width="85%">
</p>


  
While this book was created to encapsulate the entire [R Graph Gallery](https://www.r-graph-gallery.com/) website into one readable source, another purpose is serving as introductory level into data science visualization using [R programming language](https://www.r-project.org/about.html).



## Note: The book is rather large and may take time to load completely

<br>


## Animation

An animated chart displays several chart states one after the other.

It must not be confounded with an [interactive chart](https://www.r-graph-gallery.com/interactive-charts.html) that allows interaction like zooming or hovering.
This section describes 2 methods to build animations with `R`.

The first method builds many png images and concatenate them in a gif using image magick. The second relies on the `gganimate` package
that automatically builds the animation for you.

Here is a great [interactive course](https://www.datacamp.com/courses/data-visualization-with-ggplot2-part-3?tap_a=5644-dce66f&tap_s=230804-f65650) that helps getting started with animations.

#### Build-Animation Directly with `gganimate`

The [gganimate library](https://github.com/thomasp85/gganimate) is a ggplot2 extension that allows to easily create animation from your data. Basically it allows to provide a frame (the step in the animation) as another aesthetic. Note that [this course](https://www.datacamp.com/courses/data-visualization-with-ggplot2-part-3?tap_a=5644-dce66f&tap_s=230804-f65650) is dedicated to it.


### Animated Bubble Chart with R and `gganimate`
 
The `gganimate` package allows to build `animated` chart using the `ggplot2` syntax directly from R. This section shows how to apply it on a [bubble](https://www.r-graph-gallery.com/bubble-chart.html) chart, to show an evolution in time.

### Animated Bubble Chart

Before trying to build an [animated](https://www.r-graph-gallery.com/animation.html) plot with `gganimate`, make sure you understood how to build a [basic bubble chart](https://www.r-graph-gallery.com/bubble-chart.html) with R and `ggplot2`.

The idea is to add an additional aesthetics called `transition_..()` that provides a frame variable. For each value of the variable, a step on the chart will be drawn. Here, `transition_time()` is used since the frame variable is numeric.

Note that the `gganimate` automatically performs a transition between state. Several options are available, set using the `ease_aes()` function.

```r
# Get data:
library(gapminder)
 
# Charge libraries:
library(ggplot2)
library(gganimate)
 
# Make a ggplot, but add frame=year: one image per year
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")
```
<center>
  ![](https://www.r-graph-gallery.com/img/graph/271-ggplot2-animated-gif-chart-with-gganimate1.gif){width=75%}
</center>

### Use Small Multiple

Since `gganimate` is a ggplot2 extension, any ggplot2 option can be used to customize the chart. Here, an example using `facet_wrap()` to use small multiple on the previous chart, spliting the chart window per continent.

<u>Important note</u>: this example comes from the [gganimate homepage](https://github.com/thomasp85/gganimate).


```r
# Get data:
library(gapminder)
 
# Charge libraries:
library(ggplot2)
library(gganimate)
 
# Make a ggplot, but add frame=year: one image per year
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate2.gif")
```

<center>
  ![](https://www.r-graph-gallery.com/img/graph/271-ggplot2-animated-gif-chart-with-gganimate2.gif){width=75%}
</center>

### Smooth Barplot Transition

Before trying to build an [animated](https://www.r-graph-gallery.com/animation.html) plot with `gganimate`, make sure you understood how to build a [basic bar chart](https://www.r-graph-gallery.com/barplot.html) with R and  `ggplot2`.

The idea is to add an additional aesthetics called `transition_..()` that provides a frame variable. For each value of the variable, a step on the chart will be drawn. Here, transition_states() is used since the frame variable is categorical.

Note that the `gganimate` automatically performs a transition between state. Several options are available, set using the `ease_aes()` function.

```r
# libraries:
library(ggplot2)
library(gganimate)
 
# Make 2 basic states and concatenate them:
a <- data.frame(group=c("A","B","C"), values=c(3,2,4), frame=rep('a',3))
b <- data.frame(group=c("A","B","C"), values=c(5,3,7), frame=rep('b',3))
data <- rbind(a,b)  
 
# Basic barplot:
ggplot(a, aes(x=group, y=values, fill=group)) + 
  geom_bar(stat='identity')
 
# Make a ggplot, but add frame=year: one image per year
ggplot(data, aes(x=group, y=values, fill=group)) + 
  geom_bar(stat='identity') +
  theme_bw() +
  # gganimate specific bits:
  transition_states(
    frame,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out')

# Save at gif:
anim_save("288-animated-barplot-transition.gif")
```

<center>
  ![](https://www.r-graph-gallery.com/img/graph/288-animated-barplot-transition.gif){width=75%}
</center>

### Progressive Line Chart Rendering

```r
# libraries:
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)

# Keep only 3 names
don <- babynames %>% 
  filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  filter(sex=="F")
  
# Plot
don %>%
  ggplot( aes(x=year, y=n, group=name, color=name)) +
    geom_line() +
    geom_point() +
    scale_color_viridis(discrete = TRUE) +
    ggtitle("Popularity of American names in the previous 30 years") +
    theme_ipsum() +
    ylab("Number of babies born") +
    transition_reveal(year)



# Save at gif:
anim_save("287-smooth-animation-with-tweenr.gif")
```

<center>
  ![](https://www.r-graph-gallery.com/img/graph/287-smooth-animation-with-tweenr.gif){width=75%}
</center>

### Concatenate `.png` Images with `Image Magick`

[Image Magick](https://www.imagemagick.org/script/index.php) is a software that allows to work with images in command lines. You can create and output a set of images doing a loop in `R`. Then, give all these images to Image magick and it will convert them into a `.gif` format.


### Most Basic Animation with R and Image Magick

 
This section describes how to build a basic count down `.gif` animation. It uses `R` to make 10 images, and Image Magick to concatenated them in a `.gif`.

This is probably the most basic animated plot (`.gif` format) you can do with R and Image Magick.

* Start by building 10 images with `R`.
* Use Image magick to concatenate them in a `gif`.

Of course, Image Magick must be installed on your computer. See [here](http://www.imagemagick.org/script/index.php) to install it.

<u>Note</u>: : This example has been found on Mark Heckmann's [R you ready website](https://ryouready.wordpress.com/2010/11/21/animate-gif-images-in-r-imagemagick/).

```r
# Build 10 images -> save them at .png format
png(file="example%02d.png", width=480, height=480)
par(bg="grey")
  for (i in c(10:1, "G0!")){
    plot.new()
    text(.5, .5, i, cex = 6 )
  }
dev.off()

# Use image magick
system("convert -delay 80 *.png animated_count_down.gif")

# Remove png files
file.remove(list.files(pattern=".png"))
```

<center>
  ![](https://www.r-graph-gallery.com/img/graph/166-basic-animated-graph-with-imagemagick.gif){width=75%}
</center>

### Animated 3d Chart with R

This section shows how to build a 3d [scatterplot](https://www.r-graph-gallery.com/scatterplot.html) and make it spin thanks to the `rgl` package. Reproducible code is provided.

The `rgl` package is the best option to build 3d charts in R. Please see this post for an introduction to 3d scatterplots using it.

It also provides the `plot3d()` and `play3d()` functions that allow to animate the 3d chart, and eventually to export the result at a .gif format. Here is an application to the famous iris dataset, with a nice animated 3d scatterplot chart.

```r
library( rgl )
library(magick)

# Let's use the iris dataset
# iris

# This is ugly
colors <- c("royalblue1", "darkcyan", "oldlace")
iris$color <- colors[ as.numeric( as.factor(iris$Species) ) ]

# Static chart
plot3d( iris[,1], iris[,2], iris[,3], col = iris$color, type = "s", radius = .2 )

# We can indicate the axis and the rotation velocity
play3d( spin3d( axis = c(0, 0, 1), rpm = 20), duration = 10 )

# Save like gif
movie3d(
  movie="3dAnimatedScatterplot", 
  spin3d( axis = c(0, 0, 1), rpm = 7),
  duration = 10, 
  dir = "~/Desktop",
  type = "gif", 
  
```

<center>
  ![](https://www.r-graph-gallery.com/img/graph/167-animated-3d-plot-imagemagick.gif){width=75%}
</center>


```r
# Library for 3D
library(lattice)

# Initiate data
b0 <- 10
b1 <- .5
b2 <- .3
int12 <- .2
g <- expand.grid(x = 1:20, y = 1:20)
g$z <- b0 + b1*g$x + b2*g$y + int12*g$x*g$y

# Make several .png images
png(file="example%03d.png", width=480, heigh=480)
for (i in seq(0, 350 ,10)){
    print(wireframe(z ~ x * y, data = g,
              screen = list(z = i, x = -60),
              drape=TRUE))
  }
dev.off()

# convert pdf to gif using ImageMagick
system("convert -delay 40 *.png animated_3D_plot.gif")

# cleaning up
file.remove(list.files(pattern=".png"))
```

<center>
  ![](https://www.r-graph-gallery.com/img/graph/167-animated-3d-plot-imagemagick.gif){width=75%}
</center>

# Distributions

## Density Plot

### Custom with `theme_ipsum`

The `hrbrthemes` package offer a set of pre-built themes for your charts. I am personnaly a big fan of the `theme_ipsum`: easy to use and makes your chart look more professional:

```r
# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)
# Make the histogram
data %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
    geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
    ggtitle("Night price distribution of Airbnb appartements") +
    theme_ipsum()
```

<p align="center"> 
<img src="https://www.r-graph-gallery.com/21-distribution-plot-using-ggplot2_files/figure-html/unnamed-chunk-3-1.png" width="65%">
</p>

Here is an example with [another dataset](https://www.data-to-viz.com/story/OneNumOneCatSeveralObs.html) where it works much better. Groups have very distinct distribution, it is easy to spot them even if on the same chart. Note that it is much better to add group name next to their distribution instead of having a legend beside the chart.

```r 
# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", header=TRUE, sep=",")
data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))
# A dataframe for annotations
annot <- data.frame(
  text = c("Almost No Chance", "About Even", "Probable", "Almost Certainly"),
  x = c(5, 53, 65, 79),
  y = c(0.15, 0.4, 0.06, 0.1)
)
# Plot
data %>%
  filter(text %in% c("Almost No Chance", "About Even", "Probable", "Almost Certainly")) %>%
  ggplot( aes(x=value, color=text, fill=text)) +
    geom_density(alpha=0.6) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    geom_text( data=annot, aes(x=x, y=y, label=text, color=text), hjust=0, size=4.5) +
    theme_ipsum() +
    theme(
      legend.position="none"
    ) +
    ylab("") +
    xlab("Assigned Probability (%)")
```

<p align="center"> 
<img src="https://www.r-graph-gallery.com/135-stacked-density-graph_files/figure-html/unnamed-chunk-3-1.png" width="65%">
</p>

<br>

## Histogram

### Histogram with Several Groups - `ggplot2`

A [histogram](https://www.data-to-viz.com/graph/histogram.html) displays the distribution of a numeric variable. A common task is to compare this distribution through several groups. This document explains how to do so using R and [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html).

#### Several Histograms on the Same Axis

If the number of group or variable you have is relatively low, you can display all of them on the same axis, using a bit of transparency to make sure you do not hide any data.

<u>Note</u>: with 2 groups, you can also build a [mirror histogram](https://www.r-graph-gallery.com/density_mirror_ggplot2.html)

```r
# library
library(ggplot2)
library(dplyr)
library(hrbrthemes)
# Build dataset with different distributions
data <- data.frame(
  type = c( rep("variable 1", 1000), rep("variable 2", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=4) )
)
# Represent it
p <- data %>%
  ggplot( aes(x=value, fill=type)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    theme_ipsum() +
    labs(fill="")
p
```

<p align="center"> 
<img src="https://www.r-graph-gallery.com/histogram_several_group_files/figure-html/unnamed-chunk-2-1.png" width="65%">
</p>

<br>

### Boxplot on top of Histogram

This example illustrates how to split the plotting window in base R thanks to the `layout function`. Contrary to the `par(mfrow=...)` solution, `layout()` allows greater control of panel parts.

Here a [boxplot](https://www.r-graph-gallery.com/boxplot.html) is added on top of the [histogram](https://www.r-graph-gallery.com/histogram.html), allowing to quickly observe summary statistics of the distribution.

```r
# Create data 
my_variable=c(rnorm(1000 , 0 , 2) , rnorm(1000 , 9 , 2))
 
# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
 
# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(-10,20), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="value of the variable", xlim=c(-10,20))
```

<p align="center"> 
<img src="https://www.r-graph-gallery.com/82-boxplot-on-top-of-histogram_files/figure-html/unnamed-chunk-1-1.png" width="65%">
</p>

<br>

## Violin Plot

### Method 1: the `forcats` library

The [Forecats library](https://github.com/tidyverse/forcats) is a library from the `tidyverse` especially made to handle factors in R. It provides a suite of useful tools that solve common problems with factors. `The fact_reorder()` function allows to reorder the factor. The `fact_reorder()` function allows to reorder the factor (`data$name` for example) following the value of another column (`data$val` here).

```{r, echo=TRUE, message=FALSE, warning=FALSE}
# load the library
library(forcats)
# Reorder following the value of another column:
data %>%
  mutate(name = fct_reorder(name, val)) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
```

<p align="center"> 
<img src="https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2_files/figure-html/unnamed-chunk-2-1.png" width="65%">
</p>

<br>
