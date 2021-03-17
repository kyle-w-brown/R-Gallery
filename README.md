# R-Gallery


  
Welcome to the R Gallery Book, a complete guide to the [R Graph Gallery](https://www.r-graph-gallery.com/) website. This book is taken directly from R Gallery with careful detail in reproducing plots and completing ideas. This is a collection of plots, graphs, diagrams, etc., using R programming language and was combined into one single reading collection of updated R gallery plots and graphs.  


## **Check out the book [here](https://kyle-w-brown.github.io/R-Gallery/)**

<br>

<p align="center"> 
<img src="images/rgallery.png" width="65%">
</p>


  
While this book was created to encapsulate the entire [R Graph Gallery](https://www.r-graph-gallery.com/) website into one readable source, another purpose is serving as introductory level into data science visualization using [R programming language](https://www.r-project.org/about.html).



## Note: The book is rather large and may take time to load completely

<br>

# Density Plot

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
