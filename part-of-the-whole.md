# Part of a Whole 

---

```{r small-multiple-group-part-intro-image, echo=FALSE, fig.align='center', fig.cap="Barchart Small Multiple", out.width = '75%'}
knitr::include_graphics("https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2_files/figure-html/thecode5-1.png")
```


## Circular Packing

---

[Circular packing](https://www.data-to-viz.com/graph/circularpacking.html) or [circular treemap](https://www.data-to-viz.com/graph/circularpacking.html) allows to visualize a <u>hierarchic organization</u>. It is an equivalent of a [treemap](https://www.r-graph-gallery.com/treemap.html) or a [dendrogram](https://www.r-graph-gallery.com/dendrogram.html), where each node of the tree is represented as a circle and its sub-nodes are represented as circles inside of it.

#### One Level - `packcircles` and `ggplot2`

If your dataset has no hierarchy (it is basically just a few entities with attributed numeric values), the `packcircles` package is the best way to build a circular packing chart in R. The packages basically computes the position of each bubble, allowing to build the chart with `ggplot2`.

### Basic Circle Packing with One Level

This page aims to describe how to build a basic [circle packing](https://www.r-graph-gallery.com/circle-packing.html) chart with only one level of hierarchy. It uses the `packcircle` package for circle position, and `ggplot2` for drawing. This page aims to describe how to build a basic circle packing chart with only one level of hierarchy. Basically, you just represent each entity or individual of your dataset with a circle, its size depending on a provided value.

It is like a [barplot](https://www.r-graph-gallery.com/barplot.htmlv), but you use circle size instead of bar length. It is close to a [bubble plot](https://www.r-graph-gallery.com/bubble-chart.html), but X and Y positions do not mean anything. It is a circle version of a [treemap](https://www.r-graph-gallery.com/treemap.html).


 
Calculating the arrangement of dots is not a trivial problem. The `packcircles` library solves it and output coordinates of every points of the circle edges.

Finally, ggplot2 allows to draw shapes thanks to `geom_polygon()`.

```r
# Libraries
library(packcircles)
library(ggplot2)
 
# Create data
data <- data.frame(group=paste("Group", letters[1:20]), value=sample(seq(1,100),20)) 
 
# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(data$value, sizetype='area')
 
# We can add these packing information to the initial data frame
data <- cbind(data, packing)
 
# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
# plot(data$radius, data$value)
 
# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)
 
# Make the plot
ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  
  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y, size=value, label = group)) +
  scale_size_continuous(range = c(1,4)) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()
```
<center>
![](https://www.r-graph-gallery.com/305-basic-circle-packing-with-one-level_files/figure-html/thecode-1.png){width=75%}
</center>

### Circle Packing Customization with R
 
This page is dedicated to one level [circle packing](https://www.r-graph-gallery.com/circle-packing.html) customization with R. It notably shows how to use different color palettes and provides reproducible code snippets.

#### Using the `Viridis` Color Scale

This chart follows the previous most [basic circle packing](https://www.r-graph-gallery.com/305-basic-circle-packing-with-one-level) section.

It shows how to use the awesome `viridis` package to build color scales, a very good alternative to the usual `colorBrewer`.

Note that `magma` is used here, but you could use the same code with `inferno` or `viridis` instead.

```r
# libraries
library(packcircles)
library(ggplot2)
library(viridis)
 
# Create data
data <- data.frame(group=paste("Group", letters[1:20]), value=sample(seq(1,100),20)) 
 
# Generate the layout. sizetype can be area or radius, following your preference on what to be proportional to value.
packing <- circleProgressiveLayout(data$value, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)
 
# Basic color customization
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  scale_fill_manual(values = magma(nrow(data))) +
  geom_text(data = data, aes(x, y, size=value, label = group)) +
  scale_size_continuous(range = c(1,4)) +
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()
```
<center>
![](https://www.r-graph-gallery.com/306-custom-circle-packing-with-one-level_files/figure-html/thecode-1.png){width=75%}
</center>

### Map Color to Bubble Value

It is a common task to make the bubble color being lighter or darker according to its value.

This is possible by passing the focus variable to the dataframe that is read by `ggplot2`, and specifying it in tha `aes()`.

```r
# First I need to add the 'value' of each group to dat.gg.
# Here I repeat each value 51 times since I create my polygons with 50 lines
dat.gg$value <- rep(data$value, each=51)
# Plot
ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=value), colour = "black", alpha = 0.6) +
  scale_fill_distiller(palette = "BuPu", direction = 1 ) +
 
  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y, size=value, label = group)) +
  scale_size_continuous(range = c(1,4)) +
 
  # General theme:
  theme_void()  + 
  theme(legend.position="none") + 
  coord_equal()
```
<center>
![](https://www.r-graph-gallery.com/306-custom-circle-packing-with-one-level_files/figure-html/thecode2-1.png){width=75%}
</center>

### Background Customization

Change the background thanks to the `theme()` function and its `plot.background()` argument.

```r
ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=value), colour = "grey", alpha = 0.6, size=.5) +
  scale_fill_distiller(palette = "Spectral", direction = 1 ) +
  
  # Add text in the center of each bubble + control its size
  geom_label(data = data, aes(x, y, size=value, label = group)) +
  scale_size_continuous(range = c(1,4)) +
  
  # General theme:
  theme_void()  + 
  theme(
    legend.position="none",
    plot.background = element_rect(fill="black"),
    plot.title = element_text(color="white") 
  ) + 
  coord_equal() +
  ggtitle("A custom circle packing with\nblack background")
```
<center>
![](https://www.r-graph-gallery.com/306-custom-circle-packing-with-one-level_files/figure-html/thecode3-1.png){width=75%}
</center>

### Space between Bubbles
 

This chart is just a customization of the [chart #305](https://www.r-graph-gallery.com/305-basic-circle-packing-with-one-level.html) which describes the basic process to make a one level circle packing chart. I personally like to add a bit of space between each circle.

Basically, all you have to do is to reduce the `radius` size in your data once this one has been calculated. Just multiply it by a number under 0, and it will decrease the circle size.

If you have been so far, you probably want to check the [interactive version](https://www.r-graph-gallery.com/308-interactive-circle-packing.html) of the chart !

```r
# libraries
library(packcircles)
library(ggplot2)
library(viridis)
# Create data
data <- data.frame(group=paste("Group", letters[1:20]), value=sample(seq(1,100),20)) 
# Generate the layout
packing <- circleProgressiveLayout(data$value, sizetype='area')
packing$radius <- 0.95*packing$radius
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)
# Plot 
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, size=value, label = group), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal()
```
<center>
![](https://www.r-graph-gallery.com/307-add-space-in-circle-packing_files/figure-html/thecode-1.png){width=75%}
</center>

### Interactive Circle Packing with R
 
This section describes how to build an interactive [circle packing](https://www.r-graph-gallery.com/circle-packing.html) chart with R and the `ggiraph` package. It allows to hover bubbles to get additionnal information.

This chart follows sections [#305](https://www.r-graph-gallery.com/305-basic-circle-packing-with-one-level.html) and [#306](https://www.r-graph-gallery.com/306-custom-circle-packing-with-one-level.html) that explains how to build a static version of circle packing, and how to customize it.

This interactive version is very close to the static one. It uses the `ggiraph` library to transform the `ggplot2` code in something interactive. The steps are quite easy:

* First you need to prepare a column in the data frame with the text you want to display while hovering.
* Second, you need to change the geometries to use the interactive geometries of ggiraph.

Check the code below:

```r
# libraries
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)
# Create data
data <- data.frame(group=paste("Group_", sample(letters, 70, replace=T), #sample(letters, 70, replace=T), sample(letters, 70, replace=T), sep="" ), #value=sample(seq(1,70),70)) 
# Add a column with the text you want to display for each bubble:
data$text <- paste("name: ",data$group, "\n", "value:", data$value, "\n", "You can add a story here!")
# Generate the layout
packing <- circleProgressiveLayout(data$value, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)
# Make the plot with a few differences compared to the static version:
p <- ggplot() + 
  geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, #tooltip = data$text[id], data_id = id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, label = gsub("Group_", "", group)), #size=2, color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  coord_equal()
# Turn it interactive
widg <- ggiraph(ggobj = p, width_svg = 7, height_svg = 7)
widg
# save the widget
library(htmlwidgets)
saveWidget(widg, file=paste0( getwd(), "/HtmlWidget/circular_packing_interactive.html"))
```

<center>
```{r circular-packing-interactive, echo=FALSE}
htmltools::tags$iframe(title = "My embedded document", src = "partHtml/circular_packing_interactive.html", height="400px", width = "100%")
```
</center>


### Basic Circle Packing with Several Hierarchy Level

This page is dedicated to multi level circle packing. It explains how to build one using R and the ggraph package.

#### Several Levels - `ggraph`

If your dataset is a hierarchy, it is time to switch to other tools. For static versions, the `ggraph` package is the best option. It follows the grammar of graphic and makes it a breeze to customize the appearance following the same logic than `ggplot2`.

### Input & Concept
[Circular packing](https://www.r-graph-gallery.com/circular-packing.html) represents a hierarchy: The biggest circle (origin of the hierarchy) contains several big circles (nodes of level 1), which contain smaller circle (level 2) and so on.. The last level is called leaf.

The input data is a list of edges between nodes. It should look more or less like the table beside. Moreover, we usually accompany this table with another one that gives features for each node.

<center>
![*Edge List*](https://www.r-graph-gallery.com/img/other/Hierarchical_network_2col.png){width=25%}
</center>

#### Most Basic Circular Packing with `ggraph`

The `ggraph` package makes it a breeze to build a circular packing from an edge list. Here is an example based on the `flare` dataset proovded with the package.

The first step is to transform the dataframe to a graph object thanks to the `graph_from_data_frame()` function of the `igraph` package. Then, `ggraph` offers the `geom_node_circle()` function that will build the chart.

```r
# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
 
# We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
edges <- flare$edges
 
# Usually we associate another dataset that give information about each node of the dataset:
vertices <- flare$vertices
 
# Then we have to make a 'graph' object using the igraph library:
mygraph <- graph_from_data_frame( edges, vertices=vertices )
 
# Make the plot
ggraph(mygraph, layout = 'circlepack') + 
  geom_node_circle() +
  theme_void()
```

<center>
![](https://www.r-graph-gallery.com/313-basic-circle-packing-with-several-levels_files/figure-html/thecode2-1.png){width=75%}
</center>

### Switching to another Chart Type

Note that the `ggraph` library allows to easily go from one type of representation to another. Indeed several types of representation are suitable for hierarchical data: [dendrogram](https://www.r-graph-gallery.com/dendrogram.html) (can be circular), [treemap](https://www.r-graph-gallery.com/treemap.html), sunburst diagram or [network](https://www.r-graph-gallery.com/network.html)!

```r
library(ggraph)
ggraph(mygraph, layout='dendrogram', circular=TRUE) + 
  geom_edge_diagonal() +
  theme_void() +
  theme(legend.position="none")
```
<center>
![](https://www.r-graph-gallery.com/313-basic-circle-packing-with-several-levels_files/figure-html/thecode3a-1.png){width=75%}
</center>

```r
ggraph(mygraph, layout='dendrogram', circular=FALSE) + 
  geom_edge_diagonal() +
  theme_void() +
  theme(legend.position="none")
```
<center>
![](https://www.r-graph-gallery.com/313-basic-circle-packing-with-several-levels_files/figure-html/thecode3b-1.png){width=75%}
</center>

```r
ggraph(mygraph, 'treemap', weight = size) + 
  geom_node_tile(aes(fill = depth), size = 0.25) +
  theme_void() +
  theme(legend.position="none")
```
<center>
![](https://www.r-graph-gallery.com/313-basic-circle-packing-with-several-levels_files/figure-html/thecode3c-1.png){width=75%}
</center>

```r
ggraph(mygraph, 'partition', circular = TRUE) + 
  geom_node_arc_bar(aes(fill = depth), size = 0.25) +
  theme_void() +
  theme(legend.position="none")
```
<center>
![](https://www.r-graph-gallery.com/313-basic-circle-packing-with-several-levels_files/figure-html/thecode3d-1.png){width=75%}
</center>

```r
ggraph(mygraph) + 
  geom_edge_link() + 
  geom_node_point() +
  theme_void() +
  theme(legend.position="none")
```
<center>
![](https://www.r-graph-gallery.com/313-basic-circle-packing-with-several-levels_files/figure-html/thecode3e-1.png){width=75%}
</center>

### Customized Circle Packing with R and ggraph
 
This page follows the previous [introduction](https://www.r-graph-gallery.com/313-basic-circle-packing-with-several-levels.html) that explained the basis of circle packing with R and the ggraph library. It describes how to customize color, size, labels and more.

#### Bubble Size Proportionnal to a Variable

Mapping the bubble size to a numeric variable allows to add an additionnal layer of information to the chart.

Here, the `vertices` data frame has a `size` column that is used for the bubble size. Basically, it just needs to be passed to the `weight` argument of the `ggraph()` function.

```r
# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)
 
# We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
edges <- flare$edges
vertices <- flare$vertices
mygraph <- graph_from_data_frame(edges, vertices=vertices)
 
# Control the size of each circle: (use the size column of the vertices data frame)
ggraph(mygraph, layout = 'circlepack', weight=size) + 
  geom_node_circle() +
  theme_void()
```

<center>
![](https://www.r-graph-gallery.com/314-custom-circle-packing-with-several-levels_files/figure-html/thecode2-1.png){width=75%}
</center>

### Map Color to Hierarchy Depth

Adding color to circular packing definitely makes sense. The first option is to map color to depth: the origin of every node will have a color, the level 1 another one, and so on..

As usual, you can play with the colour palette to fit your needs. Here are 2 examples with the `viridis` and the `RColorBrewer` palettes:

```r
# Left: color depends of depth
p <- ggraph(mygraph, layout = 'circlepack', weight=size) + 
  geom_node_circle(aes(fill = depth)) +
  theme_void() + 
  theme(legend.position="FALSE")
p
```
<center>
![](https://www.r-graph-gallery.com/314-custom-circle-packing-with-several-levels_files/figure-html/thecode3a-1.png){width=75%}
</center>

```r
# Adjust color palette: viridis
p + scale_fill_viridis()
```
<center>
![](https://www.r-graph-gallery.com/314-custom-circle-packing-with-several-levels_files/figure-html/thecode3b-1.png){width=75%}
</center>

```r
# Adjust color palette: colorBrewer
p + scale_fill_distiller(palette = "RdPu") 
```

<center>
![](https://www.r-graph-gallery.com/314-custom-circle-packing-with-several-levels_files/figure-html/thecode3c-1.png){width=75%}
</center>

### Map Color to Hierarchy Depth

To add more insight to the plot, we often need to add labels to the circles. However you can do it only if the number of circle is not to big. Note that you can use `geom_node_text` (left) or `geom_node_label` to annotate leaves of the circle packing:

```r
# Create a subset of the dataset (I remove 1 level)
edges <- flare$edges %>% 
  filter(to %in% from) %>% 
  droplevels()
vertices <- flare$vertices %>% 
  filter(name %in% c(edges$from, edges$to)) %>% 
  droplevels()
vertices$size <- runif(nrow(vertices))
 
# Rebuild the graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )
```
<center>
![](https://www.r-graph-gallery.com/314-custom-circle-packing-with-several-levels_files/figure-html/thecode4a-1.png){width=75%}
</center>

```r
# left
ggraph(mygraph, layout = 'circlepack', weight=size ) + 
  geom_node_circle(aes(fill = depth)) +
  geom_node_text( aes(label=shortName, filter=leaf, fill=depth, size=size)) +
  theme_void() + 
  theme(legend.position="FALSE") + 
  scale_fill_viridis()
# Right 
ggraph(mygraph, layout = 'circlepack', weight=size ) + 
  geom_node_circle(aes(fill = depth)) +
  geom_node_label( aes(label=shortName, filter=leaf, size=size)) +
  theme_void() + 
  theme(legend.position="FALSE") + 
  scale_fill_viridis()
```
<center>
![](https://www.r-graph-gallery.com/314-custom-circle-packing-with-several-levels_files/figure-html/thecode4b-1.png){width=75%}
</center>


### Circular Packing with Hidden First Level of Hierarchy

This section shows how to build a [circular packing](https://www.r-graph-gallery.com/circular-packing.html) with R and the `ggraph` package, and how to remove the main bubble that packs all the others.


### Hiding the First Level for Better Styling

I personally do not like to display the big circle that surrounds the whole chart (level 0, origin). This circle does not provide any information, and the chart looks better without it in my opinion.

To get rid of it, just specify a color equal to the background color in the `scale_fill_manual()` and `scale_color_manual()` functions. Following the same idea, you can get rid of as many levels of hierarchy as you like.

```r
# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)
 
# We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
edges=flare$edges
vertices = flare$vertices
mygraph <- graph_from_data_frame( edges, vertices=vertices )
 
# Hide the first level (right)
ggraph(mygraph, layout = 'circlepack', weight="size") + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = viridis(4)[1], "2" = viridis(4)[2], "3" = viridis(4)[3], "4"=viridis(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "black", "2" = "black", "3" = "black", "4"="black") ) +
  theme_void() + 
  theme(legend.position="FALSE") 
```

<center>
![](https://www.r-graph-gallery.com/315-hide-first-level-in-circle-packing_files/figure-html/thecode3a-1.png){width=75%}
</center>

```r
# Second one: hide 2 first levels
ggraph(mygraph, layout = 'circlepack', weight="size") + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = "white", "2" = magma(4)[2], "3" = magma(4)[3], "4"=magma(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "white", "2" = "black", "3" = "black", "4"="black") ) +
  theme_void() + 
  theme(legend.position="FALSE")
```

<center>
![](https://www.r-graph-gallery.com/315-hide-first-level-in-circle-packing_files/figure-html/thecode3b-1.png){width=75%}
</center>


### Add Labels to a Specific Level of the Hierarchy

A related problem consists to add labels for one specific level of hierarchy only. For instance, if you want to display the names of group of level2, but not of level 3 to avoid cluttering the chart.

To solve this issue, the trickiest part is to determine the level of each node in the edge list data frame. Fortunately, the `data.tree` library is here to help with its `FromDataFrameNetwork()` function. It allows to isolate the level of each node, making it a breeze to select the labels to display.

```r
# Add the data.tree library
library(data.tree)
# Rebuild the data
edges <-flare$edges
vertices <- flare$vertices
# Transform it in a 'tree' format
tree <- FromDataFrameNetwork(edges)
# Then I can easily get the level of each node, and add it to the initial data frame:
mylevels <- data.frame( name=tree$Get('name'), level=tree$Get("level") )
vertices <- vertices %>% 
  left_join(., mylevels, by=c("name"="name"))
# Now we can add label for level1 and 2 only for example:
vertices <- vertices %>% 
  mutate(new_label=ifelse(level==2, shortName, NA))
mygraph <- graph_from_data_frame( edges, vertices=vertices )
# Make the graph
ggraph(mygraph, layout = 'circlepack', weight="size") + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = viridis(4)[1], "2" = viridis(4)[2], "3" = viridis(4)[3], "4"=viridis(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "black", "2" = "black", "3" = "black", "4"="black") ) +
  geom_node_label( aes(label=new_label), size=4) +
  theme_void() + 
  theme(legend.position="FALSE", plot.margin = unit(rep(0,4), "cm"))
```
<center>
![](https://www.r-graph-gallery.com/315-hide-first-level-in-circle-packing_files/figure-html/thecode2-1.png){width=75%}
</center>

### Zoomable Circle Packing with R and circlePacker
 
The circlePacker package allows to build interactive and zoomable circle packing charts. This section explains how to use the package with different kind of input datasets.

The `circlepackeR` package allows to build interactive [circle packing](https://www.r-graph-gallery.com/circular-packing.html). Click on a group, and a smooth zoom will reveal the subgroups behind it.

Circle packing is a visualization method for hierarchical data. This kind of data can be stored in 2 main ways:

* Nested data frame
* Edge list


<center>
![*Nested data frame*](https://www.r-graph-gallery.com/img/other/Nested_DataFrame.png){width=50%}
</center>

<center>
![*Edge list*](https://www.r-graph-gallery.com/img/other/Hierarchical_network_2col.png){width=25%}
</center>


#### Circular Packing fom Nested Data Frame

In a nested data frame, each line represents a leaf of the organization. Each column represents a level of the organization.

This data format will require the `data.tree` library to reformat the input dataset into something readable by `circlepackeR`.

```r
# Circlepacker package
library(circlepackeR)         
# devtools::install_github("jeromefroe/circlepackeR") # If needed
# create a nested data frame giving the info of a nested dataset:
data <- data.frame(
  root=rep("root", 15),
  group=c(rep("group A",5), rep("group B",5), rep("group C",5)), 
  subgroup= rep(letters[1:5], each=3),
  subsubgroup=rep(letters[1:3], 5),
  value=sample(seq(1:15), 15)
)
# Change the format. This use the data.tree library. This library needs a column that looks like root/group/subgroup/..., so I build it
library(data.tree)
data$pathString <- paste("world", data$group, data$subgroup, data$subsubgroup, sep = "/")
population <- as.Node(data)
# Make the plot
#circlepackeR(population, size = "value")
# You can custom the minimum and maximum value of the color range.
p <- circlepackeR(population, size = "value", color_min = "hsl(56,80%,80%)", color_max = "hsl(341,30%,40%)")
p
# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/circular_packing_circlepackeR2.html"))
```
<center>
```{r circular-packing-circlepacker, echo=FALSE}
htmltools::tags$iframe(title = "My embedded document", src = "partHtml/circular_packing_circlepackeR2.html", height="500px", width = "100%")
```
</center>

### Circular Packing fom Edge List

The edge list format has at least 2 columns. It describes all the edges of the data.

This format is widely spread. In this example, we just convert it to a nested data frame before plotting it as seen above.

```r
# Circlepacker package
library(circlepackeR)         
# devtools::install_github("jeromefroe/circlepackeR") # If needed
# Let's use the 'flare dataset' (stored in the ggraph library)
library(ggraph)
data_edge <- flare$edges
data_edge$from <- gsub(".*\\.","",data_edge$from)
data_edge$to <- gsub(".*\\.","",data_edge$to)
head(data_edge)   # This is an edge list
# We need to convert it to a nested data frame. the data.tree library is our best friend for that:
library(data.tree)
data_tree <- FromDataFrameNetwork(data_edge)
data_nested <- ToDataFrameTree(data_tree, 
                            level1 = function(x) x$path[2],
                            level2 = function(x) x$path[3],
                            level3 = function(x) x$path[4],
                            level4 = function(x) x$path[5])[-1,-1]
data_nested <- na.omit(data_nested)
# Now we can plot it as seen before!
data_nested$pathString <- paste("roots", data_nested$level1, data_nested$level2, data_nested$level3, data_nested$level4, sep = "/")
data_nested$value=1
data_Node <- as.Node(data_nested)
p <- circlepackeR(data_Node, size = "value")
p 
# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/circular_packing_circlepackeR1.html"))
```

<center>
```{r circular-packing-circlepacker-two, echo=FALSE}
htmltools::tags$iframe(title = "My embedded document", src = "partHtml/circular_packing_circlepackeR1.html", height="500px", width = "100%")
```
</center>






## Circular Stacked Barchart

---

A barchart can look pretty good using a circular layout, even if there are some [caveats](https://www.data-to-viz.com/caveat/circular_bar_yaxis.html) associated. If it interests you, visit the [circular barchart section](https://www.r-graph-gallery.com/circular-barplot.html).

### Circular Stacked Barplot
 
A [circular barplot](https://www.r-graph-gallery.com/circular-barplot.html) is a [barplot](https://www.r-graph-gallery.com/barplot.html) where bars are displayed along a circle instead of a line. This page aims to teach you how to make a grouped and stacked circular barplot with `R` and `ggplot2`.


A [circular barplot](https://www.r-graph-gallery.com/circular-barplot.html) is a [barplot]() where bars are displayed along a circle instead of a line. This page aims to teach you how to make a grouped and stacked circular barplot. I highly recommend to visit graph [#295](https://www.r-graph-gallery.com/295-basic-circular-barplot.html), [#296](https://www.r-graph-gallery.com/296-add-labels-to-circular-barplot.html) and [#297](https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html) Before diving into this code, which is a bit rough.

I tried to add as many comments as possible in the code, and thus hope that the method is understandable. If it is not, please comment and ask supplementary explanations.

You first need to understand how to make a [stacked barplot](https://www.r-graph-gallery.com/stacked-barplot.html) with ggplot2. Then understand how to properly add labels, calculating the good angles, flipping them if necessary, and adjusting their position. The trickiest part is probably the one allowing to add space between each group. All these steps are described one by one in the circular barchart [section](https://www.r-graph-gallery.com/circular-barplot.html).

```r
# library
library(tidyverse)
library(viridis)
 
# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value1=sample( seq(10,100), 60, replace=T),
  value2=sample( seq(10,100), 60, replace=T),
  value3=sample( seq(10,100), 60, replace=T)
)
 
# Transform data in a tidy format (long format)
data <- data %>% gather(key = "observation", value="value", -c(1,2)) 
 
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
 
# Get the name and the y position of each label
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
 
# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
 
# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]
 
# Make the plot
p <- ggplot(data) +      
  
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
  ggplot2::annotate("text", x = rep(max(data$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +
  
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
  geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
p
# Save at png
#ggsave(p, file="output.png", width=10, height=10)
```

<center>
![](https://www.r-graph-gallery.com/img/graph/299-circular-stacked-barplotBig.png){width=75%}
</center>

### Stacked Barplot for Evolution

Stacked area chart are sometimes used to study an evolution using each group on the X axis as a timestamp. There are many alternatives to that, like [streamgraph](https://www.r-graph-gallery.com/streamgraph.html) or [area chart](https://www.r-graph-gallery.com/area-chart.html):


### Base R

A stacked area chart showing the evolution of a few baby names in the US. <u>Zoom</u> on a specific time frame through <u>brushing</u>. Highlight a specific group by <u>hovering the legend</u>. Double click to unzoom

### Stacking Barplot

```{r stacked-barchart-part, echo=TRUE, fig.height=5, fig.width=5, message=FALSE, warning=FALSE}
# Libraries
library(tidyverse)
library(babynames)
library(streamgraph)
library(viridis)
library(hrbrthemes)
library(plotly)
# Load dataset from github
data <- babynames %>% 
  filter(name %in% c("Amanda", "Jessica",    "Patricia", "Deborah",   "Dorothy",  "Helen")) %>%
  filter(sex=="F")
# Plot
p <- data %>% 
  ggplot( aes(x=year, y=n, fill=name, text=name)) +
    geom_area( ) +
    scale_fill_viridis(discrete = TRUE) +
    theme(legend.position="none") +
    ggtitle("Popularity of American names in the previous 30 years") +
    theme_ipsum() +
    theme(legend.position="none")
ggplotly(p, tooltip="text")
```


### Three Periods Stacked Barplot

```{r stacked-barchart-three-periods, echo=TRUE, message=FALSE, warning=FALSE}
# create dummy data
don <- data.frame(
  x = rep(seq(2000,2005), 3),
  value = c(  75, 73, 68, 57, 36, 0, 15, 16, 17, 18, 19, 20, 10, 11, 15, 25, 45, 80),
  group = rep(c("A", "B", "C"), each=6)
)
#plot
don %>% 
  ggplot( aes(x=x, y=value, fill=group)) +
    geom_area( ) +
    scale_fill_viridis(discrete = TRUE) +
    theme(legend.position="none") +
    theme_ipsum() +
    theme(legend.position="none")
```

### Base R

A stacked area chart showing the evolution of a few baby names in the US. Zoom on a specific time frame through brushing. Highlight a specific group by hovering the legend. Double click to unzoom.

### Grouped, Stacked and Percent Stacked Barplot in Base R

This section explains how to build grouped, stacked and percent stacked barplot with base R. It provides a reproducible example with code for each type.

#### Grouped Barchart

 
A grouped barplot display a numeric value for a set of entities split in groups and subgroups. Before trying to build one, check how to make a [basic barplot](https://www.r-graph-gallery.com/208-basic-barplot.html) with `R` and `ggplot2`.

A few explanation about the code below:

* Input dataset must be a numeric matrix. Each group is a column. Each subgroup is a row.
* The `barplot()` function will recognize this format, and automatically perform the grouping for you.
* The `beside` allows to toggle between the grouped and the stacked barchart.

```r
# Create data
set.seed(112)
data <- matrix(sample(1:30,15) , nrow=3)
colnames(data) <- c("A","B","C","D","E")
rownames(data) <- c("var1","var2","var3")
 
# Grouped barplot
barplot(data, 
        col=colors()[c(23,89,12)] , 
        border="white", 
        font.axis=2, 
        beside=T, 
        legend=rownames(data), 
        xlab="group", 
        font.lab=2)
```

<center>
![](https://www.r-graph-gallery.com/211-basic-grouped-or-stacked-barplot_files/figure-html/thecode-1.png){width=75%}
</center>

### Grouped Stacked Barchart

A stacked barplot is very similar to the grouped barplot above. The subgroups are just displayed on top of each other, not beside. The stacked barchart is the default option of the `barplot()` function in base R, so you don't need to use the `beside` argument.

```r
# Create data
set.seed(112)
data <- matrix(sample(1:30,15) , nrow=3)
colnames(data) <- c("A","B","C","D","E")
rownames(data) <- c("var1","var2","var3")
# Get the stacked barplot
barplot(data, 
        col=colors()[c(23,89,12)] , 
        border="white", 
        space=0.04, 
        font.axis=2, 
        xlab="group")
```
<center>
![](https://www.r-graph-gallery.com/211-basic-grouped-or-stacked-barplot_files/figure-html/thecode2-1.png){width=75%}
</center>


### Percent Stacked Barplot

A percent stacked barchart displays the evolution of the proportion of each subgroup. The sum is always equal to 100%.

In base R, you have to manually compute the percentages, using the `apply()` function. This is more straightforward using [ggplot2](https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html).
 
Note that here, a custom color palette is used, thanks to the `RColorBrewer` package.

```r
# Create data
set.seed(1124)
data <- matrix(sample(1:30,15) , nrow=3)
colnames(data) <- c("A","B","C","D","E")
rownames(data) <- c("var1","var2","var3")
 
# create color palette:
library(RColorBrewer)
coul <- brewer.pal(3, "Pastel2") 
 
# Transform this data in %
data_percentage <- apply(data, 2, function(x){x*100/sum(x,na.rm=T)})
 
# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul , border="white", xlab="group")
```

<center>
![](https://www.r-graph-gallery.com/211-basic-grouped-or-stacked-barplot_files/figure-html/thecode3-1.png){width=75%}
</center>

### Barplot for `likert` Type Items
 
This section shows how to use the likert R package. It allows to build 0-centered stacked barplot to study likert type items.

[Likert](https://github.com/jbryer/likert) is an R package designed to help analyzing and visualizing Likert type items. It has been developped by Jason Bryer and Kim Speerschneider.

This barplot comes from the [demo page](https://github.com/jbryer/likert) and has been sent by [Carlos Ortega](https://www.linkedin.com/in/carlosortegafernandez).

It allows to analyse the reading attitudes from a panel of people. Each line represents a question. The barplot explains the feeling of people concerning this question.

```r
# library
library(likert) 
 
# Use a provided dataset
data(pisaitems) 
items28 <- pisaitems[, substr(names(pisaitems), 1, 5) == "ST24Q"] 
 
# Build plot
p <- likert(items28) 
plot(p)
```

<center>
![](https://www.r-graph-gallery.com/202-barplot-for-likert-type-items_files/figure-html/thecode-1.png){width=75%}
</center>

## Dendrogram

---

A [dendrogram](https://www.data-to-viz.com/graph/dendrogram.html) (or tree diagram) is a network structure. It is constituted of a root node that gives birth to several nodes connected by edges or branches. The last nodes of the hierarchy are called leaves. Many options are available to build one with R. This sections aims to lead you toward the best strategy for your data.

#### Two Types of Dendrogram

Dendrograms can be built from:

* [Hierarchical dataset](https://www.r-graph-gallery.com/dendrogram.html#hiera): Think about a CEO managing team leads managing employees and so on.
* [Clustering result](https://www.r-graph-gallery.com/dendrogram.html#clust): clustering divides a set of individuals in group according to their similarity. Its result can be visualized as a tree.


### Dendrogram fromn Hierarchical Data 

The `ggraph` package is the best option to build a dendrogram from hierarchical data with R. It is based on the grammar of graphic and thus follows the same logic that `ggplot2`.

### Dendrogram from Edge List

This section aims to describe how to make a basic [dendrogram](https://www.r-graph-gallery.com/dendrogram.html) representing hierarchical data with the ggraph library. Two input formats are considered:

* edge list - 2 columns, one row is on connection.
* nested data frame - one row is one path from root to leaf. As many columns as the number of levels in the hierarchy.

Please visit [this page](https://www.r-graph-gallery.com/335-custom-ggraph-dendrogram.html) to learn how to custom these dendrograms. If you want to create a dendrogram from clustering result, visit the [dendrogram section](https://www.r-graph-gallery.com/dendrogram.html) of the gallery.

Edge list is the most convenient format to use `ggraph`. Follow those steps:

* Transform the input dataframe to a graph object using the `graph_from_data_frame()` function from the `igraph` library.
* Use the dendrogram layout of `ggraph` with `layout = 'dendrogram'`.


<center>
![](https://www.r-graph-gallery.com/img/other/Hierarchical_network_2col.png){width=25%}
</center>

```r
# libraries
library(ggraph)
library(igraph)
library(tidyverse)
 
# create an edge list data frame giving the hierarchical structure of your individuals
d1 <- data.frame(from="origin", to=paste("group", seq(1,5), sep=""))
d2 <- data.frame(from=rep(d1$to, each=5), to=paste("subgroup", seq(1,25), sep="_"))
edges <- rbind(d1, d2)
 
# Create a graph object 
mygraph <- graph_from_data_frame( edges )
 
# Basic tree
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point() +
  theme_void()
```

<center>
![](https://www.r-graph-gallery.com/334-basic-dendrogram-with-ggraph_files/figure-html/thecode-1.png){width=75%}
</center>

### Dendrogram from a Nested Dataframe

Another common format is the nested data frame. The code below shows how to easily transform it into a nested data frame. Once it is done, just apply the code described above once more.

<center>
![](https://www.r-graph-gallery.com/img/other/Nested_DataFrame.png){width=50%}
</center>

```r
# libraries
library(ggraph)
library(igraph)
library(tidyverse)
 
# create a data frame 
data <- data.frame(
  level1="CEO",
  level2=c( rep("boss1",4), rep("boss2",4)),
  level3=paste0("mister_", letters[1:8])
)
 
# transform it to a edge list!
edges_level1_2 <- data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 <- data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edge_list=rbind(edges_level1_2, edges_level2_3)
 
# Now we can plot that
mygraph <- graph_from_data_frame( edge_list )
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point() +
  theme_void()
mygraph
```
<center>
![](https://www.r-graph-gallery.com/334-basic-dendrogram-with-ggraph_files/figure-html/thecode2-1.png){width=75%}
</center>

### Dendrogram Customization with R and `ggraph`
 
This section follows the previous [introduction](https://www.r-graph-gallery.com/334-basic-dendrogram-with-ggraph.html) to `ggraph` and dendrogram. It shows how to customize the dendrogram: layout, edge style, node features and more.

Start by creating a dataset and a graph object using the igraph package.

```r
# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
theme_set(theme_void())
 
# data: edge list
d1 <- data.frame(from="origin", to=paste("group", seq(1,7), sep=""))
d2 <- data.frame(from=rep(d1$to, each=7), to=paste("subgroup", seq(1,49), sep="_"))
edges <- rbind(d1, d2)
 
# We can add a second data frame with information for each node!
name <- unique(c(as.character(edges$from), as.character(edges$to)))
vertices <- data.frame(
  name=name,
  group=c( rep(NA,8) ,  rep( paste("group", seq(1,7), sep=""), each=7)),
  cluster=sample(letters[1:4], length(name), replace=T),
  value=sample(seq(10,30), length(name), replace=T)
)
 
# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices)
```

### Circular or Linear `layout`

First of all, you can use a linear or a circular representation using the circular option thanks to the `layout` argument of `ggraph`.

<u>Note</u>: a customized version of the circular dendrogram is available [here](https://www.r-graph-gallery.com/339-circular-dendrogram-with-ggraph.html), with more node features and labels.

```r
# Left
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal()
```

<center>
![](https://www.r-graph-gallery.com/335-custom-ggraph-dendrogram_files/figure-html/thecode-1.png){width=75%}
</center>
  
```r  
# Right
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal()
```

<center>
![](https://www.r-graph-gallery.com/335-custom-ggraph-dendrogram_files/figure-html/thecode1-1.png){width=75%}
</center>

### Edge Style

Then you can choose between different styles for your edges. The `ggraph` package comes with 2 main functions: `geom_edge_link()` and `geom_edge_diagonal()`.

Note that the most usual elbow representation is not implemented for hierarchical data yet.

```r
# Left
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_link()
```
<center>
![](https://www.r-graph-gallery.com/335-custom-ggraph-dendrogram_files/figure-html/thecode2-1.png){width=75%}
</center>


```r
# Right
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal()
```

<center>
![](https://www.r-graph-gallery.com/335-custom-ggraph-dendrogram_files/figure-html/thecode3-1.png){width=75%}
</center>

### Labels and Nodes

You probably want to add labels to give more insight to your tree. And eventually nodes. This can be done using the `geom_node_text` and `geom_node_point` respectively.

<u>Note</u>: the label addition is a bit more tricky for circular dendrogram, a solution is suggested in [graph #339](https://www.r-graph-gallery.com/339-circular-dendrogram-with-ggraph.html).

```r
# Left
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal() +
  geom_node_text(aes( label=name, filter=leaf) , angle=90 , hjust=1, nudge_y = -0.01) +
  ylim(-.4, NA)
```
<center>
![](https://www.r-graph-gallery.com/335-custom-ggraph-dendrogram_files/figure-html/thecode4-1.png){width=75%}
</center>

```r
# Right
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal() +
  geom_node_text(aes( label=name, filter=leaf) , angle=90 , hjust=1, nudge_y = -0.04) +
  geom_node_point(aes(filter=leaf) , alpha=0.6) +
  ylim(-.5, NA)
```

<center>
![](https://www.r-graph-gallery.com/335-custom-ggraph-dendrogram_files/figure-html/thecode5-1.png){width=75%}
</center>

### Customize Aesthetics

It is a common task to add color or shapes to your dendrogram. It allows to show more clearly the organization of the dataset.

`ggraph` works the same way as `ggplot2`. In the aesthetics part of each component, you can use a column of your initial data frame to be mapped to a shape, a color, a size or other..

```r
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal() +
  geom_node_text(aes( label=name, filter=leaf, color=group) , angle=90 , hjust=1, nudge_y=-0.1) +
  geom_node_point(aes(filter=leaf, size=value, color=group) , alpha=0.6) +
  ylim(-.6, NA) +
  theme(legend.position="none")
```
<center>
![](https://www.r-graph-gallery.com/335-custom-ggraph-dendrogram_files/figure-html/thecode8-1.png){width=75%}
</center>


### Circular Dendrogram with R and ggraph
 
This section shows how to build a customized circular [dendrogram](https://www.r-graph-gallery.com/dendrogram.html) using R and the `ggraph` package. It provides explanation and reproducible code.

The circular dendrogram of the `ggraph` library deserves its own page because it can be a bit tricky to adjust the labels. Indeed they need to have a good angle, be flipped upside down on the left part of the chart, and their alignment needs to be adjusted as well.

The following piece of code should allow you to display them correctly as shown beside. Note that the [graph #334](https://www.r-graph-gallery.com/334-basic-dendrogram-with-ggraph.html) gives the basis of dendrogram with ggraph, and that [graph #335](https://www.r-graph-gallery.com/335-custom-ggraph-dendrogram.html) presents a few possible customizations.

```r
# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer) 
# create a data frame giving the hierarchical structure of your individuals
d1=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2=data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
edges=rbind(d1, d2)
 
# create a vertices data.frame. One line per object of our hierarchy
vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) , 
  value = runif(111)
) 
# Let's add a column with the group of each name. It will be useful later to color points
vertices$group = edges$from[ match( vertices$name, edges$to ) ]
 
 
#Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the labels
vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves
 
# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)
 
# flip angle BY to make them readable
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
 
# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )
 
# Make the plot
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2.7, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,10) ) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
```

<center>
![](https://www.r-graph-gallery.com/339-circular-dendrogram-with-ggraph_files/figure-html/thecode8-1.png){width=75%}
</center>

### Interactive Dendrogram with R
 
This section describes how to use the `CollapsibleTree` package to build an interactive [tree diagram](https://www.r-graph-gallery.com/dendrogram.html). Explanation and reproducible code provided. 


The `collapsibletree` package is the best option to build interactive [dendrogram](https://www.r-graph-gallery.com/dendrogram.html) with R.

The input must be a data frame that stores the hierarchical information. Basically, each row describes a complete path from the root to the leaf. In this example, the `warpbreaks` dataset has 3 columns: `wool`, `tension` and `breaks`.

```r
# Load library
# install.packages("collapsibleTree")
library(collapsibleTree) 
 
# input data must be a nested data frame:
head(warpbreaks)
 
# Represent this tree:
p <- collapsibleTree( warpbreaks, c("wool", "tension", "breaks"))
p
# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/dendrogram_interactive.html"))
```

<center>
```{r interactive-treemap, echo=FALSE}
htmltools::tags$iframe(title = "My embedded document", src = "partHtml/dendrogram_interactive.html", height="400px", width = "100%")
```
</center>
  

### Dendrogram from `Clustering` Result

Hierarchical clustering is a common task in data science and can be performed with the `hclust()` function in R. The following examples will guide you through your process, showing how to prepare the data, how to run the clustering and how to build an appropriate chart to visualize its result.

#### Most Basic Dendrogram with R

* Input dataset is a `matrix` where each row is a sample, and each column is a variable. Keep in mind you can transpose a matrix using the `t()` function if needed.

* Clustering is performed on a square matrix (sample x sample) that provides the distance between samples. It can be computed using the `dist()` or the `cor()` function depending on the question your asking

* The `hclust()` function is used to perform the hierarchical clustering

* Its output can be visualized directly with the `plot()` function. See possible [customization](https://www.r-graph-gallery.com/31-custom-colors-in-dendrogram.html).

```r
# Dataset 
data <- matrix( sample(seq(1,2000),200), ncol = 10 )
rownames(data) <- paste0("sample_" , seq(1,20))
colnames(data) <- paste0("variable",seq(1,10))
# Euclidean distance
dist <- dist(data[ , c(4:8)] , diag=TRUE)
# Hierarchical Clustering with hclust
hc <- hclust(dist)
# Plot the result
plot(hc)
```

<center>
![](https://www.r-graph-gallery.com/29-basic-dendrogram_files/figure-html/thecode8-1.png){width=75%}
</center>

#### Hierarchical clustering principle:

* Take distances between objects.
* Seek the smallest distance between 2 objects.
* Aggregate the 2 objects in a cluster.
* Replace them with their barycenter. Again until having only one cluster containing every points.

There are several ways to calculate the distance between 2 clusters ( using the max between 2 points of the clusters, or the mean, or the min, or ward (default) ).

---

### Zoom on a Group

It is possible to zoom on a specific part of the tree. Select the group of interest using the `[[..]]` operator:

```r
# store the dedrogram in an object
dhc <- as.dendrogram(hc)
# set the margin
par(mar=c(4,4,2,2))
# Plot the Second group
plot(dhc[[2]] , main= "zoom on a part of the dendrogram")
```

<center>
![](https://www.r-graph-gallery.com/29-basic-dendrogram_files/figure-html/thecode-1.png){width=75%}
</center>

### Dendrogram with Color and Legend in R
 
This section describes how to apply a clustering method to a dataset and visualize the result as a [dendrogram](https://www.r-graph-gallery.com/dendrogram.html) with colors and legends. 

This is a upgrade of the basic [dendrogram](https://www.r-graph-gallery.com/dendrogram.html) presented in the [figure #29](https://www.r-graph-gallery.com/29-basic-dendrogram.html). Please refer to this previous section to understand how a dendrogram works.

In this exemple, we just show how to add specific colors to leaves and sample name. It allows to check if the expected groups are indeed found after clustering.

```r
# Build dataset (just copy and paste, this is NOT interesting)
sample <- paste(rep("sample_",24) , seq(1,24) , sep="")
specie <- c(rep("dicoccoides" , 8) , rep("dicoccum" , 8) , rep("durum" , 8))
treatment <- rep(c(rep("High",4 ) , rep("Low",4)),3)
data <- data.frame(sample,specie,treatment)
for (i in seq(1:5)){
  gene=sample(c(1:40) , 24 )
  data=cbind(data , gene)
  colnames(data)[ncol(data)]=paste("gene_",i,sep="")
 }
data[data$treatment=="High" , c(4:8)]=data[data$treatment=="High" , c(4:8)]+100
data[data$specie=="durum" , c(4:8)]=data[data$specie=="durum" , c(4:8)]-30
rownames(data) <- data[,1]    
# Have a look to the dataset
# head(data)
# Compute Euclidean distance between samples
dist=dist(data[ , c(4:8)] , diag=TRUE)
# Perfor clustering with hclust
hc <- hclust(dist)
dhc <- as.dendrogram(hc)
# Actually, each leaf of the tree has several attributes, like the color, the shape.. Have a look to it: 
specific_leaf <- dhc[[1]][[1]][[1]]
# specific_leaf
# attributes(specific_leaf)
#So if I Want to color each leaf of the Tree, I have to change the attribute of each leaf. This can be done using the dendrapply function. So I create a function that # # add 3 attributes to the leaf : one for the color (lab.col) ,one for the font lab.font and one for the size (lab.cex).
i=0
colLab<<-function(n){
    if(is.leaf(n)){
        
        #I take the current attributes
        a=attributes(n)
        
        #I deduce the line in the original data, and so the treatment and the specie.
        ligne=match(attributes(n)$label,data[,1])
        treatment=data[ligne,3];
            if(treatment=="Low"){col_treatment="blue"};if(treatment=="High"){col_treatment="red"}
        specie=data[ligne,2];
            if(specie=="dicoccoides"){col_specie="red"};if(specie=="dicoccum"){col_specie="Darkgreen"};if(specie=="durum"){col_specie="blue"}
        
        #Modification of leaf attribute
        attr(n,"nodePar")<-c(a$nodePar,list(cex=1.5,lab.cex=1,pch=20,col=col_treatment,lab.col=col_specie,lab.font=1,lab.cex=1))
        }
    return(n)
}
# Finally I just have to apply this to my dendrogram
dL <- dendrapply(dhc, colLab)
 
# And the plot
plot(dL , main="structure of the population")
legend("topright", 
     legend = c("High Nitrogen" , "Low Nitrogen" , "Durum" , "Dicoccoides" , "Dicoccum"), 
     col = c("red", "blue" , "blue" , "red" , "Darkgreen"), 
     pch = c(20,20,4,4,4), bty = "n",  pt.cex = 1.5, cex = 0.8 , 
     text.col = "black", horiz = FALSE, inset = c(0, 0.1))
```

<center>
![](https://www.r-graph-gallery.com/31-custom-colors-in-dendrogram_files/figure-html/thecode8-1.png){width=75%}
</center>

### More Customization with `Dendextend`

The `dendextend` package allows to go one step further in term of dendrogram customization. Here is a set of examples showing the main possibilities, like adding color bar on the bottom, drawing 2 trees face to face and more.

---

### Customized Dendrogram with R and the Dendextend Package

The `dendextend` package allows to apply all kinds of customization to a dendrogram: coloring nodes, labels, putting several tree face to face and more.

#### Basic Dendrogram

First of all, let's remind how to build a basic [dendrogram](https://www.r-graph-gallery.com/dendrogram.html) with `R`:

* Input dataset is a dataframe with individuals in row, and features in column.
* `dist()` is used to compute distance between sample.
* `hclust()` performs the hierarchical clustering.
* the `plot()` function can plot the output directly as a tree.

```r
# Library
library(tidyverse)
library(dendextend) 
# Data
head(mtcars)
# Clusterization using 3 variables
mtcars %>% 
  select(mpg, cyl, disp) %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() -> dend
 
# Plot
par(mar=c(7,3,1,1))  # Increase bottom margin to have the complete label
plot(dend)
```

<center>
![](https://www.r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend_files/figure-html/thecode-1.png){width=75%}
</center>

### The `set()` Function

The `set()` function of `dendextend` allows to modify the attribute of a specific part of the tree.

You can customize the `cex`, `lwd`, `col`, `lty` for branches and labels for example. You can also custom the nodes or the leaf. The code below illustrates this concept:

```r
# library
library(dendextend)
# Chart (left)
dend %>% 
  # Custom branches
  set("branches_col", "grey") %>% set("branches_lwd", 3) %>%
  # Custom labels
  set("labels_col", "orange") %>% set("labels_cex", 0.8) %>%
  plot()
```
<center>
![](https://www.r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend_files/figure-html/thecodeA-1.png){width=75%}
</center>

```r
# Middle
dend %>% 
  set("nodes_pch", 19)  %>% 
  set("nodes_cex", 0.7) %>% 
  set("nodes_col", "orange") %>% 
  plot()
```
<center>
![](https://www.r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend_files/figure-html/thecodeB-1.png){width=75%}
</center>

```r
# right
dend %>% 
  set("leaves_pch", 19)  %>% 
  set("leaves_cex", 0.7) %>% 
  set("leaves_col", "skyblue") %>% 
  plot()
```
<center>
![](https://www.r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend_files/figure-html/thecodeC-1.png){width=75%}
</center>

### Highlight Clusters

The `dendextend` library has some good functionalities to highlight the tree clusters.

You can color branches and label following their cluster attribution, specifying the number of cluster you want. The `rect.dendrogram()` function even allows to highlight one or several specific clusters with a rectangle.

```r
# Color in function of the cluster
par(mar=c(1,1,1,7))
dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  plot(horiz=TRUE, axes=FALSE)
abline(v = 350, lty = 2)
```
<center>
![](https://www.r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend_files/figure-html/thecode2A-1.png){width=75%}
</center>v


```r
# Highlight a cluster with rectangle
par(mar=c(9,1,1,1))
dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  plot(axes=FALSE)
rect.dendrogram( dend, k=3, lty = 5, lwd = 0, x=1, col=rgb(0.1, 0.2, 0.4, 0.1) ) 
```

<center>
![](https://www.r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend_files/figure-html/thecode2B-1.png){width=75%}
</center>

### Comparing with an Expected Clustering

It is a common task to compare the cluster you get with an expected distribution.

In the `mtcars` dataset we used to build our dendrogram, there is an `am` column that is a binary variable. We can check if this variable is consistent with the cluster we got using the `colored_bars()` function.



```r
# Create a vector of colors, darkgreen if am is 0, green if 1.
my_colors <- ifelse(mtcars$am==0, "forestgreen", "green")
 
# Make the dendrogram
par(mar=c(10,1,1,1))
dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  set("leaves_pch", 19)  %>% 
  set("nodes_cex", 0.7) %>% 
  plot(axes=FALSE)
 
# Add the colored bar
colored_bars(colors = my_colors, dend = dend, rowLabels = "am")
```
<center>
![](https://www.r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend_files/figure-html/thecode3-1.png){width=75%}
</center>v

### Comparing 2 Dendrograms with `tanglegram()`

It is possible to compare 2 dendrograms using the `tanglegram()` function.

Here it illustrates a very important concept: when you calculate your distance matrix and when you run your hierarchical clustering algorithm, you cannot simply use the default options without thinking about what you're doing. Have a look to the differences between 2 different methods of clusterisation.

```r
# Make 2 dendrograms, using 2 different clustering methods
d1 <- USArrests %>% dist() %>% hclust( method="average" ) %>% as.dendrogram()
d2 <- USArrests %>% dist() %>% hclust( method="complete" ) %>% as.dendrogram()
 
# Custom these kendo, and place them in a list
dl <- dendlist(
  d1 %>% 
    set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3),
  d2 %>% 
    set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3)
)
 
# Plot them together
tanglegram(dl, 
           common_subtrees_color_lines = FALSE, highlight_distinct_edges  = TRUE, highlight_branches_lwd=FALSE, 
           margin_inner=7,
           lwd=2
)
```
<center>
![](https://www.r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend_files/figure-html/thecode4-1.png){width=75%}
</center>

## Donut Chart

---

A donut or doughnut chart is a ring divided into sectors that each represent a proportion of the whole. It is very close from a [pie chart](https://www.r-graph-gallery.com/pie-plot.html) and thus suffers the same [problem](https://www.data-to-viz.com/caveat/pie.html). In R, it can be built in both ggplot2 and base R. There is no specific geom to build donut charts with ggplot2. 

### Most Basic Doughnut Chart with `ggplot2`

The `ggplot2` package allows to build [donut charts](https://www.r-graph-gallery.com/doughnut-plot.html). Note however that this is possible thanks a hack, since no specific function has been created for this kind of chart. (This is voluntary, to avoid donut charts that are dataviz [bad practice](https://www.data-to-viz.com/caveat/pie.html)).

Here is the process: 
* Input data provides a numeric variable for a set of entities. 
* Absolute numeric values must be translated to proportion. 
* Group positions must be stacked: we're gonna display them one after the other. 
* `geom_rect()` is used to plot each group as a rectangle. 
* `coord_polar()` is used to switch from stacked rectangles to a ring. 
* `xlim()` allows to switch from pie to donut: it adds the empty circle in the middle.

```r
# load library
library(ggplot2)
 
# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)
 
# Compute percentages
data$fraction = data$count / sum(data$count)
# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)
# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))
 
# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
     geom_rect() +
     coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
     xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
```
<center>
![](https://www.r-graph-gallery.com/128-ring-or-donut-plot_files/figure-html/thecode-1.png){width=75%}
</center>

### Customization

Here are a couple of things you can do improve your donut chart style:

* Use `theme_void()` to get rid of the unnecessary background, axis, labels and so on.
* Use a better color palette.
* Don't use a legend, add labels to groups directly.

```r
# load library
library(ggplot2)
# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)
 
# Compute percentages
data$fraction <- data$count / sum(data$count)
# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)
# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))
# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2
# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)
# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")
```

<center>
![](https://www.r-graph-gallery.com/128-ring-or-donut-plot_files/figure-html/thecode3-1.png){width=75%}
</center>

### Donut Thickness

It is important to understand that donut chart are just stacked rectangles that are made circular thanks to `coord_polar`.

Thus, the empty circle that makes it a donut chart is just the space between the initial Y axis and the left part of the rectangle.

* If `xlim` left boundary is big, no empty circle. You get a pie chart
* If `xlim` is low, the ring becomes thinner.

If you don't get it, just plot the chart without `coord_polar()`.

```r
# load library
library(ggplot2)
# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)
 
# Compute percentages
data$fraction <- data$count / sum(data$count)
# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)
# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))
# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2
# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)
# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")
```

<center>
![](https://www.r-graph-gallery.com/128-ring-or-donut-plot_files/figure-html/thecode2-1.png){width=75%}
</center>

### Donut Chart with Base R

It is also possible to build your donut chart without using any library. The example shows how, providing a reusable function that you can quickly apply to your input dataset.

If you want to stick to base R however, the function given below should allow you to get there.

To draw a [donut plot](https://www.r-graph-gallery.com/doughnut-plot.html), the easiest way is to use `ggplot2`, as suggested in [graph #128](https://www.r-graph-gallery.com/128-ring-or-donut-plot.html).

If you want to stick to base R however, the function given below should allow you to get there.

<u>Important</u>: this functions comes from [here](https://magesblog.com/).

```r
# The doughnut function permits to draw a donut plot
doughnut <-
function (x, labels = names(x), edges = 200, outer.radius = 0.8,
          inner.radius=0.6, clockwise = FALSE,
          init.angle = if (clockwise) 90 else 0, density = NULL,
          angle = 45, col = NULL, border = FALSE, lty = NULL,
          main = NULL, ...)
{
    if (!is.numeric(x) || any(is.na(x) | x < 0))
        stop("'x' values must be positive.")
    if (is.null(labels))
        labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L])
        xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col))
        col <- if (is.null(density))
          palette()
        else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise)
        -2 * pi
    else 2 * pi
    t2xy <- function(t, radius) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radius * cos(t2p),
             y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                  outer.radius)
        polygon(c(P$x, 0), c(P$y, 0), density = density[i],
                angle = angle[i], border = border[i],
                col = col[i], lty = lty[i])
        Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) {
            lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
            text(1.1 * Pout$x, 1.1 * Pout$y, labels[i],
                 xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0),
                 ...)
        }
        ## Add white disc          
        Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                  inner.radius)
        polygon(Pin$x, Pin$y, density = density[i],
                angle = angle[i], border = border[i],
                col = "white", lty = lty[i])
    }
    title(main = main, ...)
    invisible(NULL)
}
# Let's use the function, it works like PiePlot !
# inner.radius controls the width of the ring!
doughnut( c(3,5,9,12) , inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), rgb(0.2,0.9,0.4,0.4) , rgb(0.0,0.9,0.8,0.4)) )
```

<center>
![](https://www.r-graph-gallery.com/130-ring-or-donut-chart_files/figure-html/thecode-1.png){width=75%}
</center>



## Grouped and Stacked Barplot

---

Grouped and Stacked barplot display a numeric value for several entities, organised in groups and subgroups. It is probably better to have a solid understanding of the basic [barplot](https://www.r-graph-gallery.com/barplot.html) first.

#### Step by Step - `ggplot2`

ggplot2 is probably the best option to build grouped and stacked barchart. The input data frame requires to have 2 categorical variables that will be passed to the `x` and `fill` arguments of the `aes()` function. Toggling from grouped to stacked is pretty easy thanks to the `position` argument.

### Grouped, Stacked and Percent Stacked Barplot in `ggplot2`
 
This section explains how to build grouped, stacked and percent stacked barplot with `R` and `ggplot2`. It provides a reproducible example with code for each type.

#### Grouped Barchart

A grouped barplot display a numeric value for a set of entities split in groups and subgroups. Before trying to build one, check how to make a [basic barplot](https://www.r-graph-gallery.com/218-basic-barplots-with-ggplot2.html) with `R` and `ggplot2`.

A few explanation about the code below:

* Input dataset must provide 3 columns: the numeric value (`value`), and 2 categorical variables for the group (`specie`) and the subgroup (`condition`) levels.
* In the `aes()` call, x is the group (specie), and the subgroup (condition) is given to the `fill` argument.
* In the `geom_bar()` call, `position="dodge"` must be specified to have the bars one beside each other.

```r
# library
library(ggplot2)
 
# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)
 
# Grouped
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
    geom_bar(position="dodge", stat="identity")
```

<center>
![](https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2_files/figure-html/thecode-1.png){width=75%}
</center>

### Stacked Barchart

A stacked barplot is very similar to the grouped barplot above. The subgroups are just displayed on top of each other, not beside.

The only thing to change to get this figure is to switch the `position` argument to stack.

```r
# library
library(ggplot2)
 
# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)
 
# Stacked
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
    geom_bar(position="stack", stat="identity")
```

<center>
![](https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2_files/figure-html/thecode2-1.png){width=75%}
</center>

### Percent Stacked Barchart

Once more, there is not much to do to switch to a percent stacked barplot. Just switch to `position="fill"`. Now, the percentage of each subgroup is represented, allowing to study the evolution of their proportion in the whole.

```r
 # library
library(ggplot2)
 
# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)
 
# Stacked + percent
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
    geom_bar(position="fill", stat="identity")
```

<center>
![](https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2_files/figure-html/thecode3-1.png){width=75%}
</center>

### Grouped Barchart Customization

As usual, some customization are often necessary to make the chart look better and personnal. Let's:

* Add a `title`
* Use a `theme`
* Change color palette. See more [here](https://www.r-graph-gallery.com/ggplot2-color.html).
* Customize axis titles

```r
# library
library(ggplot2)
library(viridis)
library(hrbrthemes)
# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)
 
# Small multiple
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T) +
    ggtitle("Studying 4 species..") +
    theme_ipsum() +
    xlab("")
```

<center>
![](https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2_files/figure-html/thecode4-1.png){width=75%}
</center>

### Small Multiple

Small multiple can be used as an alternative of stacking or grouping. It is straightforward to make thanks to the `facet_wrap()` function.

```r
# library
library(ggplot2)
library(viridis)
library(hrbrthemes)
# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)
 
# Graph
ggplot(data, aes(fill=condition, y=value, x=condition)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_fill_viridis(discrete = T, option = "E") +
    ggtitle("Studying 4 species..") +
    facet_wrap(~specie) +
    theme_ipsum() +
    theme(legend.position="none") +
    xlab("")
```

<center>
![](https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2_files/figure-html/thecode5-1.png){width=75%}
</center>

## Piechart

---

A [piechart](https://www.data-to-viz.com/caveat/pie.html) is a circle divided into sectors that each represent a proportion of the whole. It is highly criticized in dataviz for meaningful reasons, [read more](https://www.data-to-viz.com/caveat/pie.html). This section teaches how to build one using R, using the `pie()` function or the ggplot2 package. The `pie()` function is natively provided in R. It allows to build nice piechart in seconds. Here is an overview of its functioning:

#### Step by Step - The `pie()` Function

The `pie()` function is natively provided in `R`. It allows to build nice piechart in seconds. Here is an overview of its functioning:

### Most Basic Piechart with `pie()`

`R` natively offers the `pie()` function that builds pie charts. The input is just a numeric variable, each value providing the value of a group of the piechart.

<u>Important note</u>: pie chart are widely known as a bad way to visualize information. Check [this section](https://www.data-to-viz.com/caveat/pie.html) for reasons and alternatives.

```r
# Create Data
Prop <- c(3,7,9,1,2)
 
# Make the default Pie Plot
pie(Prop)
```

<center>
![](https://www.r-graph-gallery.com/131-pie-plot-with-r_files/figure-html/thecode-1.png){width=75%}
</center>

### Change Labels with `labels`

Provide a vector of `labels` to the labels argument to add names to piechart groups:

```r
# You can also custom the labels:
pie(Prop , labels = c("Gr-A","Gr-B","Gr-C","Gr-D","Gr-E"))
```

<center>
![](https://www.r-graph-gallery.com/131-pie-plot-with-r_files/figure-html/thecode2-1.png){width=75%}
</center>

### Non-Circular Piechart with `edges`

Decrease the value of the `edges` argument to get angles around your piechart.

```r
# If you give a low value to the "edge" argument, you go from something circular to a shape with edges
pie(Prop , labels = c("Gr-A","Gr-B","Gr-C","Gr-D","Gr-E") , edges=10)
```

<center>
![](https://www.r-graph-gallery.com/131-pie-plot-with-r_files/figure-html/thecode3-1.png){width=75%}
</center>

### Add Stripes with `density`

The `density` arguments adds stripes. 

You can control the angle of those stripes with `angle`.

```r
# The density arguments adds stripes. You can control the angle of this lines with "angle"
pie(Prop , labels = c("Gr-A","Gr-B","Gr-C","Gr-D","Gr-E") , density=10 , angle=c(20,90,30,10,0))
```

<center>
![](https://www.r-graph-gallery.com/131-pie-plot-with-r_files/figure-html/thecode4-1.png){width=75%}
</center>

### Color with `col` and `border`

Change group color with `col`, and border color with `border`. 

Here, the `RcolorBrewer` package is used to build a nice color palette.

```r
# Prepare a color palette. Here with R color brewer:
library(RColorBrewer)
myPalette <- brewer.pal(5, "Set2") 
# You can change the border of each area with the classical parameters:
pie(Prop , labels = c("Gr-A","Gr-B","Gr-C","Gr-D","Gr-E"), border="white", col=myPalette)
```

<center>
![](https://www.r-graph-gallery.com/131-pie-plot-with-r_files/figure-html/thecode5-1.png){width=75%}
</center>

### Ggplot2 Piechart
 
A pie chart is a circle divided into sectors that each represent a proportion of the whole. This page explains how to build one with the ggplot2 package.

#### Step by Step - The `ggplot2` Package

There is no specific `geom` to build piechart with [ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html). The trick is to build a barplot and use `coord_polar` to make it circular. This is why the `pie()` function described above is probably a better alternative.

#### Most Basic Piechart

`ggplot2` does not offer any specific geom to build piecharts. The trick is the following:

* Input data frame has 2 columns: the group names (`group` here) and its value (`value` here).
* Build a stacked barchart with one bar only using the `geom_bar()` function.
* Make it circular with `coord_polar()`.

The result is far from optimal yet, keep reading for improvements.

```r
# Load ggplot2
library(ggplot2)
# Create Data
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)
# Basic piechart
ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)
```

<center>
![](https://www.r-graph-gallery.com/piechart-ggplot2_files/figure-html/thecode-1.png){width=75%}
</center>

### Improve Appearance

Previous version looks pretty bad. We need to:

* Remove useless numeric labels.
* Remove grid and grey background.

It's better now, just need to add labels directly on chart.

```r
# Load ggplot2
library(ggplot2)
# Create Data
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)
# Basic piechart
ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void() # remove background, grid, numeric labels
```

<center>
![](https://www.r-graph-gallery.com/piechart-ggplot2_files/figure-html/thecode2-1.png){width=75%}
</center>

### Adding Labels with `geom_text()`

The tricky part is to compute the y position of labels using this weird `coord_polar` transformation.

```r
# Load ggplot2
library(ggplot2)
library(dplyr)
# Create Data
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)
# Compute the position of labels
data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
# Basic piechart
ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = group), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")
```

<center>
![](https://www.r-graph-gallery.com/piechart-ggplot2_files/figure-html/thecode3-1.png){width=75%}
</center>




## Treemap

---

A [Treemap](https://www.data-to-viz.com/graph/treemap.html) displays <u>hierarchical</u> data as a set of nested rectangles. Each group is represented by a rectangle, which area is proportional to its value. Visit [data-to-viz.com](https://www.data-to-viz.com/graph/area.html) for more theoretical explanation about what it is. For a R implementation, see below.

#### Step by Step - The `treemap` Package

The `treemap` package is probably the best way to build treemaps in R. The 3 examples below will teach you how to build a very [basic treemap](https://www.r-graph-gallery.com/234-a-very-basic-treemap.html), how to deal with subgroups](https://www.r-graph-gallery.com/235-treemap-with-subgroups), and how to [customize](https://www.r-graph-gallery.com/236-custom-your-treemap) the figure. Note that once you master this package, you can very easily build an interactive version as described below.

### Most Basic Treemap with R
 
This section explains how to build a very basic [treemap](https://www.r-graph-gallery.com/treemap.html) with `R`. It uses the `treemap` package, provides reproducible code and explains how input data must be formatted.

#### Most Basic Treemap


This is the most basic [treemap](https://www.r-graph-gallery.com/treemap.html) you can do. The input dataset is simple: we just have 3 groups, and each has a value which we map to an area. 

It allows to learn the syntax of the `treemap` library: you need to provide at least a dataset (`data`), the column that provides groups (`index`), and the column that gives the size of each group (`vSize`).

See graph [#235](https://www.r-graph-gallery.com/235-treemap-with-subgroups.html) to learn how to add subgroups, and graph [#236](https://www.r-graph-gallery.com/236-custom-your-treemap.html) to customize the chart appearance.

```r
# library
library(treemap)
 
# Create data
group <- c("group-1","group-2","group-3")
value <- c(13,5,22)
data <- data.frame(group,value)
 
# treemap
treemap(data,
            index="group",
            vSize="value",
            type="index"
            )
```

<center>
![](https://www.r-graph-gallery.com/234-a-very-basic-treemap_files/figure-html/thecode-1.png){width=75%}
</center>

### Treemap with Subgroups

This section explains how to build a [treemap](https://www.r-graph-gallery.com/treemap.html) with subgroups in `R`. It uses the `treemap` package, provides reproducible code and explains how input data must be formatted.

This is a [treemap](https://www.r-graph-gallery.com/treemap.html) with several levels. We have 3 groups, each containing several subgroups. Each subgroup has a value which we map to an area.

In the `index` argument, you need to specify levels in the order of importance: group > subgroup > sub-subgroup.

<u>Note</u>: If you have one level of grouping only, see chart [#234](https://www.r-graph-gallery.com/234-a-very-basic-treemap.html).

<u>Note</u>: Showing more than 2 levels often result in a cluttered and unredable figure. Why not considering an [interactive version](https://www.r-graph-gallery.com/237-interactive-treemap.html)?

```r
# library
library(treemap)
 
# Build Dataset
group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,7,3,1,23)
data <- data.frame(group,subgroup,value)
 
# treemap
treemap(data,
            index=c("group","subgroup"),
            vSize="value",
            type="index"
            ) 
```

<center>
![](https://www.r-graph-gallery.com/235-treemap-with-subgroups_files/figure-html/thecode-1.png){width=75%}
</center>

### Customize your R Treemap

How to customize your [treemap](Customize your R treemap) built with `R`? Learn how to control borders, labels, and more. Several examples with reproducible code provided. 


#### Labels

This page aims to explain how to customize R treemaps. Make sure you already understood how to build a [basic treemap](https://www.r-graph-gallery.com/234-a-very-basic-treemap.html) with R.


The first step is to control label appearance. All the options are explained in the code below. Note that you can apply a different feature to each level of the treemap, for example using white for group labels, and orange for subgroup labels.

```r
# library
library(treemap)
 
# Create data
group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,7,3,1,23)
data <- data.frame(group,subgroup,value)
 
# Custom labels:
treemap(data, index=c("group","subgroup"),     vSize="value", type="index",
 
    fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
    fontcolor.labels=c("white","orange"),    # Color of labels
    fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
    bg.labels=c("transparent"),              # Background color of labels
    align.labels=list(
        c("center", "center"), 
        c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
    overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
    inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
 
)
```

<center>
![](https://www.r-graph-gallery.com/236-custom-your-treemap_files/figure-html/thecode-1.png){width=75%}
</center>

### Borders

You can control the border:

* Color with `border.col`
* Width with `border.lwds`
* Remember that you can still provide a vector to each option: it gives the value for groups, subgroups and so on.

```r
# Custom borders:
treemap(data, index=c("group","subgroup"), vSize="value", type="index",
 
    border.col=c("black","white"),             # Color of borders of groups, of subgroups, of subsubgroups ....
    border.lwds=c(7,2)                         # Width of colors
        
    )
```

<center>
![](https://www.r-graph-gallery.com/236-custom-your-treemap_files/figure-html/thecode2-1.png){width=75%}
</center>

### General Features

A few other arguments for more general customization. The `palette` arguments accepts any palette from `RColorBrewer`.

```r
# General features:
treemap(data, index=c("group","subgroup"), vSize="value", 
 
    type="index",                            # How you color the treemap. type help(treemap) for more info
    palette = "Set1",                        # Select your color palette from the RColorBrewer presets or make your own.
    title="My Treemap",                      # Customize your title
    fontsize.title=12,                       # Size of the title
 
) 
```

<center>
![](https://www.r-graph-gallery.com/236-custom-your-treemap_files/figure-html/thecode3-1.png){width=75%}
</center>


#### Interactivity with `d3treeR`

The `d3treeR` allows to build interactive treemaps with `R`. Click on a group to zoom in and reveal subgroups. Click on the group name on top to unzoom and come back to the previous state. Note that the syntax used in previous charts above is exactly the same. Only one more line of code is needed, give it a go!

### Interactive Treemap in R
 
With a big amount of data, a [treemap](https://www.r-graph-gallery.com/treemap.html) can get cluttered and unreadable. Interactivity allows to keep a clean an insightful figure. This section shows how to build an interactive treemap with `R` and the `d3treeR` package.

This section follows the previous chart [#234](https://www.r-graph-gallery.com/234-a-very-basic-treemap.html), [#235](https://www.r-graph-gallery.com/235-treemap-with-subgroups.html) and [#236](https://www.r-graph-gallery.com/236-custom-your-treemap.html) that describe how to build and customize [treemaps](https://www.r-graph-gallery.com/treemap.html) with the `treemap` package.

The idea is to turn the chart interactive: you can now click on a group to zoom in and show its subgroups. Click on the group name on top to unzoom and come back to the previous state.

This is done thanks to the `d3treeR` package:

```r
# library
library(treemap)
library(d3treeR)
 
# dataset
group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,7,3,1,23)
data <- data.frame(group,subgroup,value)
 
# basic treemap
p <- treemap(data,
            index=c("group","subgroup"),
            vSize="value",
            type="index",
            palette = "Set2",
            bg.labels=c("white"),
            align.labels=list(
              c("center", "center"), 
              c("right", "bottom")
            )  
          )            
 
# make it interactive ("rootname" becomes the title of the plot):
inter <- d3tree2( p ,  rootname = "General" )
# save the widget
# library(htmlwidgets)
# saveWidget(inter, file=paste0( getwd(), "/HtmlWidget/interactiveTreemap.html"))
```

<center>
```{r, echo=FALSE}
htmltools::tags$iframe(title = "My embedded document", src = "partHtml/interactiveTreemap.html", height="500px", width = "100%")
```
</center>
