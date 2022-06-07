# Import required R packages.

library(tidyr)
library(tidyverse)
# Define paths to data sets. If you don't keep your data in the same directory as the code, adapt the path names.

sp500_data <-
  download.file(
    'https://github.com/gedeck/practical-statistics-for-data-scientists/blob/master/data/sp500_data.csv.gz?raw=true',
    destfile = 'sp500.csv.gz'
  )
sp500_px <- read_csv('sp500.csv.gz')
sp500_px <- sp500_px %>% filter(X1 > '2011-01-01')

## Principal Components Analysis
### A simple example
oil_px <- sp500_px[, c('CVX', 'XOM')]
pca <- princomp(oil_px)
oil_px_pca <- pca$scores
loadings <- pca$loadings

graph <- ggplot(data = oil_px, aes(x = CVX, y = XOM)) +
  geom_point(alpha = .3) +
  scale_shape_manual(values = c(46)) +
  stat_ellipse(type = 'norm',
               level = .99,
               color = 'grey25') +
  geom_abline(
    intercept = 0,
    slope = loadings[2, 1] / loadings[1, 1],
    color = 'grey25',
    linetype = 2
  ) +
  geom_abline(
    intercept = 0,
    slope = loadings[2, 2] / loadings[1, 2],
    color = 'grey25',
    linetype = 2
  ) +
  scale_x_continuous(expand = c(0, 0), lim = c(-3, 3)) +
  scale_y_continuous(expand = c(0, 0), lim = c(-3, 3)) +
  theme_bw()
graph

### Interpreting principal components
syms <-
  c(
    'AAPL',
    'MSFT',
    'CSCO',
    'INTC',
    'CVX',
    'XOM',
    'SLB',
    'COP',
    'JPM',
    'WFC',
    'USB',
    'AXP',
    'WMT',
    'TGT',
    'HD',
    'COST'
  )
top_sp <- sp500_px %>% dplyr::select(all_of(syms))
sp_pca <- princomp(top_sp)
par(mar = c(6, 3, 0, 0) + .1, las = 2)
screeplot(sp_pca, main = '')

loadings <- sp_pca$loadings[, 1:5]
loadings <- as.data.frame(loadings)
loadings$Symbol <- row.names(loadings)
loadings <- gather(loadings, 'Component', 'Weight', -Symbol)

loadings$Color = loadings$Weight > 0
graph <-
  ggplot(loadings, aes(x = Symbol, y = Weight, fill = Color)) +
  geom_bar(stat = 'identity',
           position = 'identity',
           width = .75) +
  facet_grid(Component ~ ., scales = 'free_y') +
  guides(fill = FALSE)  +
  ylab('Component Loading') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
graph


# how much variance is explained?
(sp_pca$sdev) ^ 2
cumsum((sp_pca$sdev ^ 2)) / sum((sp_pca$sdev ^ 2))

# scatter plot of z1 and z2 
results <- data.frame(sp_pca$scores)
results$labels <- sp500_px$X1
ggplot(results, aes(x = Comp.1, y = Comp.2, label = labels)) +
  geom_point() + geom_text(hjust = 0, vjust = 0)

# https://en.wikipedia.org/wiki/Black_Monday_(2011)
# https://finance.yahoo.com/news/stock-market-news-december-01-133851538.html?fr=sycsrp_catchall

## t-sne
library(Rtsne)
library(Ecdat)
train <- na.omit(Fair)
train$occupation <- as.factor(train$occupation)
colors <- rainbow(length(unique(train$occupation)))
names(colors) <- unique(train$occupation)
train_ds <- train[,-c(1, 4, 7)]
tsne <-
  Rtsne(
    train_ds,
    dims = 2,
    perplexity = 30,
    verbose = T,
    max_iter = 1500,
    check_duplicates = F
  )

results <- data.frame(tsne$Y)
results$occupation <- train$occupation
ggplot(data = results, aes(
  x = X1,
  y = X2,
  label = occupation,
  color = occupation
)) + geom_point(size = 0) + geom_text(hjust = 0, vjust = 0.1)

## MDS
### Example 1 - from rbloggers - http://www.r-bloggers.com/multidimensional-scaling-mds-with-r/

#get data
dist.au <-
  read.csv("http://rosetta.reltech.org/TC/v15/Mapping/data/dist-Aus.csv")
dist.au
city.names <-
  c(
    "Adelaide",
    "Alice Springs",
    "Brisbane",
    "Darwin",
    "Hobart",
    "Melbourne",
    "Perth",
    "Sydney"
  )
#tidy up
row.names(dist.au) <- dist.au[, 1]
dist.au <- dist.au[, -1]
dist.au   #note - this is already a dissimilarity matrix

# if your data are not in a dissimilarity matrix, you need to make one with dist()
# Multidimensional Scaling (MDS) with function cmdscale()
fit <- cmdscale(dist.au, eig = TRUE)
fit
x <- fit$points[, 1]
y <- fit$points[, 2]
### Plotting with ggplot2
results <- data.frame(x, y)
results$city.names = city.names
ggplot(results, aes(x, y, label = city.names)) +
  geom_point() +
  geom_text(hjust = -0.15)
