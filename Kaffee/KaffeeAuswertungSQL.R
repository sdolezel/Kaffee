## Check for libraries and install if needed
if (!require(RODBC)) {
  install.packages("RODBC")
  library(RODBC)
}

if (!require(grid)) {
  install.packages("grid")
  library(grid)
}

if (!require(gridExtra)) {
  install.packages("gridExtra")
  library(gridExtra)
}


## Get data from DB
dbhandle <-
  odbcDriverConnect('driver={SQL Server};server=localhost;database=Coffee;trusted_connection=true')
df <- sqlQuery(dbhandle, 'select * from Key2operateAdvencoInternal.dbo.Feedback_Report')
odbcClose(dbhandle)

## Clean data
df <- na.omit(df)
colnames(df) <- c("time", "question", "value", "id", "type", "name")
drops <- c("question", "type")
df <- df[, !(names(df) %in% drops)]
rm(drops)

df <-
  lapply(df, function(x) {
    gsub("Tchibo Caffe Crema Mil", "Tchibo Caffe Crema Mild", x)
  })
df <-
  lapply(df, function(x) {
    gsub("Tchibo Caffe Crema Mildd", "Tchibo Caffe Crema Mild", x)
  })

df$id <- paste(df$id, df$name, sep = "_")

df <- transform(df, id = as.numeric(factor(df$id)))
df <- transform(df, value = as.numeric(factor(df$value)))

## Analyse data

# lower and upper bound
lb <- min(df$id)
ub <- max(df$id)

# Create stats
stats <-
  data.frame(
    Id = integer(),
    Name = factor(),
    Min = integer(),
    Max = integer(),
    Mean = numeric(),
    Var = numeric(),
    SD = numeric(),
    Median = numeric(),
    Modus = numeric(),
    N = numeric()
  )

# Calculate stats
for (dsid in lb:ub) {
  stats <-
    rbind(
      stats,
      data.frame(
        Id = dsid,
        Name = unique(df$name[df$id == dsid]),
        Min = min(df$value[df$id == dsid]),
        Max = max(df$value[df$id == dsid]),
        Mean = mean(df$value[df$id == dsid]),
        Var = var(df$value[df$id == dsid]),
        SD = sqrt(var(df$value[df$id == dsid])),
        Median = median(df$value[df$id == dsid]),
        Modus = as.numeric(names(sort(
          -table(df$value[df$id == dsid])))[1]),
        N = sum(df$id == dsid)
      )
    )
}

# Print stats table
png("img/Stats.png",
    height = 600,
    width = 800)
g <- tableGrob(stats,
               theme = ttheme_default(
                 colhead = list(fg_params = list(col = "navyblue", 
                                                 fontface = 4L)),
                 rowhead = list(fg_params = list(col = "white", 
                                                 fontface = 3L))
               ))
g$heights <- unit(rep(1 / nrow(g), nrow(g)), "npc")
grid.draw(g)
dev.off()

# Print histograms
png("img/Hist%03d.png")
for (dsid in lb:ub) {
  foo <-
    hist(
      x = df$value[df$id == dsid] + 0.001,
      main = paste("Histogram of" , unique(df$name[df$id == dsid])),
      breaks = seq(1, 6),
      freq = TRUE,
      col = "lightblue",
      labels = FALSE,
      panel.first = grid(),
      xlab = "Bewertung",
      xaxt = "n",
      ylab = "Anzahl"
    )
  box()
  axis(side = 1,
       at = foo$mids,
       labels = seq(1, 5))
}
dev.off()

# Print boxplots
png("img/BoxPlot.png", 
    height = 800,
    width = 1200)

# Change margins
par(mai = c(0, 0, 0, 0),
      mar = c(3, 20, 1, 1) + 0.1)


boxplot(df$value  ~ df$id, 
        yaxt = "n", 
        horizontal = TRUE)

for (dsid in lb:ub) {
  # Add Means
  points(
    x = stats$Mean[dsid],
    dsid,
    pch = 16,
    col = "red",
    lwd = 2
  )
  # Add error bars
  segments(stats$Mean[dsid] - stats$SD[dsid],
           dsid ,
           stats$Mean[dsid] + stats$SD[dsid],
           dsid,
           col = "red")
  segments(stats$Mean[dsid] - stats$SD[dsid],
           dsid - 0.1,
           stats$Mean[dsid] - stats$SD[dsid],
           dsid + 0.1,
           col = "red")
  segments(stats$Mean[dsid] + stats$SD[dsid],
           dsid - 0.1,
           stats$Mean[dsid] + stats$SD[dsid],
           dsid + 0.1,
           col = "red")
  # Add Modus
  points(
    x = stats$Modus[dsid],
    dsid,
    pch = 3,
    col = "green",
    lwd = 2
  )
  # Add axis
  axis(
    side = 2,
    las = 2,
    at = dsid,
    labels = unique(df$name[df$id == dsid]),
    cex.axis = 1
  )
}

# Close png device
dev.off()

rm(df,stats,dsid,foo,g,lb,ub)
