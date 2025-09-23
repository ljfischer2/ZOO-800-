

# random example I found on how to plot in 3D
# not really helpful, but super fun to look at and play with


# Library
install.packages("plotly")
library(plotly)

# Data: volcano is provided by plotly

# Plot
p <- plot_ly(z = volcano, type = "surface")
p 


