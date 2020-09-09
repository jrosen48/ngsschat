source("R/packages.R")  # loads packages
source("R/functions.R") # defines the create_plot() function
source("R/plan.R")      # creates the drake plan

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

vis_drake_graph(plan, file = "graph.html", hover = TRUE, font_size = 18)

## _drake.R must end with a call to drake_config().
## The arguments to drake_config() are basically the same as those to make().
## lock_envir allows functions that alter the random seed to be used. The biggest
## culprits of this seem to be interactive graphics e.g. plotly and mapdeck.
drake_config(plan,
             lock_envir = FALSE)
