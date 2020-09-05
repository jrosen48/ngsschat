source("R/packages.R")  # loads packages
source("R/functions.R") # defines the create_plot() function
source("R/plan.R")      # creates the drake plan

vis_drake_graph(plan, file = "graph.html", hover = TRUE, font_size = 18)

drake_config(plan, 
             verbose = 2,
             lock_envir = FALSE)

# drake::clean()