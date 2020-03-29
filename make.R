source("R/packages.R")  # loads packages
source("R/functions.R") # defines the create_plot() function
source("R/plan.R")      # creates the drake plan

options(drake_force_interactive = TRUE)

drake::make(plan)

drake::vis_drake_graph(plan)

# drake::clean()
