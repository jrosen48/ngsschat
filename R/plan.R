plan = drake_plan(
  orig_all = read_rds(file_in("data/all-ngsschat-tweets.rds")),
  users = read_csv(file_in("data/users-to-analyze.csv")),
  orig = read_rds(file_in("data/ngsschat-tweets-14-15.rds")),
  orig_pre = read_csv(file_in("data/orig-pre-14.csv")),
  orig_post = read_csv(file_in("data/orig-post-15.csv")),
  edge = read_csv(file_in("data/edgelist-to-analyze.csv")),
  state_data = read_excel(file_in("data/state-data.xlsx")),
  l = read_rds(file_in("data/geocoded-locations.rds")),
  coded_threads = read_csv(file_in("data/qual-coded-tweets.csv")),
  
  ts_plot = create_time_series(orig_all),
  states_proc = create_location_plot_and_return_users(l, state_data),
  sociogram_plot = create_sociogram(edge, users),
  descriptive_stats = create_descriptive_stats(users, orig, states),
  proc_users = proc_users_data_for_locations(users, l, states_proc),
  influence = prepare_for_influence(orig_pre, orig_post, proc_users, edge),
  proc_coded_threads = prepare_coded_threads(coded_threads, influence),
  
  thread_summary = rmarkdown::render(
    knitr_in("thread-summary.Rmd"),
    output_file = file_out("docs/thread-summary.html"),
    params = list(coded_threads = proc_coded_threads,
                  influence = influence),
  ),
  
  influence_to_model = prep_influence_for_modeling(proc_coded_threads, 
                                                    influence),
  
  influence_models = rmarkdown::render(
    knitr_in("influence-models.Rmd"),
    output_file = file_out("docs/influence-models.html"),
    params = list(influence = influence_to_model),
  ),
  
  render_images = rmarkdown::render(
    knitr_in("display-images.Rmd"),
    output_file = file_out("docs/display-images.html")
  ),
  
  index = rmarkdown::render(
    knitr_in("index.Rmd"),
    output_file = file_out("docs/index.html")
  ),
  
  drake_graph = rmarkdown::render(
    knitr_in("drake-graph.Rmd"),
    output_file = file_out("docs/drake-graph.html")
  )
  
)