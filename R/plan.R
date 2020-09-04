plan = drake_plan(
  
  # reading data
  
  raw_qual_coded_data = read_csv(file_in("data/raw-qual-coded-data.csv")),
  orig_all = read_rds(file_in("data/all-ngsschat-tweets.rds")),
  users = read_csv(file_in("data/users-to-analyze.csv")),
  orig = read_rds(file_in("data/ngsschat-tweets-14-15.rds")),
  orig_pre = read_csv(file_in("data/orig-pre-14.csv")),
  orig_post = read_csv(file_in("data/orig-post-15.csv")),
  edge = read_csv(file_in("data/edgelist-to-analyze.csv")),
  state_data = read_excel(file_in("data/state-data.xlsx")),
  locs = read_rds(file_in("data/geocoded-locations.rds")),
  coded_threads = read_csv(file_in("data/qual-coded-tweets.csv")),
  edgelist_of_coded_data = read_csv(file_in("data/edgelist.csv")),
  all_unfiltered_coded_threads = read_csv(file_in("data/all-unfiltered-coded-threads.csv"), col_types = cols(ID = col_character())),
  users_to_analyze = read_csv(file_in('data/users-to-analyze.csv')),
  
  # processing data
  
  fixed_coded_threads = fix_codes(coded_threads),
  ts_plot = create_time_series(orig_all),
  states_proc = create_location_plot_and_return_users(locs, state_data),
  sociogram_plot = create_sociogram(edge, users),
  descriptive_stats = create_descriptive_stats(users, orig, states),
  proc_users = proc_users_data_for_locations(users, locs, states_proc),
  influence = prepare_for_influence(orig_pre, orig_post, proc_users, edge),
  proc_coded_threads = prepare_coded_threads(fixed_coded_threads, influence),
  # influence_to_model = prep_influence_for_modeling(proc_coded_threads, influence),
  
  # RMD documents
  
  ## results
  
  preliminary_results = rmarkdown::render(
    knitr_in("descriptive-stats.Rmd"),
    output_file = file_out("docs/preliminary.html"),
    params = list(all_unfiltered_coded_threads = all_unfiltered_coded_threads,
                  users_to_analyze = users_to_analyze,
                  influence = influence)),

  conversations_rq1 = rmarkdown::render(
    knitr_in("interrater.Rmd"),
    output_file = file_out("docs/rq1.html"),
    params = list(raw_qual_coded_data = raw_qual_coded_data,
                  coded_threads = coded_threads)),
  
  participation_rq2 = rmarkdown::render(
    knitr_in("thread-summary.Rmd"),
    output_file = file_out("docs/rq2.html"),
    params = list(coded_threads = proc_coded_threads,
                  influence = influence)),
  
  sustained_involvement_rq3 = rmarkdown::render(
    knitr_in("influence-models.Rmd"),
    output_file = file_out("docs/rq3.html"),
    params = list(edgelist = edgelist_of_coded_data,
                  influence = influence)),
  
  ## other documents
  
  drake_graph = rmarkdown::render(
    knitr_in("drake-graph.Rmd"),
    output_file = file_out("docs/drake-graph.html")),
  
  rendered_site = render_site()
  
)