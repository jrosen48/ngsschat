plan = drake::drake_plan(
  
  # reading data
  
  orig_all = process_raw_tweets("data-raw/original-storify-data.csv"),
  
  orig = create_orig(orig_all),
  orig_post = create_orig_post(orig_all),
  
  raw_qual_coded_data = read_csv(file_in("data/raw-qual-coded-data.csv")),
  
  edge = read_csv(file_in("data/edgelist-to-analyze.csv")),
  state_data = read_excel(file_in("data/state-data.xlsx")),
  locs = read_rds(file_in("data/geocoded-locations.rds")), # what makes this?
  coded_threads = read_csv(file_in("data/qual-coded-tweets.csv")),
  
  edgelist_of_coded_data = read_csv(file_in("data/edgelist.csv")), # is this made from edge; or how is this made?
  
  all_unfiltered_coded_threads = read_csv(file_in("data/all-unfiltered-coded-threads.csv"), col_types = cols(ID = col_character())), # can this be made from coded threads?
  users = read_csv(file_in('data/users-to-analyze.csv')),
  
  # processing data
  
  fixed_coded_threads = fix_codes(coded_threads),
  # influence = prepare_for_influence(orig_post, proc_users, edge), # I don't think this is used; can rep with users
  proc_coded_threads = prepare_coded_threads(fixed_coded_threads, influence),
  
  # sociogram_plot = create_sociogram(edge, users),
  # descriptive_stats = create_descriptive_stats(users, orig, states),
  # proc_users = proc_users_data_for_locations(users, locs, states_proc), # can this be used for influence? is it needed

  
  # RMD documents
  
  ## results
  
  ts_plot = create_time_series(orig_all), # move to descriptive
  states_proc = create_location_plot_and_return_users(locs, state_data), # move to descriptives
  
  preliminary_results = rmarkdown::render(
    knitr_in("descriptive-stats.Rmd"),
    output_file = file_out("docs/preliminary.html"),
    params = list(all_unfiltered_coded_threads = all_unfiltered_coded_threads,
                  users_to_analyze = users,
                  influence = influence)),
  
  conversations_rq1 = rmarkdown::render(
    knitr_in("new-thread-summary.Rmd"),
    output_file = file_out("docs/rq1.html"),
    params = list(raw_qual_coded_data = raw_qual_coded_data,
                  coded_threads = coded_threads,
                  all_unfiltered_coded_threads = all_unfiltered_coded_threads,
                  users_to_analyze = users,
                  influence = influence)),
  
  participation_rq2 = rmarkdown::render(
    knitr_in("selection-models.Rmd"),
    output_file = file_out("docs/rq2.html"),
    params = list(edgelist = edgelist_of_coded_data,
                  users_to_analyze = users)),
  
  sustained_involvement_rq3 = rmarkdown::render(
    knitr_in("influence-models.Rmd"),
    output_file = file_out("docs/rq3.html"),
    params = list(edgelist = edgelist_of_coded_data,
                  influence = influence)),
  
  ## other documents
  
  drake_graph = rmarkdown::render(
    knitr_in("dependencies.Rmd"),
    output_file = file_out("docs/dependencies.html")),
  
  rendered_site = target(command = render_site(),
                         trigger = trigger(condition = TRUE))
  
)