plan = drake::drake_plan(
  
  # reading data
  
  orig_all = process_raw_tweets(file_in("data-raw/original-storify-data.csv")),
  
  orig = create_orig(orig_all),
  orig_post = create_orig_post(orig_all), 
  
  professional_role_codes = read_csv(file_in("data-raw/ngsschat-code-profiles.csv")),
  
  users = create_users(orig, professional_role_codes), # should have one additional user, etnyresci; I can't figure out how or why they were removed, except that they're in the RDS but not CSV file
  locs = geocode_locs(users),
  
  data_for_irr = read_csv(file_in("data/raw-qual-coded-data.csv")),
  
  edge = read_csv(file_in("data/edgelist-to-analyze.csv")),
  
  state_data = read_excel(file_in("data/state-data.xlsx")),
  
  codes_of_conversation_threads = read_csv(file_in("data/qual-coded-tweets.csv")),
  all_unfiltered_coded_threads = read_csv(file_in("data/all-unfiltered-coded-threads.csv"), col_types = cols(ID = col_character())), # can this be made from coded threads?
  fixed_coded_threads = fix_codes(codes_of_conversation_threads),
  proc_coded_threads = prepare_coded_threads(fixed_coded_threads, influence),
  edgelist_of_coded_data = read_csv(file_in("data/edgelist.csv")), # is this made from edge; or how is this made?
  
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
    params = list(raw_qual_coded_data = data_for_irr,
                  coded_threads = fixed_coded_threads,
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
