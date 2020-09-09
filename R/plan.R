plan = drake::drake_plan(
  
  # reading data
  
  orig_all = process_raw_tweets(file_in("data-raw/original-storify-data.csv")),
  
  orig = create_orig(orig_all),
  orig_post = create_orig_post(orig_all), 
  
  professional_role_codes = read_csv(file_in("data-raw/ngsschat-code-profiles.csv")),
  
  users = create_users(orig, professional_role_codes), # should have one additional user, etnyresci; I can't figure out how or why they were removed, except that they're in the RDS but not CSV file
  locations = geocode_locs(users),
  
  data_for_irr = read_csv(file_in("data/raw-qual-coded-data.csv")),
  
  edge = read_csv(file_in("data/edgelist-to-analyze.csv")),
  
  state_data = read_excel(file_in("data/state-data.xlsx")),
  
  dd = read_csv(file_in('data-raw/completed-coding-spreadsheet.csv'),
                col_types = cols(
                  ID = col_character()
                )), # is this duplicating other data?
  
  # WORK HERE
  all_unfiltered_coded_threads = process_conversation_thread_codes(orig_all), # 11,128 rows - 1 extra: 583802332648968192; shspjones 
  qual_coded_threads = process_qual_codes(all_unfiltered_coded_threads, users, dd), # should be 11,056
  fixed_coded_threads = fix_codes(qual_coded_threads), # should be 11,056
  proc_coded_threads = prepare_coded_threads(fixed_coded_threads, influence), # should be 7,422
  
  # all_unfiltered_coded_threads = read_csv(file_in("data/all-unfiltered-coded-threads.csv"), col_types = cols(ID = col_character())), # all_unfiltered_coded_threads - this seems to have one fewer row
  
  # codes_of_conversation_threads = read_csv(file_in("data/qual-coded-tweets.csv")),
  # fixed_coded_threads = fix_codes(codes_of_conversation_threads),
  
  proc_users = process_users(users, locations), # for desc, 1, and 3; should be 247 rows
  edgelist_of_coded_data = read_csv(file_in("data/edgelist.csv")), # how is this created? for RQ 2 and 3; 4994 rows
  
  # RMD documents
  
  ## results
  
  # ts_plot = create_time_series(orig_all), # move to descriptive
  # states_proc = create_location_plot_and_return_users(locs, state_data), # move to descriptives
  
  preliminary_results = rmarkdown::render(
    knitr_in("preliminary-results.Rmd"),
    output_file = file_out("docs/preliminary-results.html"),
    params = list(all_unfiltered_coded_threads = qual_coded_threads,
                  users_to_analyze = users)),
  
  conversations_rq1 = rmarkdown::render(
    knitr_in("conversations-rq1.Rmd"),
    output_file = file_out("docs/rq1.html"),
    params = list(raw_qual_coded_data = data_for_irr,
                  coded_threads = fixed_coded_threads,
                  all_unfiltered_coded_threads = all_unfiltered_coded_threads,
                  users_to_analyze = users)),
  
  participation_rq2 = rmarkdown::render(
    knitr_in("balanced-participation-rq2.Rmd"),
    output_file = file_out("docs/rq2.html"),
    params = list(edgelist = edgelist_of_coded_data,
                  users_to_analyze = users)),
  
  sustained_involvement_rq3 = rmarkdown::render(
    knitr_in("sustained-involvement-rq3.Rmd"),
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

