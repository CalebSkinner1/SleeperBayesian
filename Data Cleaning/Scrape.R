# attempt to scrape Sleeper data

library("rvest")
library("janitor")

# sleeper point computation
sleeper_points <- function(player_df){
  player_df %>%
    rowwise() %>%
    mutate(
      dbldbl = if_else(sum(c_across(pts:blk)>9) > 1, 1, 0),
      trpdbl = if_else(sum(c_across(pts:blk)>9) > 2, 1, 0)) %>%
    ungroup() %>%
    mutate(
      forty = if_else(pts > 39, 1, 0),
      fifty = if_else(pts > 49, 1, 0),
      sleeper_points = .5*pts + trb + ast + 2*stl + 2*blk - tov + dbldbl + 2*trpdbl + .5*x3p + 2*forty + 2*fifty,
      sleeper_points = replace_na(sleeper_points, 0)) %>%
    select(-dbldbl, -trpdbl, -forty, -fifty)
}

# scrape function
scrape_function <- function(html){
  document <- read_html(html) %>%
    html_elements("body") %>%
    html_element("#wrap")
  
  player_name <- document %>%
    html_element("#info") %>%
    html_element("#meta") %>%
    html_elements("div") %>%
    pluck(2) %>%
    html_element("h1") %>%
    html_text2() %>%
    str_remove(" Game Log") %>%
    str_remove_all("[0-9]+") %>%
    str_remove(" -")
  
  df <- document %>%
    html_element("#content") %>%
    html_elements("div") %>%
    html_element("#all_pgl_basic") %>%
    pluck(26) %>%
    html_element("#div_pgl_basic") %>%
    html_element("#pgl_basic") %>%
    html_table() %>%
    clean_names() %>%
    select(rk, g, date, pts, trb, ast, stl, blk, tov, x3p) %>%
    filter(date != "Date") %>%
    relocate(date) %>%
    mutate(
      name = player_name,
      across(rk:x3p, ~as.numeric(.x)),
      date = as_date(date)) %>%
    relocate(name)
  
  return(df)
}
  

  



