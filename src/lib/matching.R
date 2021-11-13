match.decorate <- function(df,idvar,timevar){
  idvar <- ensym(idvar)
  timevar <- ensym(timevar)
  joinspec <- c(rlang::as_string(idvar),
                rlang::as_string(timevar))
  candidates <- 
    df %>%
    select(!!idvar,!!timevar) %>%
    mutate(dontuse.after=TRUE) %>%
    as.data.table()
  candidates2 <- candidates %>%
    mutate(!!timevar := !!timevar + 1,
           dontuse.before=TRUE) %>%
    select(-dontuse.after) %>%
    as.data.table()
  matches <- 
    full_join(candidates,candidates2,
              by=joinspec)%>%
    mutate(match.status = case_when(
      is.na(dontuse.before) ~ "no.prev",
      is.na(dontuse.after) ~ "no.next",
      TRUE ~ "matched"),
      match.id = row_number())%>%
    as.data.table()
  before <- 
    matches %>%
    filter(!is.na(dontuse.before)) %>%
    mutate(match.side = "before",
           !!timevar := !!timevar-1) %>%
    as.data.table()
  after <- 
    matches %>%
    filter(!is.na(dontuse.after)) %>%
    mutate(match.side = "after") %>%
    as.data.table()
  rbind(before,after,fill=TRUE) %>%
    select(-dontuse.before,-dontuse.after) %>%
    left_join(df,by=joinspec) %>%
    as.data.table()
}
