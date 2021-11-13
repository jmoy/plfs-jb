coarse.category.lvls <- c("slf-emp","csl-emp","sal-emp",
                          "unemp","nopart","sck-emp","nwrk")
category.status <- function(status.code){
  factor(case_when(
    status.code %in% c("11","12","21") ~ "slf-emp",
    status.code == "31" ~ "sal-emp",
    status.code %in% c("41","42","51") ~ "csl-emp",
    status.code %in% c("61","71") ~ "sck-emp",
    status.code %in% c("62","72") ~ "nwrk",
    status.code %in% c("81","82") ~"unemp",
    status.code %in% c("91","92","93","94","95","97","98","99")~"nopart"
  ),
  levels = coarse.category.lvls)
}

category.status.coarse <- function(status.code){
  case_when(
    status.code %in% c("81","82") ~"unemp",
    status.code %in% c("91","92","93","94","95","97","98","99")~"nopart",
    TRUE ~ "work"
  )
}

yq.to.t <- function(year,quarter){
  qno <- as.numeric(str_sub(quarter,2,2))
  if_else(year!=3,qno,qno+8)
}

t.to.yq <- function(t) {
  t <- t+1 # PLFS starts in July
  y <- t%/%4+2017
  q <- t %% 4 + 1
  paste0(y,"-Q",q)
}

is.working <- function(status.code) {
  as.numeric(status.code) < 81
}