format_transmat <- function(transmat,title,label,probabilities){
  df <- 
    transmat %>%
    select(sex.x,status.cws.nrw.x,status.cws.nrw.y,wts) %>%
    group_by(sex.x,status.cws.nrw.x) %>%
    mutate(ps=2*wts/sum(wts)*100) %>%
    ungroup() %>%
    pivot_longer(c(wts,ps),names_to="var",values_to="val") %>%
    #filter(!(var=="ps" & status.cws.nrw.x=="ALL")) %>%
    mutate(var=factor(var,levels=c("wts","ps")),
           val = sprintf(if_else(var=="wts","%.2f","(%.2f)"),val)) %>%
    pivot_wider(names_from=status.cws.nrw.y,values_from=val,
                names_sort=TRUE) %>%
    arrange(sex.x,status.cws.nrw.x,var) %>%
    mutate(status.cws.nrw.x=if_else(var=="ps","",as.character(status.cws.nrw.x))) %>%
    filter(probabilities | var!="ps") %>%
    select(-var) %>%
    rename(`Emp.\\ status [t]`=status.cws.nrw.x)
  gps <- 
    df %>%
    mutate(rno=row_number())%>%
    group_by(sex.x) %>%
    summarize(brow=min(rno),erow=max(rno),.groups="drop")
  df <- select(df,-sex.x)
  tbl <- 
    df %>%
    kbl(booktabs=TRUE,digits=2,centering=TRUE,
        escape=FALSE,
        linesep=c("","\\addlinespace"),
        align=c("l",rep("r",ncol(df)-1)),
        caption=title,
        label=label)
  for (i in 1:nrow(gps)){
    tbl <- pack_rows(tbl,gps$sex.x[[i]],gps$brow[[i]],gps$erow[[i]],
                     bold=FALSE,italic=TRUE)
  }
  if (probabilities) {
    tbl <- row_spec(tbl,seq(2,nrow(df),by=2),color="mytxtclr2") 
  }
  tbl %>%
    add_header_above(c("","Emp.\\ status [t+1]"=ncol(df)-1),escape=FALSE) %>%
    add_header_above(c("","percentage"=ncol(df)-1)) %>%
    kable_styling(latex_options="scale_down",font_size = 10)
}
