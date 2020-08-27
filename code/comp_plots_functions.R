##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: Region Comparison Tab plotting functions
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(scales)

set.seed(20200301)
#STATE_COLS <- sample(c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"))
#STATE_COLS <- c(hcl(0,100, seq(20,100, length.out=26)), hcl(240,100, seq(100,20, length.out=26)))
#STATE_COLS <- c("#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#names(STATE_COLS) <- c("National", sort(c(state.name, "District of Columbia")))

#assuming you've loaded the JHU_COVID_DATA...
# eventually add argument to show deltas or better yet, persons per 10K (population)
create_comp_df <- function(region.list, selected.x, selected.y, y.scale=NULL, overlay.npis, link.cases.plot=FALSE, gantt.chart=TRUE, time.shift=NULL){
  
  if (selected.x=="all.dates"){
    x_var_filter_value <- as.Date("2020-01-22")
  }else{
    x_var_filter_value <- -14 #two weeks leading up to 0
  }
  
  ret.df <- rbindlist(list(STATE_INF_STATS, COUNTRY_INF_STATS, MBSA_INF_STATS, COUNTY_INF_STATS), fill = TRUE) %>%
    .[, state.region.key := paste(mState.Providence, Country.Region, sep="___")] %>%
    .[state.region.key %chin% region.list] %>%
    .[date >= as.Date("2020-01-22")] %>%
    merge(x=., y=WORLD_POP, all.x=TRUE, by = c("mState.Providence", "Country.Region")) %>%
    set_comp_date_values(df = ., selected.x = selected.x, time.shift) %>%
    set_comp_y_values(df = ., selected.y=selected.y, y.scale = y.scale, region.list=region.list) %>%
    .[x_var >= x_var_filter_value] %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  if (link.cases.plot){
    other.selected.y <- switch(selected.y, "Cases"="Deaths", "Deaths"="Cases")
    ret.df2 <- rbindlist(list(STATE_INF_STATS, COUNTRY_INF_STATS, MBSA_INF_STATS, COUNTY_INF_STATS), fill = TRUE) %>%
      .[, state.region.key := paste(mState.Providence, Country.Region, sep="___")] %>%
      .[state.region.key %chin% region.list] %>%
      .[date >= as.Date("2020-01-22")] %>%
      merge(x=., y=WORLD_POP, all.x=TRUE, by = c("mState.Providence", "Country.Region")) %>%
      set_comp_date_values(df = ., selected.x = selected.x, time.shift) %>%
      set_comp_y_values(df = ., selected.y=other.selected.y, y.scale = y.scale, region.list=region.list) %>%
      .[x_var >= x_var_filter_value] %>%
      as.data.frame(stringsAsFactors = FALSE)
  }else{
    ret.df2 <- data.frame()
  }
  
  # this returns a list of strings to feed the plotly graphs
  ret.plt.options <- pull_comp_plot_opts(region.list=region.list, selected.x=selected.x, selected.y=selected.y, y.scale=y.scale,
                                         overlay.npis=overlay.npis, link.cases.plot=link.cases.plot, plot.data = ret.df, plot.data2 = ret.df2)
  
  if (gantt.chart & length(region.list)>0){
    gantt.data <- generate_gantt_data(region.list, plot.data = ret.df, selected.x=selected.x)
  }else{
    gantt.data <- list()
  }
  
  ret.obj <- list("plt.dat"=ret.df,
                  "subplt.dat"=ret.df2,
                  "plt.options"=ret.plt.options,
                  "gantt.data"=gantt.data)
  
  return(ret.obj)
  
}

draw_refactored_comparison_plot <- function(plt.obj, link.cases.plot=FALSE, gantt.chart=TRUE, hovered.region=NULL){
  
  plt.dat <- plt.obj[["plt.dat"]]
  subplt.dat <- plt.obj[["subplt.dat"]]
  plt.options <- plt.obj[["plt.options"]]
  gantt.data <- plt.obj[["gantt.data"]] #this is a list fyi
  
  n.states <- nlevels(factor(plt.dat$mState.Providence))
  
  if (n.states>3){
    STATE_COLS <- colorRampPalette(c("#F0F0F0", "#BDBDBD"))(n.states)
    #STATE_COLS <- colorRampPalette(c("#D9D9D9", "#737373"))(n.states) orig greys, too dark w/ dark highlight
    names(STATE_COLS) <- levels(factor(plt.dat$mState.Providence))
    if (!is.null(hovered.region)){
      STATE_COLS[which(str_detect(string=names(STATE_COLS), pattern=hovered.region))] <- "#0d2f4f" 
      # this is too bright: "#03f4fc"
      # this is too dark: "#005B94"
      # this is very dark but might work with lighter greys: #0d2f4f
      
      legend.order <- sort(unique(plt.dat$mState.Providence))
      legend.order <- c(hovered.region, legend.order[legend.order!=hovered.region])
      
      #ordering traces
      plt.dat <- plt.dat %>%
        mutate(mState.Providence = factor(mState.Providence, levels = rev(legend.order)))
      
      if (link.cases.plot){
        subplt.dat <- subplt.dat %>%
          mutate(mState.Providence = factor(mState.Providence, levels = rev(legend.order)))
      }
      
    }
  }else{
    STATE_COLS <- rev(brewer.pal(8, "Blues")[c(4,6,8)]) [1:n.states]
    names(STATE_COLS) <- levels(factor(plt.dat$mState.Providence))
  }
  
  main.plt <- plot_ly(data = plt.dat, type = "scatter", mode = "lines", colors = STATE_COLS, legendgroup=~mState.Providence, 
                      showlegend=TRUE, customdata = ~mState.Providence, source = plt.options[['source.id']]) %>%
    add_trace(x=~x_var, y = ~y_val, color = ~mState.Providence, mode = "lines", line = list(width=plt.options[["line.width"]]), type = 'scatter',
              hovertext = ~trace.hovertext, hoverinfo = "text") %>%
    layout(font = list(family = "Arial"),
           title=list(text=plt.options[['plot.title']], x = 0, xref="paper"),
           xaxis=list(title=plt.options[['xaxis.title']], range = c(plt.options[['xmin']], plt.options[['xmax']])), 
           margin=list(t=42,b=57, pad=3),
           yaxis=list(title=plt.options[['y.axis.title']],
                      range=c(plt.options[['ymin']], plt.options[['ymax']]),
                      type = plt.options[['y.plot.scale']],
                      
                      tickformat=plt.options[['y.tickformat']],
                      exponentformat = "B",
                      tickfont=list(size=plt.options[['y.tickfont']])))
  
  if (link.cases.plot){
    
    secondary.plt <- plot_ly(data = subplt.dat, type = "scatter", mode = "lines", colors = STATE_COLS, legendgroup=~mState.Providence, 
                             showlegend=FALSE, customdata = ~mState.Providence, source = plt.options[['source.id']]) %>%
      add_trace(x=~x_var, y = ~y_val, color = ~mState.Providence, mode = "lines", line = list(width=plt.options[["line.width"]]), type = 'scatter',
                hovertext = ~trace.hovertext, hoverinfo = "text") %>%
      layout(font = list(family = "Arial"),
             xaxis=list(title=plt.options[['xaxis.title']], range = c(plt.options[['xmin']], plt.options[['xmax']])), 
             margin=list(t=42,b=57, pad=3),
             yaxis=list(title=plt.options[['y.axis.title2']],
                        type = plt.options[['y.plot.scale']],
                        range=c(plt.options[['ymin2']], plt.options[['ymax2']]),
                        tickformat=plt.options[['y.tickformat']],
                        exponentformat = "B",
                        tickfont=list(size=plt.options[['y.tickfont']])))
    
  }
  
  
  if (gantt.chart){
    
    # first, pull out all gantt objs and recolor gantt shapes
    
    gantt.shapes <- gantt.data[['gantt.shapes']]
    gantt.tooltip.data <- gantt.data[['gantt.tooltip.data']]
    gantt.annotations <- gantt.data[['gantt.annotations']]
    full.gantt.df <- gantt.data[['full.gantt.df']]
    
    gantt.shapes <- lapply(gantt.shapes, function(el){
      if (el$type == "rect"){
        el[['line']] <- list(color=STATE_COLS[[el$line]])
        el[['fillcolor']] <- STATE_COLS[[el[['fillcolor']]]]
      }
      return(el)
    })
    
    #gantt.height <- 110+(length(unique(region.list))*40)
    gantt.plt <- plot_ly(data =  gantt.tooltip.data, 
                         colors = STATE_COLS[names(STATE_COLS) %in% unique(gantt.tooltip.data$state)], 
                         legendgroup=~state, showlegend=FALSE, source = plt.options[['source.id']],
                         customdata =~state) %>% 
      add_trace(data = gantt.tooltip.data, y = ~y, x = ~x, color = ~state, text = ~text,
                hoverinfo = "text", mode = "markers", type = "scatter", opacity=0, showlegend=FALSE) %>%
      add_annotations(data = gantt.annotations, x=-0.11, xref = "paper", y = ~yval, text = ~paste0("<b>", policy_long,"</b>"), 
                      bgcolor = "rgba(255,255,255,0.8)", fillcolor="#ffffff", showarrow=FALSE, font = list(size=11)) %>%
      layout(
        font = list(family = "Arial"),
        margin = list(l=125, t=42, b=57, pad=3),
        yaxis = list(title = "",
                     range = c(0, nlevels(factor(gantt.annotations$policy_long))),
                     rangemode = "fixed",
                     zeroline = FALSE,
                     showline = FALSE,
                     showticklabels = FALSE,
                     showgrid = FALSE),
        xaxis=list(title=plt.options[['xaxis.title']]),
        shapes = gantt.shapes,
        hovermode = "closest",
        hoverlabel = list(align = "right")
      ) %>%
      config(displayModeBar = FALSE)
  }
  
  if (gantt.chart){
    if (link.cases.plot){
      
      ret.plt <- plotly::subplot(main.plt, gantt.plt, secondary.plt, nrows = 3, 
                                 shareX = TRUE, shareY = FALSE, 
                                 heights = c(0.35, 0.3, 0.35), titleY = TRUE, titleX = TRUE) %>%
        layout(height = 850,
               title = list(text=plt.options[['plot.title']], x = 0, xref="paper"),
               font = list(family = "Arial"),
               margin = list(l=125, t=42, b=57, pad=3)) %>%
        add_annotations(x=0.5, xref="paper", y=0.66, yref="paper", text=plt.options[['xaxis.title']], 
                        showarrow=FALSE, font = list(size=14)) %>%
        event_register("plotly_hover")
      
    }else{
      ret.plt <- plotly::subplot(main.plt, gantt.plt, nrows = 2, 
                                 shareX = TRUE, shareY = FALSE, 
                                 heights = c(0.75, 0.25), titleY = TRUE) %>%
        layout(height = 650,
               title = list(text=plt.options[['plot.title']], x = 0, xref="paper"),
               font = list(family = "Arial"),
               margin = list(l=125, t=42, b=57, pad=3)) %>%
        event_register("plotly_hover")
    }
  }else if (link.cases.plot){
    ret.plt <- plotly::subplot(main.plt, secondary.plt, nrows = 2, 
                               shareX = TRUE, shareY = FALSE, 
                               heights = c(0.5, 0.5), titleY = TRUE) %>%
      layout(height = 800,
             font = list(family = "Arial"),
             title = list(text=plot.title, x = 0, xref="paper")) %>%
      add_annotations(x=0.5, xref="paper", y=0.51, yref="paper", text=plt.options[['xaxis.title']], 
                      showarrow=FALSE, font = list(size=14))  %>%
      event_register("plotly_hover")
  }else{
    ret.plt <- main.plt %>%
      layout(height = 450)  %>%
      event_register("plotly_hover")
  }
  
  return(ret.plt)
}

generate_gantt_data <- function(region.list, plot.data, selected.x){
  
  init.npi <- data.table(NPI_DATA) %>%
    .[, state.region.key := paste(state, Country.Region, sep = "___")] %>%
    .[state.region.key %chin% region.list] %>%
    .[state.region.key %chin% as.character(unique(plot.data$state.region.key))]
  
  if (nrow(init.npi)>0){
    if (selected.x=="all.dates"){
      #these are data.frames
      plt.dat.prep <- select(plot.data, state.region.key) %>%
        distinct() %>%
        mutate(x_var_date = NA)
    }else{
      plt.dat.prep <- plot.data[, c("state.region.key", "x_var_date")] %>%
        unique(.)
    }
    
    
    npi.range.dat <- init.npi[, tmp.txt.helper := paste0(state_abb, ": ", policy_long)] %>%
      .[, list(policy_long, policy, start_date, end_date, tmp.txt.helper, state, state.region.key)] %>%
      unique(.) %>%
      .[!is.na(start_date)] %>%
      .[, policy := factor(policy)] %>%
      .[, policy := droplevels(policy)] %>%
      .[, ':='(policy.seg.step = as.numeric(factor(policy, levels=rev(levels(policy))))-1,
               state = as.factor(state))] %>%
      .[, ':='(n.segment.breaks = nlevels(state)*2 + (nlevels(state) + 1),
               y0_val = (as.numeric(factor(state, levels = rev(levels(state))))-1)*3 + 1,
               y1_val = (as.numeric(factor(state, levels = rev(levels(state))))-1)*3 + 3), 
        by = policy] %>%
      .[, ':='(y0_val = policy.seg.step + (y0_val/n.segment.breaks),
               y1_val = policy.seg.step + (y1_val/n.segment.breaks)), 
        by = policy] %>%
      merge(x = ., 
            y = plt.dat.prep,
            all.x=TRUE, 
            by = "state.region.key") %>%
      .[, ':='(end_date_proxy = ifelse(is.na(end_date), as.Date(start_date)+ddays(365), as.Date(end_date)),
               final.txt.tip = ifelse(is.na(end_date), 
                                      paste0("<b>", tmp.txt.helper, "<br>Implemented: </b>", format(start_date, "%b %d, %Y")),
                                      paste0("<b>", tmp.txt.helper, "<br>Implemented: </b>", format(start_date, "%b %d, %Y"), 
                                             "<br><b>Ended: </b>", format(end_date, "%b %d, %Y"))))] %>%
      .[, keyby = .(state, state.region.key, policy, policy_long, start_date, end_date, end_date_proxy, 
                    policy.seg.step, y0_val, y1_val, final.txt.tip),
        .(x_var_date=unique(x_var_date))] %>%
      .[, ':=' (x_var_npi_start = ifelse(selected.x=="all.dates", as.Date(start_date, origin="1970-01-01"), ifelse(is.na(x_var_date), NA, as.numeric(as.Date(start_date)- as.Date(x_var_date)))),
                x_var_npi_end = ifelse(selected.x=="all.dates", as.Date(end_date_proxy, origin="1970-01-01"), ifelse(is.na(x_var_date), NA, as.numeric(as.Date(end_date_proxy, origin="1970-01-01")- as.Date(x_var_date))))),
        by = .(state, state.region.key, policy, policy_long, start_date, end_date, end_date_proxy, 
               policy.seg.step, y0_val, y1_val, final.txt.tip)] %>%
      .[, end_date_proxy := as.Date(end_date_proxy, origin="1970-01-01")] %>%
      unique(.) %>%
      .[start_date >= as.Date("2020-01-22")] %>%
      as.data.frame(stringsAsFactors = FALSE)
    
    if (selected.x=="all.dates"){
      npi.range.dat$x_var_npi_start <- as.Date(npi.range.dat$x_var_npi_start, origin="1970-01-01")
      npi.range.dat$x_var_npi_end <- as.Date(npi.range.dat$x_var_npi_end, origin="1970-01-01")
    }
    
    
    all.shapes <- list()
    
    for (i in 1:nrow(npi.range.dat)){
      
      if (!is.na(npi.range.dat[['x_var_npi_start']][i])){
        if (!is.na(npi.range.dat[['x_var_npi_end']][i])){
          
          this.state <- as.character(npi.range.dat$state[i])
          gantt.line <- list(type="rect",
                             x0 = npi.range.dat$x_var_npi_start[i],
                             x1 = npi.range.dat$x_var_npi_end[i],
                             y0 = npi.range.dat$y0_val[i],
                             y1 = npi.range.dat$y1_val[i],
                             line = this.state,
                             fillcolor = this.state,
                             xref = "x",
                             yref = "y",
                             opacity = 0.85)
          
          all.shapes <- c(all.shapes, list(gantt.line))
        }
      }
    }
    
    annot.dat <- data.table(npi.range.dat)[, c("policy.seg.step", "policy_long", "policy")] %>% 
      unique(.) %>%
      .[,':='(yval = policy.seg.step + 0.5,
              policy_long = ifelse(policy_long=="Mandatory Quarantine for Travelers", "Quarantine for Travelers", policy_long))] %>%
      as.data.frame(stringsAsFactors = FALSE)
    
    for (j in 1:nrow(annot.dat)){
      horiz.line <- list(type = "line",
                         x0 = 0, x1 = 1, xref = "paper",yref="y",
                         y0 = j, y1 = j, line = list(color = "grey"),
                         opacity = 0.8)
      all.shapes <- c(all.shapes, list(horiz.line)) 
    }
    
    tt.helper.dat <- data.table(npi.range.dat) %>%
      .[!is.na(x_var_npi_start) & x_var_npi_start < Inf & x_var_npi_start > -Inf] %>%
      as_tibble() %>%
      group_by(final.txt.tip) %>%
      do({
        tmp.dat <- .
        const.cols <- data.frame(
          y = (tmp.dat$y0_val[1]+tmp.dat$y1_val[1])/2,
          color = tmp.dat$state[1],
          text = tmp.dat$final.txt.tip[1],
          state=tmp.dat$state[1],
          stringsAsFactors = FALSE
        )
        if (selected.x == "all.dates"){
          date.range <- seq(as.Date(tmp.dat$start_date[1], origin="1970-01-01"), 
                            as.Date(Sys.Date(), origin="1970-01-01"), by = "3 days")
        }else{
          if (is.na(tmp.dat$x_var_npi_end[1]) | is.na(tmp.dat$x_var_npi_start[1])){
            date.range <- seq(from =  -12, 
                              to = 120,
                              by =3)
          }else{
            date.range <- seq(from =  tmp.dat$x_var_npi_start[1], 
                              to = tmp.dat$x_var_npi_end[1],
                              by =3)
          }
        }
        
        ret.df <- merge(x = date.range, const.cols)
        ret.df
      })
      
    ret.obj <- list("gantt.shapes" = all.shapes,
                    "gantt.tooltip.data" = tt.helper.dat,
                    "gantt.annotations" = annot.dat,
                    "full.gantt.df" = npi.range.dat)
  }else{
    
    all.shapes <- list()
    
    annot.dat <- data.table(NPI_DATA) %>% 
      .[, policy := factor(policy)] %>%
      .[, policy := droplevels(policy)] %>%
      .[, ':='(policy.seg.step = as.numeric(factor(policy, levels=rev(levels(policy))))-1,
               state = as.factor(state))] %>%
      .[,list(policy.seg.step, policy_long, policy)] %>%
      unique(.) %>%
      .[,':='(yval = policy.seg.step + 0.5,
              policy_long = ifelse(policy_long=="Mandatory Quarantine for Travelers", "Quarantine for Travelers", policy_long))] %>%
      as.data.frame(stringsAsFactors = FALSE)
    
    for (j in 1:nrow(annot.dat)){
      horiz.line <- list(type = "line",
                         x0 = 0, x1 = 1, xref = "paper",yref="y",
                         y0 = j, y1 = j, line = list(color = "grey"),
                         opacity = 0.8)
      all.shapes <- c(all.shapes, list(horiz.line)) 
    }
    
    ret.obj <- list("gantt.shapes" = all.shapes,
                    "gantt.tooltip.data" = data.frame(),
                    "gantt.annotations" = annot.dat,
                    "full.gantt.df" = npi.range.dat)
    
  }
  return(ret.obj)
}

pull_comp_plot_opts <- function(region.list, selected.x, selected.y, y.scale=NULL, overlay.npis = TRUE, link.cases.plot=FALSE, plot.data, plot.data2){
  
  ret.list <- list()
  
  selected.y.rev <- ifelse(selected.y=="Cases", "Deaths", 
                           ifelse(selected.y=="Deaths", "Cases", "Other"))
  
  y.axis.title.ops <- c("raw.vals"="(Total)", "log10" = "(Logarithmic)", "smoothed.delta" = paste0("(New Daily ", selected.y, ")"), 
                        "per.million.daily" = paste0("(New Daily ", selected.y, " per 100,000)"),
                        "per.million" = "(Persons per 100,000)", "daily.rate" = "(Daily Growth Rate)", 
                        "smoothed.rate" = "(Growth Rate %)", "doubling.days" = "Doubling Time (Days)", "None"="",
                        "mobility" = "Relative Mobility", "R_t" = "Reproduction Rate", "testing" = "Test Positivity",
                        "Mobility" = "Relative Mobility", "Testing" = "Test Positivity", "Hospitalizations" = "Inpatient Occupancy by COVID Patients",
                        "hospitalizations" = "Inpatient Occupancy by COVID Patients")
  
  y.axis.title.ops2 <- c("raw.vals"="(Total)", "log10" = "(Logarithmic)", "smoothed.delta" = paste0("(New Daily ", selected.y.rev, ")"), 
                        "per.million.daily" = paste0("(New Daily ", selected.y.rev, " per 100,000)"),
                        "per.million" = "(Persons per 100,000)", "daily.rate" = "(Daily Growth Rate)", 
                        "smoothed.rate" = "(Growth Rate %)", "doubling.days" = "Doubling Time (Days)", "None"="",
                        "mobility" = "Relative Mobility", "R_t" = "Reproduction Rate", "testing" = "Test Positivity",
                        "Mobility" = "Relative Mobility", "Testing" = "Test Positivity", "Hospitalizations" = "Inpatient Occupancy by COVID Patients",
                        "hospitalizations" = "Inpatient Occupancy by COVID Patients")
  
  time.key = c("all.dates" ="From All Time", "first.case" = "Since First Recorded Case",
               "first.100"="Since First 100 Recorded Cases", "first.death" = "Since First Recorded Death",
               "2.deaths.per.mil" = "Since Hitting 0.2 Deaths per 100,000", "sah.order" = "Since Issuing Stay-At-Home Order",
               "first.20" = "Since First 20 Cases", "first.20.shift" = "Since First 20 Cases (With Region Alignment)",
               "300.cases.per.mil.shift" = "Since Hitting 300 Cases/Million (With Region Alignment)")
  
  scale.key <- c("raw.vals"="","log10"="", "smoothed.delta"="New Daily ", "per.million" = "per 100,000 ", "per.million.daily" = "New Daily ", 
                 "daily.rate"="Daily Growth Rate in ", "smoothed.rate" = "Growth Rate % in ", "doubling.days" = "Doubling Time of ", 
                 "mobility"="Relative ", "R_t"="", "testing"="", "hospitalizations"="")
  
  ret.list[['y.axis.title']] <- ifelse(
    selected.y %in% c("R_t", "Reproduction_Rate"),
    HTML(paste0("Viral Reproduction Rate (R",tags$sub("t"),")")),
    ifelse(selected.y %in% c("Mobility", "Testing", "Hospitalizations"),
           paste(y.axis.title.ops[y.scale]),
           paste(selected.y, y.axis.title.ops[y.scale])
           )
    )
  
  npi.active.text <- ifelse(
    link.cases.plot, ifelse(overlay.npis, " COVID-19 Deaths, Cases & NPI Implementation ", " COVID-19 Deaths & Cases "),
    ifelse(overlay.npis, "& NPI Implementation ", ""))
  
  ret.list[['plt.height']] <- ifelse(link.cases.plot, ifelse(overlay.npis, 850, 800), 
                                     ifelse(overlay.npis, ifelse(y.scale=="hospitalizations", 425, 650), 450)) #lowercase hospitalizations == spread page plot
  
  ret.list[['source.id']] <- "H"
  
  ret.list[["line.width"]] <- ifelse(length(unique(plot.data$mState.Providence))>3, 2, 4)
  
  if (selected.y == "Mobility"){
    ret.list[['plot.title']] <- ifelse(length(region.list)>1,
                         paste0("Relative Mobility ", npi.active.text, time.key[selected.x], sep=""),
                         paste0(str_split(region.list[1], pattern = "___",simplify = TRUE)[1] , " Relative Mobility ", npi.active.text, time.key[selected.x], sep=""))
    
  }else if (selected.y == "Testing"){
    ret.list[['plot.title']] <- ifelse(length(region.list)>1,
                         paste0("Test Positivity ", npi.active.text, time.key[selected.x], sep=""),
                         paste0(str_split(region.list[1], pattern = "___",simplify = TRUE)[1] , " Test Positivity ", npi.active.text, time.key[selected.x], sep=""))
    
  }else if (selected.y == "Hospitalizations"){
    ret.list[['plot.title']] <- ifelse(length(region.list)>1,
                                       paste0("Inpatient Bed Occupancy by COVID-19 Patients ", npi.active.text, time.key[selected.x], sep=""),
                                       paste0(str_split(region.list[1], pattern = "___",simplify = TRUE)[1] , " Inpatient Bed Occupancy by COVID-19 Patients ", npi.active.text, time.key[selected.x], sep=""))
    
  }else if (selected.y == "Reproduction_Rate"){
    ret.list[['plot.title']] <- ifelse(length(region.list)>1,
                         paste0("Reproduction Rate & NPI Implementation", time.key[selected.x], sep=" "),
                         paste0(str_split(region.list[1], pattern = "___",simplify = TRUE)[1] , " Reproduction Rate ", npi.active.text, time.key[selected.x], sep=""))
    
  }else if (link.cases.plot){
    
    ret.list[['plot.title']] <- ifelse(length(region.list)>1,
                         paste0(scale.key[y.scale], npi.active.text, time.key[selected.x], sep=""),
                         paste(str_split(region.list[1], pattern = "___",simplify = TRUE)[1] , npi.active.text, time.key[selected.x], sep="")
    )
    
  }else{
    ret.list[['plot.title']] <- ifelse(length(region.list)>1,
                         paste0(scale.key[y.scale], "COVID-19 Cases ", time.key[selected.x]),
                         paste0(scale.key[y.scale], str_split(region.list[1], pattern = "___",simplify = TRUE)[1],  " COVID-19 Cases ", time.key[selected.x])
    )
  }
  
  if (selected.x == "all.dates"){
    ret.list[['xmin']] <- as.Date(min(plot.data$x_var_date, na.rm = TRUE))
    ret.list[['xmax']] <- as.Date(max(plot.data$x_var_date, na.rm = TRUE))
  }else{
    ret.list[['xmin']] <- min(plot.data$x_var, na.rm = TRUE)
    ret.list[['xmax']] <- max(plot.data$x_var, na.rm = TRUE)
  }
  
  
  ret.list[['ymin']] <- ifelse(y.scale == "daily.rate", min(plot.data$y_val, na.rm = TRUE)*1.05, 0)
  ret.list[['ymax']] <- ifelse(y.scale == "log10", log10(max(plot.data$y_val, na.rm = TRUE)*1.25), max(plot.data$y_val, na.rm = TRUE)*1.25)
  
  ret.list[['ymin2']] <- ifelse(nrow(plot.data2)>0, ifelse(y.scale == "daily.rate", min(plot.data2$y_val, na.rm = TRUE)*1.05, 0), ret.list[['ymin']])
  ret.list[['ymax2']] <- ifelse(nrow(plot.data2)>0, 
                                ifelse(y.scale == "log10", 
                                       log10(max(plot.data2$y_val, na.rm = TRUE)*1.25),
                                       max(plot.data2$y_val, na.rm = TRUE)*1.25), ret.list[['ymax']])
  
  ret.list[['y.axis.title2']] <- paste(selected.y.rev, y.axis.title.ops2[y.scale])
  
  ret.list[['xaxis.title']] <- switch(selected.x,
                                      "all.dates"="",
                                      "first.case"="Days Since First Case",
                                      "first.100"="Days Since First 100 Cases",
                                      "first.death"="Days Since First Recorded Death",
                                      "2.deaths.per.mil"="Days Since Hitting 0.2 Deaths per 100,000",
                                      "sah.order"="Days Since Issuing Stay-At-Home Order",
                                      "first.20"="Days Since First 20 Cases",
                                      "first.case"="Days Since First Recorded Case",
                                      "300.cases.per.mil.shift" = "Days Since Hitting 300 Cases/Million, with Other Region Alignment",
                                      "first.20.shift"="Days Since First 20 Cases, with Other Region Alignment")
  
  ret.list[['tickformat']] <- ifelse(str_detect(y.scale, pattern = "rate") | y.scale == "daily.rate" | simpleCap(y.scale) == "Hospitalizations", "%", "")
  ret.list[['y.plot.scale']] <- ifelse(y.scale == "log10", "log", "linear")
  ret.list[['y.tickfont']] <- ifelse(y.scale == "log10", 14, 10)
  
  return(ret.list)
 
}

set_comp_y_values <- function(df, selected.y, y.scale, region.list){
  #deal with tooltips here too probably
  
  if (selected.y == "Cases"){
    tmp.df <- df[, raw_y := case_count]
  }else if (selected.y == "Deaths"){
    tmp.df <- df[, raw_y := death_count]
  }else if (selected.y=="Hospitalizations"){
    ret.df <- data.table(HOSPITALIZATION_DATA) %>%
      .[state.region.key %in% region.list] %>%
      .[,':='(date=as.Date(date),
              y_val = hosp_covid_cases/tot_hosp_beds
              )] %>%
      merge(x=., y = df, all.y=TRUE, all.x=FALSE, by = c('state.region.key', 'date')) %>%
      .[, trace.hovertext := paste0("<b>", mState.Providence,"<br>", date, "</b><br>","<i>Inpatient Occupancy by COVID-19 Patients:</i> ", round(100*y_val, 1), "%")]
    
  }else if (selected.y == "R_t" | selected.y == "Reproduction_Rate"){
    tmp.rt <- data.table(RT)[, !c("state_abbr", "state")] %>%
      .[,':='(R_t = ML,
             date = as.Date(date),
             y_val = ML)]
    ret.df <- merge(df, tmp.rt, all.x=TRUE, by = c('state.region.key', 'date')) %>%
      .[,trace.hovertext := paste0("<b>", mState.Providence,"<br>", date, "</b><br>","<i>Viral Reproduction Rate:</i> ", round(y_val, 3))]
  }else if (selected.y == "Mobility"){
    tmp.mob <-  data.table(MOBILITY_DATA) %>%
      .[, ':='(state.region.key = paste(mState.Providence, Country.Region, sep="___"),
               date = as.Date(date),
               y_val = percent_off_norm_maxMobility)] %>%
      .[, list(date, y_val, state.region.key)]
    
    ret.df <- merge(df, tmp.mob, all.x=TRUE, by = c("state.region.key", "date")) %>%
      .[,trace.hovertext := paste0("<b>", mState.Providence,"<br>", date, "</b><br>","<i>Relative Mobility:</i> ", round(y_val, 3))]
    
  }else if (selected.y == "Testing"){
    tmp.testing <-  data.table(DAILY_STATE_TESTING)[, list(state, date, daily_positive, daily_negative)] %>%
      .[, ':='(daily_positive = ifelse(is.na(daily_positive), 0, daily_positive),
               daily_negative = ifelse(is.na(daily_negative), 0, daily_negative))] %>%
      .[, total.tests := daily_positive + daily_negative] %>%
      .[, y_val := daily_positive/total.tests] %>%
      .[, list(state, date, y_val)]
    
    ret.df <- merge(df, tmp.testing, all.x=TRUE, by.x=c("State", "date"), by.y = c("state", "date")) %>%
      .[,trace.hovertext := paste0("<b>", mState.Providence,"<br>", date, "</b><br>","<i>Test Positivity:</i> ", round(100*y_val, 3), "%")]
  }
  
  if (selected.y %in% c("Cases", "Deaths")){
    
    if (y.scale == "raw.vals"){
      ret.df <- tmp.df[, y_val := raw_y] %>%
        .[,trace.hovertext := paste0("<b>", mState.Providence,"<br>", date, "</b><br>","<i>", selected.y ,":</i> ", raw_y)]
      
    }else if (y.scale == "smoothed.delta"){
      ret.df <- tmp.df[, lag3 := shift(raw_y, n=3, fill=0, type = "lag"), by = state.region.key] %>%
        .[,y_val := (raw_y-lag3)/3] %>%
        .[,trace.hovertext := paste0("<b>", mState.Providence,"<br>", date, "</b><br>","<i>New Daily ", selected.y ,":</i> ", round(y_val, 3))]
    }else if (y.scale == "log10"){
      ret.df <- tmp.df[, y_val := raw_y, by = state.region.key] %>%
        .[,trace.hovertext := paste0("<b>", mState.Providence,"<br>", date, "</b><br>","<i>", selected.y ,":</i> ", raw_y)]
    }else if (y.scale == "per.million"){
      ret.df <- tmp.df[, y_val :=  100000*(raw_y/population), by = state.region.key] %>%
        .[,trace.hovertext := paste0("<b>", mState.Providence,"<br>", date, "</b><br>","<i>per 100,000 ", selected.y ,":</i> ", round(y_val,3))]
    }else if (y.scale == "per.million.daily"){
      ret.df <- tmp.df[, lag3 := shift(raw_y, n=3, fill=0, type = "lag"), by = state.region.key] %>%
        .[,y_val := (raw_y-lag3)/3/population*100000] %>%                           
        .[, lag3 := NULL] %>%
        .[,trace.hovertext := paste0("<b>", mState.Providence,"<br>", date, "</b><br>","<i>New Daily ", selected.y ,":</i> ", round(y_val,3))]
    }else if (y.scale == "daily.rate"){
      ret.df <- tmp.df[, ':='(
        lag1 = shift(raw_y, n=1, fill=0, type = "lag"),
        lag2 = shift(raw_y, n=2, fill=0, type = "lag"),
        lag3 = shift(raw_y, n=3, fill=0, type = "lag")),
        by = state.region.key] %>%
        .[, ':='(
            lag1r = ifelse(lag1 == 0 & raw_y>lag1, 1, (raw_y-lag1)/lag1),
            lag2r = ifelse(lag2 == 0 & lag1>lag2, 1, (lag1-lag2)/lag2),
            lag3r = ifelse(lag3 == 0 & lag2>lag3, 1, (lag2-lag3)/lag3))] %>%
        .[,y_val := (lag1r+lag2r+lag3r)/3] %>%
        .[,y_val := ifelse(abs(y_val)>3, 3*abs(y_val)/y_val, y_val)] %>%
        .[, c("lag1", "lag2", "lag3", "lag1r", "lag2r", "lag3r") := NULL] %>%
        .[,trace.hovertext := paste0("<b>", mState.Providence,"<br>", date, "</b><br>","<i>Daily Growth Rate in ", selected.y ,":</i> ", round(100*y_val,1), "%")]
    }else if (y.scale == "smoothed.rate"){
      ret.df <- tmp.df[, ':='(
          lag1 = shift(raw_y, n=1, fill=0, type = "lag"),
          lag2 = shift(raw_y, n=2, fill=0, type = "lag"),
          lag3 = shift(raw_y, n=3, fill=0, type = "lag"),
          lag4 = shift(raw_y, n=4, fill=0, type = "lag"),
          lag5 = shift(raw_y, n=5, fill=0, type = "lag")), 
        by = state.region.key] %>%
        .[, ':='(
          lag1r = ifelse(lag1 == 0 & raw_y>lag1, 1, (raw_y-lag1)/lag1),
          lag2r = ifelse(lag2 == 0 & lag1>lag2, 1, (lag1-lag2)/lag2),
          lag3r = ifelse(lag3 == 0 & lag2>lag3, 1, (lag2-lag3)/lag3),
          lag4r = ifelse(lag4 == 0 & lag3>lag4, 1, (lag3-lag4)/lag4),
          lag5r = ifelse(lag5 == 0 & lag4>lag5, 1, (lag4-lag5)/lag5))] %>%
        .[,  smoothing_denom := (lag1r>0) + (lag2r>0) + (lag3r>0) + (lag4r>0) + (lag5r>0)] %>%
        .[,y_val := (lag1r+lag2r+lag3r+lag4r+lag5r)/smoothing_denom] %>%
        .[, c("lag1", "lag2", "lag3", "lag4", "lag5",
              "lag1r", "lag2r", "lag3r", "lag4r", "lag5r",
               "smoothing_denom") := NULL] %>%
        .[, trace.hovertext := paste0("<b>", mState.Providence,"<br>", date, "</b><br>","<i>Growth Rate % in ", selected.y ,":</i> ", round(100*y_val,1), "%")]
        
    }else if (y.scale == "doubling.days"){
      ret.df <- tmp.df[, ':='(
        lag1 = shift(raw_y, n=1, fill=0, type = "lag"),
        lag2 = shift(raw_y, n=2, fill=0, type = "lag"),
        lag3 = shift(raw_y, n=3, fill=0, type = "lag"),
        lag4 = shift(raw_y, n=4, fill=0, type = "lag"),
        lag5 = shift(raw_y, n=5, fill=0, type = "lag")), 
        by = state.region.key] %>%
        .[, ':='(
          lag1r = ifelse(lag1 == 0 & raw_y>lag1, 1, (raw_y-lag1)/lag1),
          lag2r = ifelse(lag2 == 0 & lag1>lag2, 1, (lag1-lag2)/lag2),
          lag3r = ifelse(lag3 == 0 & lag2>lag3, 1, (lag2-lag3)/lag3),
          lag4r = ifelse(lag4 == 0 & lag3>lag4, 1, (lag3-lag4)/lag4),
          lag5r = ifelse(lag5 == 0 & lag4>lag5, 1, (lag4-lag5)/lag5))] %>%
        .[,  smoothing_denom := (lag1r>0) + (lag2r>0) + (lag3r>0) + (lag4r>0) + (lag5r>0)] %>%
        .[,smoothed.rate := (lag1r+lag2r+lag3r+lag4r+lag5r)/smoothing_denom] %>% 
        .[,y_val := ifelse(smoothed.rate == Inf | smoothed.rate == -Inf, NA, 70/(100*smoothed.rate))] %>%
        .[!is.na(y_val) & y_val > 0 & y_val < Inf] %>%
        .[, c("lag1", "lag2", "lag3", "lag4", "lag5",
              "lag1r", "lag2r", "lag3r", "lag4r", "lag5r",
              "smoothing_denom", "smoothed.rate") := NULL] %>%
        .[,trace.hovertext := paste0("<b>", mState.Providence,"<br>", date, "</b><br>","<i>Doubling Time of ", selected.y ,":</i> ", round(y_val,3))]
        
    }
  }
 
  return(ret.df)
}

set_comp_date_values <- function(df, selected.x, time.shift=NULL){
  
  if (selected.x == "all.dates"){
    ret.df <- df %>%
      .[, ':='(x_var = as.Date(date, origin="1970-01-01"),
               x_var_date = as.Date(date)), 
        by = state.region.key] %>%
      .[, x_var := as.Date(x_var)]
  }else if (selected.x == "first.case"){
    ret.df <- df %>%
      .[, x_var_date := min(date[case_count>0],na.rm = TRUE), by = state.region.key] %>%
      .[,x_var := as.numeric(as.Date(date) - as.Date(x_var_date))]
        
  }else if (selected.x == "first.100"){
    ret.df <- df %>%
      .[, x_var_date := suppressWarnings(min(date[case_count>=100],na.rm = TRUE)), by = state.region.key] %>%
      .[, x_var := as.numeric(as.Date(date) - as.Date(x_var_date))]
                
  }else if (selected.x == "first.20"){
    ret.df <- df %>%
      .[, x_var_date := min(date[case_count>=20],na.rm = TRUE),by = state.region.key] %>%
      .[,x_var := as.numeric(as.Date(date) - as.Date(x_var_date))]
  }else if (selected.x == "first.death"){
    ret.df <- df %>%
      .[, ':='(x_var_date = firstDeathDate,
               x_var =  as.numeric(as.Date(date)- as.Date(firstDeathDate))), 
        by = state.region.key]
  }else if (selected.x == "2.deaths.per.mil"){
    ret.df <- df %>% 
      .[, Deaths_per.million := (death_count/population)*100000] %>%
      .[
        , x_var_date := suppressWarnings(min(date[Deaths_per.million>=0.2],na.rm=TRUE)), by = state.region.key] %>%
      .[
         , x_var := as.numeric(as.Date(date) - as.Date(x_var_date))
        ]
  }else if (selected.x == "sah.order"){
  
    region.list <- unique(df$state.region.key)
    sah.dat <- data.table(NPI_DATA) %>%
      .[, state.region.key := paste(state, Country.Region, sep = "___")] %>%
      .[state.region.key %chin% region.list & policy_long == "Stay At Home Order"] %>%
      .[, .(x_var_date = min(start_date, na.rm=FALSE)), by = state.region.key] %>%
      unique(.)
    
    ret.df <- merge(data.table(df), sah.dat, all.x=TRUE, by = c("state.region.key")) %>%
      .[, x_var := ifelse(is.na(x_var_date) | x_var_date == Inf | x_var_date == -Inf, NA,
                          as.numeric(as.Date(date) - as.Date(x_var_date))),
        by = state.region.key]
  }else if (selected.x == "first.20.shift"){
    ret.df <- df %>%
      .[, first20CasesDate := min(date[case_count>=20],na.rm = TRUE), by = "state.region.key"] %>%
      merge(x = ., y=time.shift, by.x = "state.region.key", by.y="ret.list", all.x=TRUE) %>%
      .[, x_var_date := as.Date(first20CasesDate) + ret.shift] %>%
      .[, x_var := as.numeric(as.Date(date) - as.Date(x_var_date))]
    
  }else if (selected.x == "300.cases.per.mil.shift"){
    ret.df <- df %>%
      .[, first300CasesPerMillionDate := suppressWarnings(min(date[case_count>=300],na.rm = TRUE)), by = "state.region.key"] %>%
      merge(x = ., y=time.shift, by.x = "state.region.key", by.y="ret.list", all.x=TRUE) %>%
      .[, x_var_date := as.Date(first300CasesPerMillionDate) + ret.shift] %>%
      .[, x_var := as.numeric(as.Date(date) - as.Date(x_var_date))]
    
  }else{
    print("Error: Unknown `selected.x` option selected in region_comparison tab.")
  }
  
  if (selected.x != "all.dates"){
    ret.df <- ret.df %>%
      .[, x_var := ifelse(x_var==Inf, NA, x_var)]
  }
 
   return(ret.df)
}


draw_comparisons <- function(region.list, selected.x, selected.y, overlay.npis, y.scale=NULL, plot.title=NULL, time.shift = NULL, gantt.chart=FALSE, link.cases.plot=FALSE, include.legend=TRUE, plt.height=NULL, bar.line.duo = "none"){
  
  npi.plt.dat <- data.frame()
  scale.key <- c("raw.vals"="","log10"="", "smoothed.delta"="New Daily ", "per.million" = "per 100,000 ", "per.million.daily" = "New Daily ", 
                 "daily.rate"="Daily Growth Rate in ", "smoothed.rate" = "Growth Rate % in ", "doubling.days" = "Doubling Time of ", "mobility"="Relative ", "R_t"="", "testing"="")
  tt.label <- paste0(scale.key[y.scale], str_replace_all(selected.y, pattern = "_", replacement = " "))
  tt.label.val <- ifelse(y.scale=="log10" | y.scale=="raw.vals", 
                         selected.y, 
                         paste(selected.y, y.scale, sep="_"))
  
  plt.dat <- STATE_INF_STATS %>%
    union_all(COUNTRY_INF_STATS) %>%
    union_all(MBSA_INF_STATS) %>%
    union_all(COUNTY_INF_STATS) %>%
    mutate(state.region.key = paste(mState.Providence, Country.Region, sep="___")) %>%
    filter(state.region.key %in% region.list) %>%
    group_by(mState.Providence) %>%
    mutate(firstCaseDate = min(date[case_count>0],na.rm = TRUE),
           first100CasesDate = suppressWarnings(min(date[case_count>=100],na.rm=TRUE)), 
           first20CasesDate = suppressWarnings(min(date[case_count>=20],na.rm=TRUE)), 
           daysFromFirstDeath = as.numeric(as.Date(date)- as.Date(firstDeathDate)),
           daysFromFirstCase = as.numeric(as.Date(date) - as.Date(firstCaseDate)),
           daysFromFirst100Cases = as.numeric(as.Date(date) - as.Date(first100CasesDate)),
           daysFromFirst20Cases = as.numeric(as.Date(date) - as.Date(first20CasesDate)),
           date = as.Date(date)
    ) %>%
    ungroup() 

  if (!is.null(time.shift)) {
    plt.dat <- plt.dat %>%
      left_join(time.shift, by = c("state.region.key" = "ret.list")) %>%
      mutate(first20CasesDateShift = as.Date(first20CasesDate) + ret.shift, 
              daysFromFirst20CasesShift = as.numeric(as.Date(date) - as.Date(first20CasesDateShift))
      )
  } else {
    plt.dat <- plt.dat %>% 
      mutate(first20CasesDateShift = NA, 
             daysFromFirst20CasesShift = NA
      )
  }
  
  if (selected.y == "Mobility"){
    plt.dat <- plt.dat %>%
      left_join(MOBILITY_DATA %>%
                  mutate(state.region.key = paste(mState.Providence, Country.Region, sep="___"),
                         date = as.Date(date)) %>%
                  select(date, Mobility_mobility = percent_off_norm_maxMobility, state.region.key) %>%
                  filter(state.region.key %in% region.list),
                by = c("state.region.key", "date"))
  }else{
    plt.dat <- mutate(plt.dat, Mobility_mobility = NA)
  }
  
  if (selected.y == "Testing"){
    plt.dat <- plt.dat %>%
      left_join(select(DAILY_STATE_TESTING, state, date, tot_positive, tot_negative) %>%
                  mutate(tot_positive = ifelse(is.na(tot_positive), 0, tot_positive),
                         tot_negative = ifelse(is.na(tot_negative), 0, tot_negative),
                         total.tests = tot_positive + tot_negative,
                         Testing_testing = tot_positive/total.tests) %>%
                  select(state, date, Testing_testing), 
                by = c("State"="state", "date"="date"))
  }else{
    plt.dat <- mutate(plt.dat, Testing_testing = NA)
  }
  

  plt.dat <- plt.dat %>% 
    left_join(WORLD_POP, by=c("mState.Providence", "Country.Region")) %>%
    select(state=mState.Providence, date, Cases=case_count, Deaths=death_count, Recovered=recovered_count, 
           firstCaseDate, firstDeathDate, first100CasesDate, first20CasesDate,first20CasesDateShift,
           daysFromFirstDeath, daysFromFirstCase,daysFromFirst100Cases, daysFromFirst20Cases, daysFromFirst20CasesShift, population,
           Mobility_mobility,Testing_testing,
           state.region.key) %>%
    left_join((RT %>% 
                 select(-state_abbr, -state) %>%
                 mutate(R_t=ML, 
                        date = as.Date(date),
                        `Reproduction_Rate_R_t` = ML) %>%
                 filter(state.region.key %in% region.list)
               ), by=c('state.region.key', 'date')) %>%
    filter(date >= as.Date("2020-01-22")) %>%
    left_join(
      NPI_DATA %>%
        mutate(state.region.key = paste(state, Country.Region, sep = "___")) %>%
        filter(state.region.key %in% region.list) %>%
        filter(policy_long == "Stay At Home Order") %>%
        select(state.region.key, start_date) %>%
        group_by(state.region.key) %>%
        summarise(firstSAHDate = min(start_date, na.rm=FALSE)) %>%
        ungroup() %>%
        distinct(),
      by = "state.region.key"
    ) %>%
    group_by(state) %>% # grouping to create delta/diff vars
    arrange(as.Date(date)) %>% 
    mutate(
      daysFromSAH = ifelse(is.na(firstSAHDate) | firstSAHDate == Inf | firstSAHDate == -Inf, NA, as.numeric(as.Date(date) - as.Date(firstSAHDate))),
      Cases_smoothed.delta = (Cases - lag(Cases, n = 3, default = 0))/3,
      Deaths_smoothed.delta = (Deaths - lag(Deaths, n = 3, default = 0))/3,
      Recovered_smoothed.delta = (Recovered - lag(Recovered, n = 3, default = 0))/3,
      Cases_log10 = log10(Cases),
      Deaths_log10 = log10(Deaths),
      Recovered_log10 = log10(Recovered),
      Cases_per.million = (Cases/population)*100000,
      Deaths_per.million = (Deaths/population)*100000,
      Recovered_per.million = (Recovered/population)*100000,
      Cases_per.million.daily = (Cases - lag(Cases, n = 3, default = 0))/3/population*100000,
      Deaths_per.million.daily = (Deaths - lag(Deaths, n = 3, default = 0))/3/population*100000,
      
      Cases_lag1 = Cases - lag(Cases, n = 1, default = 0),
      Deaths_lag1 = Deaths - lag(Deaths, n = 1, default = 0),
      Recovered_lag1 = Recovered - lag(Recovered, n = 1, default = 0),
      Cases_lag1r = (Cases_lag1 - lag(Cases_lag1, n=1, default = 0))/lag(Cases_lag1, n=1, default = 1),
      Deaths_lag1r = (Deaths_lag1 - lag(Deaths_lag1, n=1, default = 0))/lag(Deaths_lag1, n=1, default = 1),
      Recovered_lag1r = (Recovered_lag1 - lag(Recovered_lag1, n=1, default = 0))/lag(Recovered_lag1, n=1, default = 1),
      
      Cases_lag2 = lag(Cases, n = 1, default = 0) - lag(Cases, n = 2, default = 0),
      Deaths_lag2 =  lag(Deaths, n = 1, default = 0) - lag(Deaths, n = 2, default = 0),
      Recovered_lag2 =  lag(Recovered, n = 1, default = 0) - lag(Recovered, n = 2, default = 0),
      
      Cases_lag2r = (Cases_lag2 - lag(Cases_lag2, n=1, default = 0))/lag(Cases_lag2, n=1, default = 1),
      Deaths_lag2r = (Deaths_lag2 - lag(Deaths_lag2, n=1, default = 0))/lag(Deaths_lag2, n=1, default = 1),
      Recovered_lag2r = (Recovered_lag2 - lag(Recovered_lag2, n=1, default = 0))/lag(Recovered_lag2, n=1, default = 1),
      
      Cases_lag3 =  lag(Cases, n = 2, default = 0) - lag(Cases, n = 3, default = 0),
      Deaths_lag3 =  lag(Deaths, n = 2, default = 0) - lag(Deaths, n = 3, default = 0),
      Recovered_lag3 =  lag(Recovered, n = 2, default = 0) - lag(Recovered, n = 3, default = 0),
      
      Cases_lag3r = (Cases_lag3 - lag(Cases_lag3, n=1, default = 0))/lag(Cases_lag3, n=1, default = 1),
      Deaths_lag3r = (Deaths_lag3 - lag(Deaths_lag3, n=1, default = 0))/lag(Deaths_lag3, n=1, default = 1),
      Recovered_lag3r = (Recovered_lag3 - lag(Recovered_lag3, n=1, default = 0))/lag(Recovered_lag3, n=1, default = 1),
      
      Cases_daily.rate = (Cases_lag1r+Cases_lag2r+Cases_lag3r)/3,
      Deaths_daily.rate = (Deaths_lag1r+Deaths_lag2r+Deaths_lag3r)/3,
      Recovered_daily.rate = (Recovered_lag1r+Recovered_lag2r+Recovered_lag3r)/3,
      
      Cases_daily.rate = ifelse(Cases_daily.rate > 3, 3, ifelse(Cases_daily.rate < -3, -3, Cases_daily.rate)),
      Deaths_daily.rate = ifelse(Deaths_daily.rate > 3, 3, ifelse(Deaths_daily.rate< -3, -3, Deaths_daily.rate)),
      Recovered_daily.rate = ifelse(Recovered_daily.rate > 3, 3, ifelse(Recovered_daily.rate < -3, -3, Recovered_daily.rate)),
      
      Cases_daily.rate1 = ifelse(lag(Cases, n = 1, default = 1) == 0, 0, 
                                (Cases - lag(Cases, n = 1, default = 0))/lag(Cases, n = 1, default = 1)),
      Deaths_daily.rate1 = ifelse(lag(Deaths, n = 1, default = 1)==0, 0, 
                                 (Deaths - lag(Deaths, n = 1, default = 0))/lag(Deaths, n = 1, default = 1)),
      Recovered_daily.rate1 = ifelse(lag(Recovered, n = 1, default = 1)==0, 0, 
                                    (Recovered - lag(Recovered, n = 1, default = 0))/lag(Recovered, n = 1, default = 1)),
      Cases_daily.rate2 = ifelse(lag(Cases, n = 2, default = 1) == 0, 0, 
                                (lag(Cases, n = 1, default = 0) - lag(Cases, n = 2, default = 0))/lag(Cases, n = 2, default = 1)),
      Deaths_daily.rate2 = ifelse(lag(Deaths, n = 2, default = 1)==0, 0, 
                                 (lag(Deaths, n = 1, default = 0) - lag(Deaths, n = 2, default = 0))/lag(Deaths, n = 2, default = 1)),
      Recovered_daily.rate2 = ifelse(lag(Recovered, n = 2, default = 1), 0, 
                                    (lag(Recovered, n = 1, default = 0) - lag(Recovered, n = 2, default = 0))/lag(Recovered, n = 2, default = 1)),
      
      Cases_daily.rate3 = ifelse(lag(Cases, n = 3, default = 1) == 0, 0, 
                                 (lag(Cases, n = 2, default = 0) - lag(Cases, n = 3, default = 0))/lag(Cases, n = 3, default = 1)),
      
      Deaths_daily.rate3 = ifelse(lag(Deaths, n = 3, default = 1)==0, 0, 
                                  (lag(Deaths, n = 2, default = 0) - lag(Deaths, n = 3, default = 0))/lag(Deaths, n = 3, default = 1)),
      
      Recovered_daily.rate3 = ifelse(lag(Recovered, n = 3, default = 1)==0, 0, 
                                     (lag(Recovered, n = 2, default = 0) - lag(Recovered, n = 3, default = 0))/lag(Recovered, n = 3, default = 1)),
      
      Cases_daily.rate4 = ifelse(lag(Cases, n = 4, default = 1) == 0, 0, 
                                 (lag(Cases, n = 3, default = 0) - lag(Cases, n = 4, default = 0))/lag(Cases, n = 4, default = 1)),
      
      Deaths_daily.rate4 = ifelse(lag(Deaths, n = 4, default = 1)==0, 0, 
                                  (lag(Deaths, n = 3, default = 0) - lag(Deaths, n = 4, default = 0))/lag(Deaths, n = 4, default = 1)),
      
      Recovered_daily.rate4 = ifelse(lag(Recovered, n = 4, default = 1)==0, 0, 
                                     (lag(Recovered, n = 3, default = 0) - lag(Recovered, n = 4, default = 0))/lag(Recovered, n = 4, default = 1)),
      
      Cases_daily.rate5 = ifelse(lag(Cases, n = 5, default = 1) == 0, 0, 
                                 (lag(Cases, n = 4, default = 0) - lag(Cases, n = 5, default = 0))/lag(Cases, n = 5, default = 1)),
      
      Deaths_daily.rate5 = ifelse(lag(Deaths, n = 5, default = 1)==0, 0, 
                                  (lag(Deaths, n = 4, default = 0) - lag(Deaths, n = 5, default = 0))/lag(Deaths, n = 5, default = 1)),
      
      Recovered_daily.rate5 = ifelse(lag(Recovered, n = 5, default = 1)==0, 0, 
                                     (lag(Recovered, n = 4, default = 0) - lag(Recovered, n = 5, default = 0))/lag(Recovered, n = 5, default = 1)),
      
      Cases_smoothing_denom = (Cases_daily.rate1 > 0) + (Cases_daily.rate2 > 0) + (Cases_daily.rate3 > 0) + (Cases_daily.rate4 > 0) + (Cases_daily.rate5 > 0),
      Deaths_smoothing_denom = (Deaths_daily.rate1 > 0) + (Deaths_daily.rate2 > 0) + (Deaths_daily.rate3 > 0) + (Deaths_daily.rate4 > 0) + (Deaths_daily.rate5 > 0),
      Recovered_smoothing_denom = (Recovered_daily.rate1 > 0) + (Recovered_daily.rate2 > 0) + (Recovered_daily.rate3 > 0) + (Recovered_daily.rate4 > 0) + (Recovered_daily.rate5 > 0),
      
      Cases_smoothed.rate = (Cases_daily.rate1 + Cases_daily.rate2 + Cases_daily.rate3 + Cases_daily.rate4 + Cases_daily.rate5)/Cases_smoothing_denom,
      Deaths_smoothed.rate = (Deaths_daily.rate1 + Deaths_daily.rate2 + Deaths_daily.rate3 + Deaths_daily.rate4 + Deaths_daily.rate5)/Deaths_smoothing_denom,
      Recovered_smoothed.rate = (Recovered_daily.rate1 + Recovered_daily.rate2 + Recovered_daily.rate3 + Recovered_daily.rate4 + Recovered_daily.rate5)/Recovered_smoothing_denom,
      Cases_doubling.days = ifelse(Cases_smoothed.rate == Inf | Cases_smoothed.rate == -Inf, NA, 70/(100*Cases_smoothed.rate)),
      Deaths_doubling.days = ifelse(Deaths_smoothed.rate == Inf | Deaths_smoothed.rate == -Inf, NA, 70/(100*Deaths_smoothed.rate)),
      Recovered_doubling.days = ifelse(Recovered_smoothed.rate == Inf | Recovered_smoothed.rate == -Inf, NA, 70/(100*Recovered_smoothed.rate)),
      
      trace.hovertext = paste0("<b>", state,"<br>", date, "</b><br>","<i>", tt.label, ": </i>", round(get(tt.label.val),3)),
      first2DeathsPerMillionDate = suppressWarnings(min(date[Deaths_per.million>=2],na.rm=TRUE)),
      daysFromFirst2DeathsPerMillion = as.numeric(as.Date(date) - as.Date(first2DeathsPerMillionDate)),      
      first300CasesPerMillionDate = suppressWarnings(min(date[Cases_per.million>=300],na.rm=TRUE)),
      daysFromFirst300CasesPerMillion = as.numeric(as.Date(date) - as.Date(first300CasesPerMillionDate))
      ) %>%
    ungroup() 
  
  
  if (!is.null(time.shift)) {
    plt.dat <- plt.dat %>%
      left_join(time.shift, by = c("state.region.key" = "ret.list")) %>%
      mutate(first300CasesPerMillionDateShift = as.Date(first300CasesPerMillionDate) + ret.shift, 
             daysFromFirst300CasesPerMillionShift = as.numeric(as.Date(date) - as.Date(first300CasesPerMillionDateShift))
      )
  } else {
    plt.dat <- plt.dat %>% 
      mutate(first300CasesPerMillionDateShift = NA, 
             daysFromFirst300CasesPerMillionShift = NA
      )
  }
  
  if(y.scale=="doubling.days"){
    if (selected.y=="Cases"){
      plt.dat <- plt.dat %>%
        filter(!is.na(Cases_doubling.days), Cases_doubling.days > 0, Cases_doubling.days < Inf)
    }else if (selected.y=="Deaths"){
      plt.dat <- plt.dat %>%
        filter(!is.na(Deaths_doubling.days), Deaths_doubling.days > 0, Deaths_doubling.days < Inf)
    }
  }
  
  y.axis.title.ops <- c("raw.vals"="(Total)", "log10" = "(Logarithmic)", "smoothed.delta" = paste0("(New Daily ", selected.y, ")"), 
                        "per.million.daily" = paste0("(New Daily ", selected.y, " per 100,000)"),
                        "per.million" = "(Persons per 100,000)", "daily.rate" = "(Daily Growth Rate)", 
                        "smoothed.rate" = "(Growth Rate %)", "doubling.days" = "Doubling Time (Days)", "None"="",
                        "mobility" = "Relative Mobility", "R_t" = "Reproduction Rate", "testing" = "Test Positivity")
  
  if (selected.y == "Recovered"){
    y.axis.title <- paste("Recoveries", y.axis.title.ops[y.scale])
  }else if (selected.y == "R_t"){
    y.axis.title = HTML(paste0("Viral Reproduction Rate (R",tags$sub("t"),")"))
  }else if (selected.y == "Mobility" | selected.y == "Testing"){
    y.axis.title <- paste(y.axis.title.ops[y.scale])
  }else if (selected.y == "Reproduction_Rate"){
    y.axis.title <- HTML(paste0("Viral Reproduction Rate (R",tags$sub("t"),")"))
  }else{
    y.axis.title <- paste(selected.y, y.axis.title.ops[y.scale])
  }
  
  y.plot.scale <- ifelse(y.scale == "log10", "log", "linear")
  y.opt2 <- selected.y
  if (!is.null(y.scale)){
    if (y.scale != "raw.vals"){
      selected.y <- paste(selected.y, y.scale, sep="_")
    }
  }
  
  if (!is.null(plot.title)){
    time.key = c("all.dates" ="From All Time", "first.case" = "Since First Recorded Case",
                "first.100"="Since First 100 Recorded Cases", "first.death" = "Since First Recorded Death",
                "2.deaths.per.mil" = "Since Hitting 0.2 Deaths per 100,000", "sah.order" = "Since Issuing Stay-At-Home Order",
                "first.20" = "Since First 20 Cases", "first.20.shift" = "Since First 20 Cases (With Region Alignment)",
                "300.cases.per.mil.shift" = "Since Hitting 300 Cases per 100,000 (With Region Alignment)")

    npi.active.text <- ifelse(
      link.cases.plot, ifelse(overlay.npis, " COVID-19 Deaths, Cases & NPI Implementation ", " COVID-19 Deaths & Cases "),
      ifelse(overlay.npis, "& NPI Implementation ", ""))
    
    if (selected.y %in% c("Recovered", "R_t")){
      plot.title <- ifelse(length(region.list)>1,
                           paste(scale.key[y.scale], "COVID-19", selected.y, time.key[selected.x], sep=" "),
                           paste(str_split(region.list[1], pattern = "___",simplify = TRUE)[1] , "COVID-19", selected.y, time.key[selected.x], sep=" ")
                           )
    }else if (selected.y == "Mobility_mobility"){
      plot.title <- ifelse(length(region.list)>1,
                           paste0("Relative Mobility ", npi.active.text, time.key[selected.x], sep=""),
                           paste0(str_split(region.list[1], pattern = "___",simplify = TRUE)[1] , " Relative Mobility ", npi.active.text, time.key[selected.x], sep=""))
      
    }else if (selected.y == "Testing_testing"){
      plot.title <- ifelse(length(region.list)>1,
                           paste0("Test Positivity ", npi.active.text, time.key[selected.x], sep=""),
                           paste0(str_split(region.list[1], pattern = "___",simplify = TRUE)[1] , " Test Positivity ", npi.active.text, time.key[selected.x], sep=""))
      
    }else if (selected.y == "Reproduction_Rate_R_t"){
      plot.title <- ifelse(length(region.list)>1,
                           paste0("Reproduction Rate & NPI Implementation", time.key[selected.x], sep=" "),
                           paste0(str_split(region.list[1], pattern = "___",simplify = TRUE)[1] , " Reproduction Rate ", npi.active.text, time.key[selected.x], sep=""))
      
    }else if (link.cases.plot){
      
      plot.title <- ifelse(length(region.list)>1,
                           paste0(scale.key[y.scale], npi.active.text, time.key[selected.x], sep=""),
                           paste(str_split(region.list[1], pattern = "___",simplify = TRUE)[1] , npi.active.text, time.key[selected.x], sep="")
      )
      
    }else{
      plot.title <- ifelse(length(region.list)>1,
                           paste0(scale.key[y.scale], "COVID-19 ",  y.opt2, " ", time.key[selected.x]),
                           paste0(scale.key[y.scale], str_split(region.list[1], pattern = "___",simplify = TRUE)[1],  " COVID-19 ",  y.opt2, " ", time.key[selected.x])
      )
    }

  }else{
    plot.title <- NULL
  }
  
  if (nlevels(factor(plt.dat$state))>1){
    STATE_COLS <- viridis(nlevels(factor(plt.dat$state)), end = 0.85)
  }else{
    STATE_COLS <- "#0D2F4F"
  }
  
  names(STATE_COLS) <- levels(factor(plt.dat$state))
  
  if (!gantt.chart & overlay.npis & length(region.list)>0){
    
    npi.plt.dat <- NPI_DATA %>%
      mutate(state.country.key = paste(state, Country.Region, sep = "___")) %>%
      filter(state.country.key %in% region.list) %>% 
      #filter(state %in% state.list) %>%
      left_join(select(plt.dat, firstCaseDate, firstDeathDate,first100CasesDate, first20CasesDate, first20CasesDateShift, 
                       first2DeathsPerMillionDate, first300CasesPerMillionDate, first300CasesPerMillionDateShift, firstSAHDate, !!!selected.y, state.region.key), 
                by = c("state.country.key"="state.region.key")) %>%
      mutate(tmp.txt.helper = paste0(state_abb, ": ", policy_long)) %>%
      distinct() %>%
      group_by(state, state.country.key, start_date) %>%
      summarise(policy.ft = paste(unique(policy), collapse = "\n"),
                tooltip.txt = unique(if_else(!str_detect(state, pattern = "[A-Z]{2}-[A-Z]{2}"),
                                     paste0("<b>",unique(state),"<br>",unique(start_date), "</b><br><i>", paste(unique(policy_long), collapse = "<br>"),"</i>"), #default TT
                                     paste0("<b>",unique(state),"<br>",unique(start_date), "</b><br><i>", paste(unique(tmp.txt.helper), collapse = "<br>"),"</i>") #MSAs that span a state
                                     )),
                policy.long.ft = paste(unique(policy_long), collapse = "\n"),
                firstCaseDate=unique(firstCaseDate),
                firstDeathDate = unique(firstDeathDate),
                first100CasesDate = unique(first100CasesDate),
                first20CasesDate = unique(first20CasesDate),
                first20CasesDateShift = unique(first20CasesDateShift),
                first2DeathsPerMillionDate = unique(first2DeathsPerMillionDate),
                first300CasesPerMillionDate = unique(first300CasesPerMillionDate),
                first300CasesPerMillionDateShift = unique(first300CasesPerMillionDateShift),
                firstSAHDate = unique(firstSAHDate),
                max.y=max(get(selected.y), na.rm=TRUE)#,
                #max.y=ifelse(str_detect(tt.label, "Rate"), max.y*100, max.y)
      ) %>%
      mutate(start_date=as.Date(start_date),
             daysFromFirstDeath = as.numeric(as.Date(start_date)- as.Date(firstDeathDate)),
             daysFromFirst100Cases = as.numeric(as.Date(start_date) - as.Date(first100CasesDate)),
             daysFromFirst20Cases = as.numeric(as.Date(start_date) - as.Date(first20CasesDate)),
             daysFromFirst20CasesShift = as.numeric(as.Date(start_date) - as.Date(first20CasesDateShift)),
             daysFromFirstCase = as.numeric(as.Date(start_date) - as.Date(firstCaseDate)),
             daysFromFirst2DeathsPerMillion = as.numeric(as.Date(start_date) - as.Date(first2DeathsPerMillionDate)),
             daysFromFirst300CasesPerMillion = as.numeric(as.Date(start_date) - as.Date(first300CasesPerMillionDate)),
             daysFromFirst300CasesPerMillionShift = as.numeric(as.Date(start_date) - as.Date(first300CasesPerMillionDateShift)),
             daysFromSAH = ifelse(is.na(firstSAHDate) | firstSAHDate == Inf | firstSAHDate == -Inf, NA, as.numeric(as.Date(start_date) - as.Date(firstSAHDate))),
             ) %>%
      ungroup() %>%
      group_by(state, state.country.key) %>%
      mutate(max.y=ifelse(max.y==0 | max.y == Inf | max.y == -Inf, min(max.y[max.y>0],na.rm=T), max.y),
             max.y = ifelse(is.na(max.y) | max.y == Inf | max.y == -Inf, 
                            ifelse(str_detect(string = toupper(y.scale), "RATE"), 0.1, 0.95), max.y)
             ) %>%
      ungroup() %>%
      distinct() %>%
      filter(start_date >= as.Date("2020-01-22"))
    
    x.scale.map <- c("all.dates"="start_date", "first.case"="daysFromFirstCase", "first.100"="daysFromFirst100Cases", "first.20" = "daysFromFirst20Cases",
                     "first.20.shift" = "daysFromFirst20CasesShift",
                     "first.death"="daysFromFirstDeath", "2.deaths.per.mil"="daysFromFirst2DeathsPerMillion", 
                     "300.cases.per.mil.shift"="daysFromFirst300CasesPerMillionShift", "sah.order" = "daysFromSAH")
    
    if (selected.x == "all.dates"){
      npi.plt.dat.v2 <- plt.dat %>%
        group_by(state) %>%
        #filter(get(x.scale.map[selected.x])>=-1) %>%
        summarise(updated.y = max(get(selected.y), na.rm=TRUE)) %>%
        ungroup()
    }else{
      npi.plt.dat.v2 <- plt.dat %>%
        group_by(state) %>%
        filter(get(x.scale.map[selected.x])>=-1) %>%
        summarise(updated.y = max(get(selected.y), na.rm=TRUE)) %>%
        ungroup()
    }
    
    
    npi.plt.dat <- npi.plt.dat %>%
      left_join(npi.plt.dat.v2, by = "state") %>%
      mutate(max.y = ifelse(!is.na(updated.y) & updated.y != Inf & updated.y != -Inf, updated.y, max.y))
    
    if(selected.y=='R_t'){
      npi.plt.dat = npi.plt.dat %>% filter(start_date >= min(plt.dat[which(!is.na(plt.dat$R_t)), 'date'][[1]]))
    }
    
    npi.lines <- list()
    
    for (i in 1:nrow(npi.plt.dat)) {
      line <- list(
        type = "line",
        line = list(color = "blue"),
        xref = "x",
        yref = "y" #ifelse(str_detect(string = tt.label, pattern = "Rate"), "paper", "y")
      )
      line <- list()
      line[["x0"]] <- ifelse(selected.x=="first.case", 
                             npi.plt.dat$daysFromFirstCase[i], 
                             ifelse(selected.x=="first.death", 
                                    npi.plt.dat$daysFromFirstDeath[i], 
                                    ifelse(selected.x=="first.100", 
                                           npi.plt.dat$daysFromFirst100Cases[i], 
                                           ifelse(selected.x=="first.20", 
                                                 npi.plt.dat$daysFromFirst20Cases[i], 
                                                 ifelse(selected.x=="first.20.shift", 
                                                        npi.plt.dat$daysFromFirst20CasesShift[i], 
                                                       ifelse(selected.x=="2.deaths.per.mil",
                                                              npi.plt.dat$daysFromFirst2DeathsPerMillion[i],
                                                              ifelse(selected.x=="300.cases.per.mil.shift",
                                                                     npi.plt.dat$daysFromFirst300CasesPerMillionShift[i],
                                                                      ifelse(selected.x=="sah.order",
                                                                             npi.plt.dat$daysFromSAH[i],
                                                                             npi.plt.dat$start_date[i]
                                                                            )
                                                                     )
                                                              )
                                                        )
                                                  )
                                           )
                                    )
                             )
      if (selected.x=="all.dates"){
        line[["x0"]] <- as.Date(line[["x0"]], origin="1970-01-01")
      }
      line[["x1"]] <- line[["x0"]]
      line[["y0"]] <- 0
      line[["y1"]] <- npi.plt.dat$max.y[i]
      line[["line"]] <- list(color="lightgrey")
      npi.lines <- c(npi.lines, list(line)) 
    }
    
  }else if (gantt.chart & length(region.list)>0){
    
    npi.range.dat0 <- NPI_DATA %>%
      mutate(state.country.key = paste(state, Country.Region, sep = "___")) %>%
      filter(state.country.key %in% region.list) 
      
    
    if (nrow(npi.range.dat0)>0){
      npi.range.dat <- npi.range.dat0 %>%
        mutate(tmp.txt.helper = paste0(state_abb, ": ", policy_long)) %>%
        select(policy_long, policy, start_date, end_date, tmp.txt.helper, state, state.country.key) %>%
        distinct() %>%
        filter(!is.na(start_date)) %>%
        #filter(policy_long %in% CURRENT_NPIS) %>%
        mutate(
          policy = factor(policy),
          policy = droplevels(policy),
          policy.seg.step = as.numeric(factor(policy, levels=rev(levels(policy))))-1,
          state = as.factor(state)
        ) %>%
        group_by(policy) %>%
        mutate(
          n.segment.breaks = nlevels(state)*2 + (nlevels(state) + 1),
          y0_val = (as.numeric(factor(state, levels = rev(levels(state))))-1)*3 + 1,
          y1_val = y0_val + 2,
          y0_val = policy.seg.step + (y0_val/n.segment.breaks),
          y1_val = policy.seg.step + (y1_val/n.segment.breaks)
        ) %>%
        ungroup() %>%
        #filter(state %in% state.list) %>%
        left_join(select(plt.dat, firstCaseDate, firstDeathDate,first100CasesDate, first20CasesDate, first20CasesDateShift, 
                         first2DeathsPerMillionDate, first300CasesPerMillionDate, first300CasesPerMillionDateShift, firstSAHDate, state.region.key), 
                  by = c("state.country.key"="state.region.key")) %>%
        mutate(
          end_date_proxy = ifelse(is.na(end_date), as.Date(start_date)+ddays(365), as.Date(end_date)),
          final.txt.tip = ifelse(is.na(end_date), 
                                 paste0("<b>", tmp.txt.helper, "<br>Implemented: </b>", format(start_date, "%b %d, %Y")),
                                 paste0("<b>", tmp.txt.helper, "<br>Implemented: </b>", format(start_date, "%b %d, %Y"), 
                                        "<br><b>Ended: </b>", format(end_date, "%b %d, %Y")))) %>%
        group_by(state, state.country.key, policy, policy_long, start_date, end_date, end_date_proxy, 
                 policy.seg.step, y0_val, y1_val, final.txt.tip) %>%
        summarise(firstCaseDate=unique(firstCaseDate),
                  firstDeathDate = unique(firstDeathDate),
                  first100CasesDate = unique(first100CasesDate),
                  first20CasesDate = unique(first20CasesDate),
                  first20CasesDateShift = unique(first20CasesDateShift),
                  first2DeathsPerMillionDate = unique(first2DeathsPerMillionDate),
                  first300CasesPerMillionDate = unique(first300CasesPerMillionDate),
                  first300CasesPerMillionDateShift = unique(first300CasesPerMillionDateShift),
                  firstSAHDate = unique(firstSAHDate)
        ) %>%
        mutate(daysFromFirstDeath = as.numeric(as.Date(start_date)- as.Date(firstDeathDate)),
               daysFromFirst100Cases = as.numeric(as.Date(start_date) - as.Date(first100CasesDate)),
               daysFromFirst20Cases = as.numeric(as.Date(start_date) - as.Date(first20CasesDate)),
               daysFromFirst20CasesShift = as.numeric(as.Date(start_date) - as.Date(first20CasesDateShift)),
               daysFromFirstCase = as.numeric(as.Date(start_date) - as.Date(firstCaseDate)),
               daysFromFirst2DeathsPerMillion = as.numeric(as.Date(start_date) - as.Date(first2DeathsPerMillionDate)),
               daysFromFirst300CasesPerMillion = as.numeric(as.Date(start_date) - as.Date(first300CasesPerMillionDate)),
               daysFromFirst300CasesPerMillionShift = as.numeric(as.Date(start_date) - as.Date(first300CasesPerMillionDateShift)),
               daysFromSAH = ifelse(is.na(firstSAHDate), NA, as.numeric(as.Date(start_date) - as.Date(firstSAHDate))),
               
               endToFirstDeath = as.numeric(as.Date(end_date_proxy, origin="1970-01-01")- as.Date(firstDeathDate)),
               endToFirst100Cases = as.numeric(as.Date(end_date_proxy, origin="1970-01-01") - as.Date(first100CasesDate)),
               endToFirst20Cases = as.numeric(as.Date(end_date_proxy, origin="1970-01-01") - as.Date(first20CasesDate)),
               endToFirst20CasesShift = as.numeric(as.Date(end_date_proxy, origin="1970-01-01") - as.Date(first20CasesDateShift)),
               endToFirstCase = as.numeric(as.Date(end_date_proxy, origin="1970-01-01") - as.Date(firstCaseDate)),
               endToFirst2DeathsPerMillion = as.numeric(as.Date(end_date_proxy, origin="1970-01-01") - as.Date(first2DeathsPerMillionDate)),
               endToFirst300CasesPerMillion = as.numeric(as.Date(end_date_proxy, origin="1970-01-01") - as.Date(first300CasesPerMillionDate)),
               endToFirst300CasesPerMillionShift = as.numeric(as.Date(end_date_proxy, origin="1970-01-01") - as.Date(first300CasesPerMillionDateShift)),
               endToSAH = ifelse(is.na(firstSAHDate), NA, as.numeric(as.Date(end_date_proxy, origin="1970-01-01") - as.Date(firstSAHDate)))
        ) %>%
        ungroup() %>%
        mutate(end_date_proxy = as.Date(end_date_proxy, origin="1970-01-01")) %>%
        distinct() %>%
        filter(start_date >= as.Date("2020-01-22"))
      
      x.scale.map <- c("all.dates"="start_date", "first.case"="daysFromFirstCase", "first.100"="daysFromFirst100Cases", "first.20" = "daysFromFirst20Cases",
                       "first.20.shift" = "daysFromFirst20CasesShift",
                       "first.death"="daysFromFirstDeath", "2.deaths.per.mil"="daysFromFirst2DeathsPerMillion", 
                       "300.cases.per.mil.shift"="daysFromFirst300CasesPerMillionShift", "sah.order" = "daysFromSAH")
      
      x.scale.map.end <- str_replace_all(x.scale.map, pattern = "daysFrom", "endTo")
      names(x.scale.map.end) <- names(x.scale.map)
      x.scale.map.end["all.dates"] <- "end_date_proxy"
      
      all.shapes <- list()
      
      for (i in 1:nrow(npi.range.dat)){
        
        if (!is.na(npi.range.dat[[paste0(x.scale.map[selected.x])]][i])){
          if (!is.na(npi.range.dat[[paste0(x.scale.map.end[selected.x])]][i])){
            
            this.state <- npi.range.dat$state[i]
            gantt.line <- list(type="rect",
                               x0 = npi.range.dat[[paste0(x.scale.map[selected.x])]][i],
                               x1 = npi.range.dat[[paste0(x.scale.map.end[selected.x])]][i],
                               y0 = npi.range.dat$y0_val[i],
                               y1 = npi.range.dat$y1_val[i],
                               line = list(color = STATE_COLS[which(names(STATE_COLS)==this.state)]),
                               fillcolor = STATE_COLS[which(names(STATE_COLS)==this.state)],
                               xref = "x",
                               yref = "y",
                               opacity = 0.85)
            
            all.shapes <- c(all.shapes, list(gantt.line))
          }
        }
      }
      
      annot.dat <- npi.range.dat %>% 
        select(policy.seg.step, policy_long, policy) %>% 
        distinct() %>%
        mutate(yval = policy.seg.step + 0.5,
               policy_long = ifelse(policy_long=="Mandatory Quarantine for Travelers", "Quarantine for Travelers", policy_long))
      
      for (j in 1:nrow(annot.dat)){
        horiz.line <- list(type = "line",
                           x0 = 0, x1 = 1, xref = "paper",yref="y",
                           y0 = j, y1 = j, line = list(color = "grey"),
                           opacity = 0.8)
        all.shapes <- c(all.shapes, list(horiz.line)) 
      }
      
      tt.helper.dat <- npi.range.dat %>%
        mutate(x0.tmp = get(paste0(x.scale.map[selected.x])),
               x1.tmp = get(paste0(x.scale.map.end[selected.x]))) %>%
        filter(!is.na(x0.tmp),
               x0.tmp != Inf,
               x0.tmp != -Inf) %>%
        group_by(final.txt.tip) %>%
        do({
          tmp.dat <- .
          const.cols <- data.frame(
            y = (tmp.dat$y0_val[1]+tmp.dat$y1_val[1])/2,
            color = tmp.dat$state[1],
            text = tmp.dat$final.txt.tip[1],
            state=tmp.dat$state[1],
            stringsAsFactors = FALSE
          )
          if (selected.x == "all.dates"){
            date.range <- seq(as.Date(tmp.dat$start_date[1], origin="1970-01-01"), 
                              as.Date(Sys.Date(), origin="1970-01-01"), by = "3 days")
          }else{
            if (is.na(tmp.dat$x1.tmp[1]) | is.na(tmp.dat$x0.tmp[1])){
              date.range <- seq(from =  -12, 
                                to = 120,
                                by =3)
            }else{
              date.range <- seq(from =  tmp.dat$x0.tmp[1], 
                                to = tmp.dat$x1.tmp[1],
                                by =3)
            }
            
          }
          
          ret.df <- merge(x = date.range, const.cols)
          ret.df
        })
      
      #gantt.height <- 110+(length(unique(region.list))*40)
      gantt.plt <- plot_ly(colors = STATE_COLS[names(STATE_COLS) %in% unique(tt.helper.dat$state)], legendgroup=~state, showlegend=FALSE) %>% 
        add_trace(data = tt.helper.dat, y = ~y, x = ~x, color = ~state, text = ~text,
                  hoverinfo = "text", mode = "markers", type = "scatter", opacity=0, showlegend=FALSE) %>%
        add_annotations(data = annot.dat, x=-0.11, xref = "paper", 
                        y = ~yval, text = ~paste0("<b>", policy_long,"</b>"), 
                        bgcolor = "rgba(255,255,255,0.8)", fillcolor="#ffffff",
                        showarrow=FALSE,
                        #hovertext = ~paste0("<b>", policy_long,"</b>"), 
                        font = list(size=11)) %>%
        layout(
          font = list(family = "Arial"),
          yaxis = list(title = "",
                       range = c(0, nlevels(factor(npi.range.dat$policy_long))),
                       rangemode = "fixed",
                       zeroline = FALSE,
                       showline = FALSE,
                       showticklabels = FALSE,
                       showgrid = FALSE),
          xaxis = list(title = ""),
          shapes = all.shapes,
          hovermode = "closest",
          hoverlabel = list(align = "right")
        ) %>%
        config(displayModeBar = FALSE)
    }else{
      
      all.shapes <- list()
      
      annot.dat <- NPI_DATA %>% 
        mutate(
          policy = factor(policy),
          policy = droplevels(policy),
          policy.seg.step = as.numeric(factor(policy, levels=rev(levels(policy))))-1,
          state = as.factor(state)
        ) %>%
        select(policy.seg.step, policy_long, policy) %>% 
        distinct() %>%
        mutate(yval = policy.seg.step + 0.5,
               policy_long = ifelse(policy_long=="Mandatory Quarantine for Travelers", "Quarantine for Travelers", policy_long))
      
      for (j in 1:nrow(annot.dat)){
        horiz.line <- list(type = "line",
                           x0 = 0, x1 = 1, xref = "paper",yref="y",
                           y0 = j, y1 = j, line = list(color = "grey"),
                           opacity = 0.8)
        all.shapes <- c(all.shapes, list(horiz.line)) 
      }
      
      gantt.plt <- plot_ly(colors = STATE_COLS, showlegend=FALSE) %>%
        add_annotations(data = annot.dat, x=-0.11, xref = "paper", 
                        y = ~yval, text = ~paste0("<b>", policy_long,"</b>"), 
                        bgcolor = "rgba(255,255,255,0.8)", fillcolor="#ffffff",
                        showarrow=FALSE,
                        #hovertext = ~paste0("<b>", policy_long,"</b>"), 
                        font = list(size=11)) %>%
        layout(
          font = list(family = "Arial"),
          yaxis = list(title = "",
                       range = c(0, nlevels(factor(annot.dat$policy_long))),
                       rangemode = "fixed",
                       zeroline = FALSE,
                       showline = FALSE,
                       showticklabels = FALSE,
                       showgrid = FALSE),
          xaxis = list(title = ""),
          shapes = all.shapes,
          hovermode = "closest",
          hoverlabel = list(align = "right")
        ) %>%
        config(displayModeBar = FALSE)
    } 
  }
  
  sel.y.log.opt <- ifelse(y.plot.scale=="log", paste0("~", y.opt2), paste0("~", selected.y)) #ifelse(y.plot.scale=="log", y.opt2, selected.y)
  x.filter.col <- ifelse(y.plot.scale=="log", y.opt2, selected.y)
  x.filter.val <- ifelse(y.scale=="daily.rate", -3, 0)
  
  ymin <- ifelse(y.scale == "daily.rate", min(plt.dat$Cases_daily.rate, plt.dat$Deaths_daily.rate, na.rm=TRUE)*1.05, 0)
  
  if (selected.x=="first.case"){
    
    if (bar.line.duo != "none"){
      tmp.dat <- plt.dat %>%
        group_by(state) %>%
        filter(daysFromFirstCase >= -1)
      
      xmin <- min(tmp.dat$daysFromFirstCase,na.rm = TRUE)
      xmax <- max(tmp.dat$daysFromFirstCase,na.rm = TRUE)
      
      xaxis.title <- "Days since First Case"
      
      yvar.delta <- paste(bar.line.duo, "lag1", sep="_")
      yvar.cum <- paste0(bar.line.duo)
      
      tmp.dat$delta.val <- tmp.dat[[yvar.delta]]
      tmp.dat$cum.val <- tmp.dat[[yvar.cum]]
      tmp.dat$trace.hovertext <- paste0("<b>", tmp.dat$state,"<br>", tmp.dat$date, "</b><br>","<i>New Daily ", bar.line.duo, ": </i>", tmp.dat$delta.val)
      tmp.dat$trace.hovertext2 <- paste0("<b>", tmp.dat$state,"<br>", tmp.dat$date, "</b><br>","<i>Cumulative ", bar.line.duo, ": </i>", tmp.dat$cum.val)
      
      LINE_COL <- "#87DEFF"
      STATE_COLS<- c("#0D2F4F", LINE_COL)
      names(STATE_COLS) <- c(as.character(levels(factor(tmp.dat$state))), "Cumulative")
      
      yaxis2.opts <- list(
        overlaying = "y",
        tickfont = list(color=LINE_COL),
        side = "right",
        range = c(0, max(tmp.dat$cum.val*1.05,na.rm=TRUE)),
        title = paste("Cumulative", bar.line.duo),
        showgrid = FALSE
      )
      
      ret.plt <- plot_ly(type="scatter", colors=STATE_COLS, data = tmp.dat, legendgroup=~state, showlegend=include.legend) %>%
        add_trace(data = tmp.dat, x=~daysFromFirstCase, 
                  y= ~delta.val,
                  color = ~state, type = 'bar',
                  hovertext = ~trace.hovertext,# paste0("<b>", tmp.dat$state,"<br>", tmp.dat$date, "</b><br>","<i># ", selected.y, ": ", round(tmp.dat[[selected.y]],2) ),
                  hoverinfo="text"
        ) %>%
        add_trace(data = tmp.dat, x=~daysFromFirstCase, 
                  y= ~cum.val,
                  color = ~state, mode = 'lines', type = "scatter", 
                  line = list(color = LINE_COL, width=2.5),
                  hovertext = ~trace.hovertext2,# paste0("<b>", tmp.dat$state,"<br>", tmp.dat$date, "</b><br>","<i># ", selected.y, ": ", round(tmp.dat[[selected.y]],2) ),
                  hoverinfo="text", yaxis = "y2", showlegend=FALSE
        ) %>%
        layout(title=list(text=plot.title, x = 0, xref="paper"),
               font = list(family = "Arial"),
               xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
               margin=list(t=42,b=57, pad=3),
               hovermode = "x",
               yaxis=list(title= paste0("New Daily ", bar.line.duo),
                          exponentformat = "B",
                          tickfont=list(size=10)),
               yaxis2=yaxis2.opts
        )
        
      
      
    }else{
      tmp.dat <- plt.dat %>%
        group_by(state) %>%
        filter(daysFromFirstCase >= -1)
      
      xmin <- min(tmp.dat$daysFromFirstCase,na.rm = TRUE)
      xmax <- max(tmp.dat$daysFromFirstCase,na.rm = TRUE)
      
      xaxis.title <- "Days since First Case"
      
      ret.plt <- plot_ly(type="scatter", colors=STATE_COLS, mode="lines", data = tmp.dat, legendgroup=~state, showlegend=include.legend) %>%
        add_trace(data = tmp.dat, x=~daysFromFirstCase, 
                  y= as.formula(sel.y.log.opt), 
                  color = ~state, mode = 'lines',
                  line = list(width=4), type="scatter",
                  hovertext = ~trace.hovertext,# paste0("<b>", tmp.dat$state,"<br>", tmp.dat$date, "</b><br>","<i># ", selected.y, ": ", round(tmp.dat[[selected.y]],2) ),
                  hoverinfo="text"
        )
      
      
      if (nrow(npi.plt.dat)>0 & nrow(tmp.dat)>0){
        npi.min <- min(npi.plt.dat$daysFromFirstCase, na.rm = TRUE)
        xmin <- min(xmin, npi.min, na.rm=TRUE)
        
        ret.plt <- ret.plt %>%
          add_trace(data = npi.plt.dat, x = ~daysFromFirstCase, y=~max.y, color=~state, text = ~policy.ft,
                    textposition="top middle", mode="text",showlegend=FALSE, inherit = F, hovertext = ~tooltip.txt, hoverinfo="text") %>%
          layout(title=list(text=plot.title, x = 0, xref="paper"),
                 font = list(family = "Arial"),
                 legend = list(x = 100, y = 0.5), shapes = npi.lines,
                 xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
                 margin=list(t=42,b=57, pad=3),
                 yaxis=list(title=y.axis.title, 
                            range=c(ymin, max(tmp.dat[,selected.y],na.rm=TRUE)*1.25),
                            type = y.plot.scale,
                            tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                            exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                            tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10))
                 )) 
      }else{
        ret.plt <- layout(ret.plt, 
                          font = list(family = "Arial"),
                          title=list(text=plot.title, x = 0, xref="paper"),
                          xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
                          margin=list(t=42,b=57, pad=3),
                          yaxis=list(title=y.axis.title,
                                     type = y.plot.scale,
                                     range=c(ymin, max(tmp.dat[[selected.y]],na.rm=TRUE)*1.25),
                                     tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                                     exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                                     tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10))))
      }
    }
    
  }else if (selected.x=="first.death"){
    tmp.dat <- plt.dat %>%
      group_by(state) %>%
      filter(get(x.filter.col)>=x.filter.val)
      #filter(daysFromFirstDeath >= -1)

    xmin <- min(tmp.dat$daysFromFirstDeath,na.rm = TRUE)
    xmax <- max(tmp.dat$daysFromFirstDeath,na.rm = TRUE)
    
    xaxis.title <- "Days since First Death"
    
    ret.plt <- plot_ly(type="scatter", colors=STATE_COLS, mode="lines", data = tmp.dat, legendgroup=~state, showlegend=include.legend) %>%
      add_trace(data = tmp.dat, x=~daysFromFirstDeath, 
                y= as.formula(sel.y.log.opt), 
                color = ~state, mode = 'lines',
                line = list(width=4), type="scatter",
                hovertext = ~trace.hovertext,#paste0("<b>", tmp.dat$state,"<br>", tmp.dat$date, "</b><br>","<i># ", selected.y, ": ", round(tmp.dat[[selected.y]],2) ),
                hoverinfo="text")
    
    if (nrow(npi.plt.dat)>0 & nrow(tmp.dat)>0){
      npi.min <- min(npi.plt.dat$daysFromFirstDeath, na.rm = TRUE)
      xmin <- min(xmin, npi.min, na.rm=TRUE)
      
      ret.plt <- ret.plt %>%
        add_trace(data = npi.plt.dat, x = ~daysFromFirstDeath, y=~max.y, color=~state, text = ~policy.ft,
                  textposition="top middle", mode="text",showlegend=FALSE, hovertext = ~tooltip.txt, hoverinfo="text") %>%
        layout(title=list(text=plot.title, x = 0, xref="paper"),
               legend = list(x = 100, y = 0.5), 
               shapes = npi.lines, 
               font = list(family = "Arial"),
               xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
               margin=list(t=42,b=57, pad=3),
                        yaxis=list(title=y.axis.title, 
                                   range=c(ymin, max(tmp.dat[[selected.y]],na.rm=TRUE)*1.25),
                                   type = y.plot.scale,
                                   tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                                   exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                                   tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10)))) 
    }else{
      ret.plt <- layout(ret.plt, 
                        title=list(text=plot.title, x = 0, xref="paper"), 
                        margin=list(t=42,b=57, pad=3),
                        font = list(family = "Arial"),
                        xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
                        yaxis=list(title=y.axis.title,
                                   type = y.plot.scale,
                                   range=c(ymin, max(tmp.dat[[selected.y]],na.rm=TRUE)*1.25),
                                   tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                                   exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                                   tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10))))
    }

    
  }else if (selected.x=="first.100"){
    tmp.dat <- plt.dat %>%
      group_by(state) %>%
      filter(get(x.filter.col) >= x.filter.val) #daysFromFirst100Cases >= -1)
    
    xmin <- min(tmp.dat$daysFromFirst100Cases[tmp.dat$daysFromFirst100Cases>-200],na.rm = TRUE)
    xmax <- max(tmp.dat$daysFromFirst100Cases,na.rm = TRUE)
    
    xaxis.title <- "Days since First 100 Cases"
    
    ret.plt <- plot_ly(type="scatter", colors=STATE_COLS, mode="lines", data = tmp.dat, legendgroup=~state, showlegend=include.legend) %>%
      add_trace(data = tmp.dat, x=~daysFromFirst100Cases, 
                y= as.formula(sel.y.log.opt),  
                color = ~state, mode = 'lines',
                line = list(width=4), type="scatter",
                hovertext = ~trace.hovertext, #paste0("<b>", tmp.dat$state,"<br>", tmp.dat$date, "</b><br>","<i># ", selected.y, ": ", round(tmp.dat[[selected.y]],2) ),
                hoverinfo="text") 
    
    if (nrow(npi.plt.dat)>0 & nrow(tmp.dat)>0){
      npi.min <- min(npi.plt.dat$daysFromFirst100Cases, na.rm = TRUE)
      xmin <- min(xmin, npi.min, na.rm=TRUE)
      
      ret.plt <- ret.plt %>%
        add_trace(data = npi.plt.dat, x = ~daysFromFirst100Cases, y=~max.y, color=~state, text = ~policy.ft,
                  textposition="top middle", mode="text",showlegend=FALSE, hovertext = ~tooltip.txt, hoverinfo="text") %>%
        layout(title=list(text=plot.title, x = 0, xref="paper"), 
               legend = list(x = 100, y = 0.5), 
               font = list(family = "Arial"),
               shapes = npi.lines, xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
               margin=list(t=42,b=57, pad=3),
                        yaxis=list(title=y.axis.title, 
                                   range=c(ymin, max(tmp.dat[[selected.y]],na.rm=TRUE)*1.25),
                                   type = y.plot.scale,
                                   tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                                   exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                                   tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10)))) 
    }else{
      ret.plt <- layout(ret.plt, 
                        title=list(text=plot.title, x = 0, xref="paper"), 
                        margin=list(t=42,b=57, pad=3),
                        font = list(family = "Arial"),
                        xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
                        yaxis=list(title=y.axis.title,
                                   type = y.plot.scale,
                                   range=c(ymin, max(tmp.dat[[selected.y]],na.rm=TRUE)*1.25),
                                   tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                                   exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                                   tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10))))
    }
    
  }else if (selected.x=="first.20"){
    tmp.dat <- plt.dat %>%
      group_by(state) %>%
      filter(get(x.filter.col) >= x.filter.val) #daysFromFirst100Cases >= -1)
    
    xmin <- min(tmp.dat$daysFromFirst20Cases[tmp.dat$daysFromFirst20Cases>-200],na.rm = TRUE)
    xmax <- max(tmp.dat$daysFromFirst20Cases,na.rm = TRUE)
    
    xaxis.title <- "Days since First 20 Cases"
    
    ret.plt <- plot_ly(type="scatter", colors=STATE_COLS, mode="lines", data = tmp.dat, legendgroup=~state, showlegend=include.legend) %>%
      add_trace(data = tmp.dat, x=~daysFromFirst20Cases, 
                y= as.formula(sel.y.log.opt),  
                color = ~state, mode = 'lines',
                line = list(width=4), type="scatter",
                hovertext = ~trace.hovertext, #paste0("<b>", tmp.dat$state,"<br>", tmp.dat$date, "</b><br>","<i># ", selected.y, ": ", round(tmp.dat[[selected.y]],2) ),
                hoverinfo="text")
    
    if (nrow(npi.plt.dat)>0 & nrow(tmp.dat)>0){
      npi.min <- min(npi.plt.dat$daysFromFirst20Cases, na.rm = TRUE)
      xmin <- min(xmin, npi.min, na.rm=TRUE)
      
      ret.plt <- ret.plt %>%
        add_trace(data = npi.plt.dat, x = ~daysFromFirst20Cases, y=~max.y, color=~state, text = ~policy.ft,
                  textposition="top middle", mode="text",showlegend=FALSE, hovertext = ~tooltip.txt, hoverinfo="text") %>%
        layout(title=list(text=plot.title, x = 0, xref="paper"), 
               legend = list(x = 100, y = 0.5), 
               shapes = npi.lines, xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
               margin=list(t=42,b=57, pad=3),
               font = list(family = "Arial"),
               yaxis=list(title=y.axis.title, 
                          range=c(ymin, max(tmp.dat[,selected.y],na.rm=TRUE)*1.25),
                          type = y.plot.scale,
                          tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                          exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                          tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10)))) 
    }else{
      ret.plt <- layout(ret.plt, 
                        title=list(text=plot.title, x = 0, xref="paper"), 
                        margin=list(t=42,b=57, pad=3),
                        font = list(family = "Arial"),
                        xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
                        yaxis=list(title=y.axis.title,
                                   type = y.plot.scale,
                                   range=c(ymin, max(tmp.dat[[selected.y]],na.rm=TRUE)*1.25),
                                   tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                                   exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                                   tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10))))
    }
    
  }else if (selected.x=="first.20.shift"){
    tmp.dat <- plt.dat %>%
      group_by(state) %>%
      filter(get(x.filter.col) >= x.filter.val) #daysFromFirst100Cases >= -1)
    
    xmin <- min(tmp.dat$daysFromFirst20CasesShift[tmp.dat$daysFromFirst20CasesShift>-200],na.rm = TRUE)
    xmax <- max(tmp.dat$daysFromFirst20CasesShift,na.rm = TRUE)
    
    xaxis.title <- "Days since First 20 Cases, with Other Region Alignment"
    
    ret.plt <- plot_ly(type="scatter", colors=STATE_COLS, mode="lines", data = tmp.dat, legendgroup=~state, showlegend=include.legend) %>%
      add_trace(data = tmp.dat, x=~daysFromFirst20CasesShift, 
                y= as.formula(sel.y.log.opt),  
                color = ~state, mode = 'lines',
                line = list(width=4), type="scatter",
                hovertext = ~trace.hovertext, #paste0("<b>", tmp.dat$state,"<br>", tmp.dat$date, "</b><br>","<i># ", selected.y, ": ", round(tmp.dat[[selected.y]],2) ),
                hoverinfo="text")
    
    if (nrow(npi.plt.dat)>0 & nrow(tmp.dat)>0){
      npi.min <- min(npi.plt.dat$daysFromFirst20CasesShift, na.rm = TRUE)
      xmin <- min(xmin, npi.min, na.rm=TRUE)
      
      ret.plt <- ret.plt %>%
        add_trace(data = npi.plt.dat, x = ~daysFromFirst20CasesShift, y=~max.y, color=~state, text = ~policy.ft,
                  textposition="top middle", mode="text",showlegend=FALSE, hovertext = ~tooltip.txt, hoverinfo="text") %>%
        layout(title=list(text=plot.title, x = 0, xref="paper"), 
               legend = list(x = 100, y = 0.5), 
               margin=list(t=42,b=57, pad=3),
               font = list(family = "Arial"),
               shapes = npi.lines, xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
               yaxis=list(title=y.axis.title, 
                          range=c(ymin, max(tmp.dat[,selected.y],na.rm=TRUE)*1.25),
                          type = y.plot.scale,
                          tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                          exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                          tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10)))) 
    }else{
      ret.plt <- layout(ret.plt, 
                        title=list(text=plot.title, x = 0, xref="paper"), 
                        xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
                        margin=list(t=42,b=57, pad=3),
                        font = list(family = "Arial"),
                        yaxis=list(title=y.axis.title,
                                   type = y.plot.scale,
                                   range=c(ymin, max(tmp.dat[[selected.y]],na.rm=TRUE)*1.25),
                                   tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                                   exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                                   tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10))))
    }
    
  }else if (selected.x=="all.dates"){

    # add confidence bands if plotting R_t
    if(selected.y=='R_t'){
      
      tmp.dat <- plt.dat %>% filter(!is.na(R_t)) %>% mutate(below1 = ifelse(R_t <= 1, "Green", "Red"))
      xmin <- min(tmp.dat$date,na.rm = TRUE)
      xmax <- max(tmp.dat$date,na.rm = TRUE)
      xaxis.title <- ""
      
      thresh_line = list(type = "line", x0 = xmin,
           x1 = xmax, y0 = 1, y1 = 1,
           line = list(color = 'rgba(150, 150, 150, 1)', width=3, dash='dash', y_ref='paper'))
      
      if(nrow(tmp.dat) > 0){
        tmp.dat$trace.hovertext = paste0("<b>", tmp.dat$state,"<br>", tmp.dat$date, "</b><br>","<i>R(t): ", round(tmp.dat[[selected.y]],2),
                                         "<br>","<i>Cases ", comma(tmp.dat$Cases))
        
        if (nrow(npi.plt.dat)>0) {
          all.lines = npi.lines
          all.lines[[length(npi.lines)+1]]= thresh_line
        }
        
        ret.plt <- plot_ly(type="scatter", colors = STATE_COLS, mode='lines', data = tmp.dat, legendgroup=~state, showlegend=include.legend) %>% #colors=c('darkred', 'darkgreen'), mode="markers") %>%
          add_trace(data = tmp.dat, 
                    x=~as.Date(date), 
                    y= as.formula(sel.y.log.opt), 
                    #color = ~R_t <= 1, #TODO - split out into red and gren lines above and below 1
                    color= ~state,
                    mode = 'lines',
                    line = list(width=4), type="scatter",
                    hovertext = ~trace.hovertext,
                    hoverinfo="text", 
                    showlegend=F)
        
        state = str_split(region.list[[1]], '___')[[1]][[1]] 
        ## TO DO: not sure why confidence bands aren't plotting KAF
        plot.title = HTML(paste0(state, ' COVID-19 R',tags$sub("t"),' Since ', format(min(tmp.dat$date), format='%B %d')))
        ret.plt <- ret.plt %>% add_ribbons(data = tmp.dat, x=~as.Date(date),y=~R_t,ymin = ~Low_90,ymax = ~High_90,
                                           line = list(color = 'rgba(201, 201, 201, 0.05)'), hoverinfo = "skip",
                                           fillcolor = 'rgba(201, 201, 201, 0.4)',showlegend=F) %>%
          layout(title=list(text=plot.title, x = 0, xref="paper"),
                 font = list(family = "Arial"))
        
      }else{
        ret.plt = plot_ly(type='scatter', mode='markers', data = tmp.dat, legendgroup=~state, showlegend=include.legend) 
      }
           
    }else{
      
      xmin <- min(plt.dat$date,na.rm = TRUE)
      xmax <- max(plt.dat$date,na.rm = TRUE)
      xaxis.title <- ""
      
      ret.plt <- plot_ly(type="scatter", colors=STATE_COLS, mode="lines", data = plt.dat, legendgroup=~state, showlegend=include.legend) %>%
        add_trace(data = plt.dat, 
                  x=~as.Date(date), 
                  y= as.formula(sel.y.log.opt), 
                  color = ~state, mode = 'lines',
                  line = list(width=4), type="scatter",
                  hovertext = ~trace.hovertext, #paste0("<b>", plt.dat$state,"<br>", plt.dat$date, "</b><br>","<i># ", selected.y, ": ", round(plt.dat[[selected.y]],2) ),
                  hoverinfo="text")
      
    }
    
    if (nrow(npi.plt.dat)>0 & nrow(plt.dat)>0){
      npi.min <- min(npi.plt.dat$start_date, na.rm = TRUE)
      xmin <- min(xmin, npi.min, na.rm=TRUE)
      
      ret.plt <- ret.plt %>%
        add_trace(data = npi.plt.dat, x = ~as.Date(start_date), y=~max.y, color=~state,
                  text = ~policy.ft, textposition="top middle", mode="text",showlegend=FALSE, hovertext = ~tooltip.txt, hoverinfo="text") %>%
        layout(legend = list(x = 100, y = 0.5), 
               title=list(text=plot.title, x = 0, xref="paper"),
               font = list(family = "Arial"),
               margin=list(t=42,b=57, pad=3),
               shapes = all.lines, xaxis=list(title=""), 
               yaxis=list(title=y.axis.title, range=c(ymin, max(plt.dat[,selected.y],na.rm=TRUE)*1.25),
                          type = y.plot.scale, tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                          exponentformat = ifelse(y.plot.scale=="log", "B", "B"),tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10))),
               xaxis=list(range = c(xmin, xmax)))
    }else{
        #all.lines = list(thresh_line)
        ret.plt <- layout(ret.plt, 
                          title=list(text=plot.title, x = 0, xref="paper"), 
                          font = list(family = "Arial"),
                          #shapes=all.lines,
                          xaxis=list(title="", range = c(xmin, xmax)), 
                          margin=list(t=42,b=57, pad=3),
                          yaxis=list(title=y.axis.title, 
                                     type = y.plot.scale,
                                     range=c(ymin, max(plt.dat[[selected.y]],na.rm=TRUE)*1.25),
                                     tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                                     exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                                     tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10))))
      }
    
  }else if (selected.x=="2.deaths.per.mil"){
    tmp.dat <- plt.dat %>%
      group_by(state) %>%
      filter(get(x.filter.col)>x.filter.val)
      #filter(daysFromFirst2DeathsPerMillion >= -1)
    xmin <- min(tmp.dat$daysFromFirst2DeathsPerMillion[tmp.dat$daysFromFirst2DeathsPerMillion>-200],na.rm = TRUE)
    xmax <- max(tmp.dat$daysFromFirst2DeathsPerMillion,na.rm = TRUE)
    
    xaxis.title <- "Days since Hitting 0.2 Deaths per 100,000"
    
    ret.plt <- plot_ly(type="scatter", colors=STATE_COLS, mode="lines", data = tmp.dat, legendgroup=~state, showlegend=include.legend) %>%
      add_trace(data = tmp.dat, x=~daysFromFirst2DeathsPerMillion, 
                y= as.formula(sel.y.log.opt),  
                color = ~state, mode = 'lines',
                line = list(width=4), type="scatter",
                hovertext = ~trace.hovertext,
                hoverinfo="text")
    
    if (nrow(npi.plt.dat)>0 & nrow(tmp.dat)>0){
      npi.min <- min(npi.plt.dat$daysFromFirst2DeathsPerMillion, na.rm = TRUE)
      xmin <- min(xmin, npi.min, na.rm=TRUE)
      
      ret.plt <- ret.plt %>%
        add_trace(data = npi.plt.dat, x = ~daysFromFirst2DeathsPerMillion, y=~max.y, color=~state, text = ~policy.ft,
                  textposition="top middle", mode="text",showlegend=FALSE, hovertext = ~tooltip.txt, hoverinfo="text") %>%
        layout(title=list(text=plot.title, x = 0, xref="paper"), 
               legend = list(x = 100, y = 0.5), 
               shapes = npi.lines, xaxis=list(title=xaxis.title, range = c(xmin, xmax)),
               font = list(family = "Arial"),
               margin=list(t=42,b=57, pad=3),
                        yaxis=list(title=y.axis.title, 
                                   range= c(ymin, max(tmp.dat[,selected.y],na.rm=TRUE)*1.25),
                                   type = y.plot.scale,
                                   tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                                   exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                                   tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10)))) 
    }else{
      ret.plt <- layout(ret.plt, 
                        title=list(text=plot.title, x = 0, xref="paper"), 
                        margin=list(t=42,b=57, pad=3),
                        font = list(family = "Arial"),
                        xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
                        yaxis=list(title=y.axis.title,
                                   type = y.plot.scale,
                                   range=c(ymin, max(tmp.dat[[selected.y]],na.rm=TRUE)*1.25),
                                   tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                                   exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                                   tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10))))
    }
    
  }else if (selected.x=="300.cases.per.mil.shift"){
    tmp.dat <- plt.dat %>%
      group_by(state) %>%
      filter(get(x.filter.col)>x.filter.val)
    #filter(daysFromFirst2DeathsPerMillion >= -1)
    xmin <- min(tmp.dat$daysFromFirst300CasesPerMillionShift[tmp.dat$daysFromFirst300CasesPerMillionShift>-200],na.rm = TRUE)
    xmax <- max(tmp.dat$daysFromFirst300CasesPerMillionShift,na.rm = TRUE)
    
    xaxis.title <- "Days since Hitting 300 Cases/Million, with Other Region Alignment"
    
    ret.plt <- plot_ly(type="scatter", colors=STATE_COLS, mode="lines", data = tmp.dat, legendgroup=~state, showlegend=include.legend) %>%
      add_trace(data = tmp.dat, x=~daysFromFirst300CasesPerMillionShift, 
                y= as.formula(sel.y.log.opt),  
                color = ~state, mode = 'lines',
                line = list(width=4), type="scatter",
                hovertext = ~trace.hovertext,
                hoverinfo="text")
    
    if (nrow(npi.plt.dat)>0 & nrow(tmp.dat)>0){
      npi.min <- min(npi.plt.dat$daysFromFirst300CasesPerMillionShift, na.rm = TRUE)
      xmin <- min(xmin, npi.min, na.rm=TRUE)
      
      ret.plt <- ret.plt %>%
        add_trace(data = npi.plt.dat, x = ~daysFromFirst300CasesPerMillionShift, y=~max.y, color=~state, text = ~policy.ft,
                  textposition="top middle", mode="text",showlegend=FALSE, hovertext = ~tooltip.txt, hoverinfo="text") %>%
        layout(title=list(text=plot.title, x = 0, xref="paper"), 
               margin=list(t=42,b=57, pad=3),
               legend = list(x = 100, y = 0.5), 
               font = list(family = "Arial"),
               shapes = npi.lines, xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
               yaxis=list(title=y.axis.title, 
                          range= c(ymin, max(tmp.dat[,selected.y],na.rm=TRUE)*1.25),
                          type = y.plot.scale,
                          tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                          exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                          tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10)))) 
    }else{
      ret.plt <- layout(ret.plt, 
                        title=list(text=plot.title, x = 0, xref="paper"), 
                        font = list(family = "Arial"),
                        margin=list(t=42,b=57, pad=3),
                        xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
                        yaxis=list(title=y.axis.title,
                                   type = y.plot.scale,
                                   range=c(ymin, max(tmp.dat[[selected.y]],na.rm=TRUE)*1.25),
                                   tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                                   exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                                   tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10))))
    }
  }else if (selected.x=="sah.order"){
    tmp.dat <- plt.dat %>%
      group_by(state) %>%
      filter(daysFromSAH>0)#filter(daysFromSAH >= -1)
    
    xaxis.title <- "Days since Stay-At-Home Order Issued"
    
    xmin <- min(tmp.dat$daysFromSAH[tmp.dat$daysFromSAH>-200],na.rm = TRUE)
    xmax <- max(tmp.dat$daysFromSAH,na.rm = TRUE)
  
    ret.plt <- plot_ly(type="scatter", colors=STATE_COLS, mode="lines", data = tmp.dat, legendgroup=~state, showlegend=include.legend) %>%
      add_trace(data = tmp.dat, x=~daysFromSAH, 
                y= as.formula(sel.y.log.opt),  
                color = ~state, mode = 'lines',
                line = list(width=4), type="scatter",
                hovertext = ~trace.hovertext, #paste0("<b>", tmp.dat$state,"<br>", tmp.dat$date, "</b><br>","<i># ", selected.y, ": ", round(tmp.dat[[selected.y]],2) ),
                hoverinfo="text")
    
    if (nrow(npi.plt.dat)>0 & nrow(tmp.dat)>0){
      npi.min <- min(npi.plt.dat$daysFromSAH, na.rm = TRUE)
      xmin <- min(xmin, npi.min, na.rm=TRUE)
      
      ret.plt <- ret.plt %>%
        add_trace(data = npi.plt.dat, x = ~daysFromSAH, y=~max.y, color=~state, text = ~policy.ft,
                  textposition="top middle", mode="text",showlegend=FALSE, hovertext = ~tooltip.txt, hoverinfo="text") %>%
        layout(title=list(text=plot.title, x = 0, xref="paper"), 
               margin=list(t=42,b=57, pad=3),
               font = list(family = "Arial"),
               legend = list(x = 100, y = 0.5), 
               shapes = npi.lines, xaxis=list(title=xaxis.title, range = c(xmin, xmax)), 
               yaxis=list(title=y.axis.title, 
                          range=c(ymin, max(tmp.dat[,selected.y],na.rm=TRUE)*1.25),
                          type = y.plot.scale,
                          tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                          exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                          tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10)))) 
    }else{
      ret.plt <- layout(ret.plt, 
                        title=list(text=plot.title, x = 0, xref="paper"), 
                        xaxis=list(title= xaxis.title, range = c(xmin, xmax)), 
                        margin=list(t=42,b=57, pad=3),
                        font = list(family = "Arial"),
                        yaxis=list(title=y.axis.title,
                                   type = y.plot.scale,
                                   range=c(ymin, max(tmp.dat[[selected.y]],na.rm=TRUE)*1.25),
                                   tickformat=ifelse(str_detect(y.scale, pattern = "rate"), "%", ""),
                                   exponentformat = ifelse(y.plot.scale=="log", "B", "B"),
                                   tickfont=list(size=ifelse(y.plot.scale=="log", 14, 10))))
    }
    
  }
  if(selected.y=='R_t'){
    if(nrow(tmp.dat)>0){
      ret.plt <- ret.plt %>% layout(yaxis=list(range= c(min(.8*min(tmp.dat[,selected.y], na.rm=T),.5),
                                                        max(tmp.dat[,selected.y],na.rm=T)*1.25)),
                                    font = list(family = "Arial"))
    }else{
      ret.plt <- ret.plt %>% layout(title=list(text='<b>Not Enough R_t Data<b>', font=list(color='red'), xref='paper', x=.5),
                                    font = list(family = "Arial"))
    }
  }

  if (gantt.chart){
    req(gantt.plt)
    
    if (link.cases.plot){
      cases.plot <- draw_comparisons(region.list = region.list, selected.x = selected.x, selected.y="Cases",
                                     overlay.npis=FALSE, y.scale = y.scale, plot.title=NULL, gantt.chart = FALSE, include.legend=FALSE)
      
      ret.plt <- plotly::subplot(ret.plt, gantt.plt, cases.plot, nrows = 3, 
                                 shareX = TRUE, shareY = FALSE, 
                                 heights = c(0.35, 0.3, 0.35), titleY = TRUE, titleX = TRUE) %>%
        layout(height = 850,
               title = list(text=plot.title, x = 0, xref="paper"),
               font = list(family = "Arial"),
               margin = list(l=125, t=42, b=57, pad=3)) %>%
        add_annotations(x=0.5, xref="paper", y=0.66, yref="paper", text=xaxis.title, showarrow=FALSE, font = list(size=14))
      
    }else{
      
      if (bar.line.duo != "none"){
        ret.plt <- plotly::subplot(ret.plt, gantt.plt, nrows = 2, 
                                   shareX = TRUE, shareY = FALSE, 
                                   heights = c(0.75, 0.25), titleY = TRUE) %>%
          layout(height = 650,
                 title = list(text=plot.title, x = 0, xref="paper"),
                 font = list(family = "Arial"),
                 margin = list(l=125, t=42, b=57, r=55, pad=3))
      }else{
        ret.plt <- plotly::subplot(ret.plt, gantt.plt, nrows = 2, 
                                   shareX = TRUE, shareY = FALSE, 
                                   heights = c(0.75, 0.25), titleY = TRUE) %>%
          layout(height = 650,
                 title = list(text=plot.title, x = 0, xref="paper"),
                 font = list(family = "Arial"),
                 margin = list(l=125, t=42, b=57, pad=3))
      }
      
    }

  }else if (link.cases.plot){
    cases.plot <- draw_comparisons(region.list = region.list, selected.x = selected.x, selected.y="Cases",
                                   overlay.npis=FALSE, y.scale = y.scale, plot.title=NULL, gantt.chart = FALSE, include.legend=FALSE)
    ret.plt <- plotly::subplot(ret.plt, cases.plot, nrows = 2, 
                               shareX = TRUE, shareY = FALSE, 
                               heights = c(0.5, 0.5), titleY = TRUE) %>%
      layout(height = 800,
             font = list(family = "Arial"),
             title = list(text=plot.title, x = 0, xref="paper")) %>%
      add_annotations(x=0.5, xref="paper", y=0.51, yref="paper", text=xaxis.title, showarrow=FALSE, font = list(size=14))
  }
  
  if(!is.null(plt.height)){
    ret.plt <- ret.plt %>% layout(height=plt.height, font = list(family = "Arial"))
  }
  return(ret.plt)
  
}


return_region_comp_data <- function(region.list, selected.x, y.scale=NULL){
  
  scale.key <- c("raw.vals"="","log10"="", "smoothed.delta"="Smoothed Daily Change in Total ", "per.million" = "per 100,000 ", 
                 "per.million.daily" = "Smoothed Daily Change in Total per 100,000 ", 
                 "daily.rate"="Daily Growth Rate in ", "smoothed.rate" = "3-Day Growth Rate in ", "doubling.days" = "Doubling Time of ")
  
  x.scale.map <- c("all.dates"="start_date", "first.case"="daysFromFirstCase", "first.100"="daysFromFirst100Cases", "first.20" = "daysFromFirst20Cases",
                   "first.20.shift" = "daysFromFirst20CasesShift",
                   "first.death"="daysFromFirstDeath", "2.deaths.per.mil"="daysFromFirst2DeathsPerMillion", 
                   "300.cases.per.mil.shift"="daysFromFirst300CasesPerMillionShift", "sah.order" = "daysFromSAH")
  
  selected.vars <- c("state", "population", "date", x.scale.map[selected.x], paste("Cases", y.scale, sep="_"), paste("Deaths", y.scale, sep="_"))
  
  plt.dat <- STATE_INF_STATS %>%
    union_all(COUNTRY_INF_STATS) %>%
    union_all(MBSA_INF_STATS) %>%
    union_all(COUNTY_INF_STATS) %>%
    mutate(state.region.key = paste(mState.Providence, Country.Region, sep="___")) %>%
    filter(state.region.key %in% region.list) %>%
    group_by(mState.Providence) %>%
    mutate(firstCaseDate = min(date[case_count>0],na.rm = TRUE),
           first100CasesDate = suppressWarnings(min(date[case_count>=100],na.rm=TRUE)), 
           first20CasesDate = suppressWarnings(min(date[case_count>=20],na.rm=TRUE)), 
           daysFromFirstDeath = as.numeric(as.Date(date)- as.Date(firstDeathDate)),
           daysFromFirstCase = as.numeric(as.Date(date) - as.Date(firstCaseDate)),
           daysFromFirst100Cases = as.numeric(as.Date(date) - as.Date(first100CasesDate)),
           daysFromFirst20Cases = as.numeric(as.Date(date) - as.Date(first20CasesDate)),
           date = as.Date(date)
    ) %>%
    ungroup() %>% 
    mutate(first20CasesDateShift = NA, 
           daysFromFirst20CasesShift = NA
    ) %>% 
    left_join(WORLD_POP, by=c("mState.Providence", "Country.Region")) %>%
    select(state=mState.Providence, date, Cases=case_count, Deaths=death_count, Recovered=recovered_count, 
           firstCaseDate, firstDeathDate, first100CasesDate, first20CasesDate,first20CasesDateShift,
           daysFromFirstDeath, daysFromFirstCase,daysFromFirst100Cases, daysFromFirst20Cases, daysFromFirst20CasesShift, population,
           state.region.key) %>%
    left_join((RT %>% select(-state_abbr, -state) %>%
                 mutate(R_t=ML, state.region.key=paste0(region,'___United States')) %>%
                 filter(state.region.key %in% region.list)
    ), by=c('state.region.key', 'date')) %>%
    filter(date >= as.Date("2020-01-22")) %>%
    left_join(
      NPI_DATA %>%
        mutate(state.region.key = paste(state, Country.Region, sep = "___")) %>%
        filter(state.region.key %in% region.list) %>%
        filter(policy_long == "Stay At Home Order") %>%
        select(state.region.key, start_date) %>%
        group_by(state.region.key) %>%
        summarise(firstSAHDate = min(start_date, na.rm=FALSE)) %>%
        ungroup() %>%
        distinct(),
      by = "state.region.key"
    ) %>%
    group_by(state) %>% # grouping to create delta/diff vars
    arrange(as.Date(date)) %>%
    mutate(
      Cases_raw.vals = Cases,
      Deaths_raw.vals = Deaths,
      
      daysFromSAH = ifelse(is.na(firstSAHDate) | firstSAHDate == Inf | firstSAHDate == -Inf, NA, as.numeric(as.Date(date) - as.Date(firstSAHDate))),
      Cases_smoothed.delta = (Cases - lag(Cases, n = 3, default = 0))/3,
      Deaths_smoothed.delta = (Deaths - lag(Deaths, n = 3, default = 0))/3,
      Recovered_smoothed.delta = (Recovered - lag(Recovered, n = 3, default = 0))/3,
      Cases_log10 = log10(Cases),
      Deaths_log10 = log10(Deaths),
      Recovered_log10 = log10(Recovered),
      Cases_per.million = (Cases/population)*100000,
      Deaths_per.million = (Deaths/population)*100000,
      Recovered_per.million = (Recovered/population)*100000,
      Cases_per.million.daily = (Cases - lag(Cases, n = 3, default = 0))/3/population*100000,
      Deaths_per.million.daily = (Deaths - lag(Deaths, n = 3, default = 0))/3/population*100000,
      
      Cases_daily.rate1 = ifelse(lag(Cases, n = 1, default = 1) == 0, 0, 
                                (Cases - lag(Cases, n = 1, default = 0))/lag(Cases, n = 1, default = 1)),
      Deaths_daily.rate1 = ifelse(lag(Deaths, n = 1, default = 1)==0, 0, 
                                 (Deaths - lag(Deaths, n = 1, default = 0))/lag(Deaths, n = 1, default = 1)),
      Recovered_daily.rate1 = ifelse(lag(Recovered, n = 1, default = 1)==0, 0, 
                                    (Recovered - lag(Recovered, n = 1, default = 0))/lag(Recovered, n = 1, default = 1)),
      
      Cases_daily.rate2 = ifelse(lag(Cases, n = 2, default = 1) == 0, 0, 
                                 (lag(Cases, n = 1, default = 0) - lag(Cases, n = 2, default = 0))/lag(Cases, n = 2, default = 1)),
      Deaths_daily.rate2 = ifelse(lag(Deaths, n = 2, default = 1)==0, 0, 
                                  (lag(Deaths, n = 1, default = 0) - lag(Deaths, n = 2, default = 0))/lag(Deaths, n = 2, default = 1)),
      Recovered_daily.rate2 = ifelse(lag(Recovered, n = 2, default = 1), 0, 
                                     (lag(Recovered, n = 1, default = 0) - lag(Recovered, n = 2, default = 0))/lag(Recovered, n = 2, default = 1)),
      
      Cases_daily.rate3 = ifelse(lag(Cases, n = 3, default = 1) == 0, 0, 
                                 (lag(Cases, n = 2, default = 0) - lag(Cases, n = 3, default = 0))/lag(Cases, n = 3, default = 1)),
      
      Deaths_daily.rate3 = ifelse(lag(Deaths, n = 3, default = 1)==0, 0, 
                                  (lag(Deaths, n = 2, default = 0) - lag(Deaths, n = 3, default = 0))/lag(Deaths, n = 3, default = 1)),
      
      Recovered_daily.rate3 = ifelse(lag(Recovered, n = 3, default = 1)==0, 0, 
                                     (lag(Recovered, n = 2, default = 0) - lag(Recovered, n = 3, default = 0))/lag(Recovered, n = 3, default = 1)),
      
      Cases_daily.rate4 = ifelse(lag(Cases, n = 4, default = 1) == 0, 0, 
                                 (lag(Cases, n = 3, default = 0) - lag(Cases, n = 4, default = 0))/lag(Cases, n = 4, default = 1)),
      
      Deaths_daily.rate4 = ifelse(lag(Deaths, n = 4, default = 1)==0, 0, 
                                  (lag(Deaths, n = 3, default = 0) - lag(Deaths, n = 4, default = 0))/lag(Deaths, n = 4, default = 1)),
      
      Recovered_daily.rate4 = ifelse(lag(Recovered, n = 4, default = 1)==0, 0, 
                                     (lag(Recovered, n = 3, default = 0) - lag(Recovered, n = 4, default = 0))/lag(Recovered, n = 4, default = 1)),
      
      Cases_daily.rate5 = ifelse(lag(Cases, n = 5, default = 1) == 0, 0, 
                                 (lag(Cases, n = 4, default = 0) - lag(Cases, n = 5, default = 0))/lag(Cases, n = 5, default = 1)),
      
      Deaths_daily.rate5 = ifelse(lag(Deaths, n = 5, default = 1)==0, 0, 
                                  (lag(Deaths, n = 4, default = 0) - lag(Deaths, n = 5, default = 0))/lag(Deaths, n = 5, default = 1)),
      
      Recovered_daily.rate5 = ifelse(lag(Recovered, n = 5, default = 1)==0, 0, 
                                     (lag(Recovered, n = 4, default = 0) - lag(Recovered, n = 5, default = 0))/lag(Recovered, n = 5, default = 1)),
      
      Cases_smoothing_denom = (Cases_daily.rate1 > 0) + (Cases_daily.rate2 > 0) + (Cases_daily.rate3 > 0) + (Cases_daily.rate4 > 0) + (Cases_daily.rate5 > 0),
      Deaths_smoothing_denom = (Deaths_daily.rate1 > 0) + (Deaths_daily.rate2 > 0) + (Deaths_daily.rate3 > 0) + (Deaths_daily.rate4 > 0) + (Deaths_daily.rate5 > 0),
      Recovered_smoothing_denom = (Recovered_daily.rate1 > 0) + (Recovered_daily.rate2 > 0) + (Recovered_daily.rate3 > 0) + (Recovered_daily.rate4 > 0) + (Recovered_daily.rate5 > 0),
      
      Cases_smoothed.rate = (Cases_daily.rate1 + Cases_daily.rate2 + Cases_daily.rate3 + Cases_daily.rate4 + Cases_daily.rate5)/Cases_smoothing_denom,
      Deaths_smoothed.rate = (Deaths_daily.rate1 + Deaths_daily.rate2 + Deaths_daily.rate3 + Deaths_daily.rate4 + Deaths_daily.rate5)/Deaths_smoothing_denom,
      Recovered_smoothed.rate = (Recovered_daily.rate1 + Recovered_daily.rate2 + Recovered_daily.rate3 + Recovered_daily.rate4 + Recovered_daily.rate5)/Recovered_smoothing_denom,
      Cases_doubling.days = ifelse(Cases_smoothed.rate == Inf | Cases_smoothed.rate == -Inf, NA, 70/(100*Cases_smoothed.rate)),
      Deaths_doubling.days = ifelse(Deaths_smoothed.rate == Inf | Deaths_smoothed.rate == -Inf, NA, 70/(100*Deaths_smoothed.rate)),
      Recovered_doubling.days = ifelse(Recovered_smoothed.rate == Inf | Recovered_smoothed.rate == -Inf, NA, 70/(100*Recovered_smoothed.rate)),
      
      first2DeathsPerMillionDate = suppressWarnings(min(date[Deaths_per.million>=2],na.rm=TRUE)),
      daysFromFirst2DeathsPerMillion = as.numeric(as.Date(date) - as.Date(first2DeathsPerMillionDate)),      
      first300CasesPerMillionDate = suppressWarnings(min(date[Cases_per.million>=300],na.rm=TRUE)),
      daysFromFirst300CasesPerMillion = as.numeric(as.Date(date) - as.Date(first300CasesPerMillionDate))
    ) %>%
    ungroup()
  
  ret.dat <- plt.dat %>%
    select(!!!selected.vars) %>%
    rename(region=state) %>%
    distinct()
  
  return(ret.dat)
}
