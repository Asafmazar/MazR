library(tidyverse)
library(stringr)
library(yarrr)
library(labelled)
library(readr)
library(devtools)

#link source_url('https://raw.githubusercontent.com/Asafmazar/MazR/master/Maz.R')

###################################
# Pretty colors
###################################

pret_color <- function() {
  cl_pink_l <- '#f58ee9'
  cl_pink <- '#ef43dc'
  cl_blue_l <- '#95b6ff'
  cl_blue <- '#5086ff'
  cl_turq_l <- '#73d1d4'
  cl_turq <- '#17b3b7'
  cl_green_l <- '#74d07f'
  cl_green <- '#17b12b'
  cl_red_l <- '#f89e9b'
  cl_red <- '#f35e5a'
  cl_lav <- '#E0B0FF'
  cl_yellow <- '#FFDB58'
  
  cl_pal <- c(cl_pink_l, cl_pink, cl_blue_l, cl_blue, cl_turq_l, cl_turq,
              cl_green_l, cl_green, cl_red_l, cl_red, cl_lav, cl_yellow)
  
  ps_blue <- '#2E7AC6'
  ps_orange <- '#ED5432'
  ps_lightblue <- '#5DC1FF'
  ps_pink <- '#FE0065'
  ps_yellow <- '#F9A124'
  ps_purple <- '#6B6ECF'
  ps_green <- '#57CE91'
  
  ps_pal <- c(ps_blue, ps_orange, ps_lightblue, ps_pink,
              ps_yellow, ps_purple, ps_green)
  
  hex <- hue_pal()(9)
  
  show_col(c(cl_pal, ps_pal, hex))
}

#############################
# Pre-Merge checks
#############################

pre_merge <- function(x, y, show_ids = F) {

  tb <- tibble(
    xandy = length(intersect(x, y)),
    xnoty = length(setdiff(x, y)),
    ynotx = length(setdiff(y, x))
  ) %>% 
    pivot_longer(xandy:ynotx) %>% 
    mutate(value_com = scales::comma(value),
           prop = scales::percent(prop.table(value), accuracy = 1)
    )

    ids <- list(
      xandy = intersect(x, y),
      xnoty = setdiff(x, y),
      ynotx = setdiff(y, x)
    )

  return(list(summary = tb, ids = ids))
}

##########################################
# Turtle tertile function
##########################################

turtle <- function(x) {

  cat("  _____     ____\n /      \\  |  o | \n|        |/ ___\\| \n|_________/     \n|_|_| |_|_|\n")
    
  quant <- quantile(x, c(0.33, 0.66))
  
  return(fct_case_when(
    x < quant[1] ~ 'Low',
    x < quant[2] ~ 'Medium',
    x >= quant[2] ~ 'High'
  ))
}

##########################################
# Functions for lookup table recoding
##########################################

lookup <- function(dat_col, lookupdat, old_col, new_col) {
  inds_dat <- match(dat_col, lookupdat[[old_col]])
  dat_col[!is.na(inds_dat)] <- lookupdat[[new_col]][na.omit(inds_dat)]
  return(dat_col)
}

#############################
# Functions for using multilevel models in the specr package
#############################

# Random intercept model (only country as grouping variable)
lmer_ri_1 <- function(formula, data,...) {
  require(lme4)
  require(broom.mixed)
  formula <- paste(formula, "+ (1|country)")
  lmer(formula, data)
}

# Including random slopes (only country as grouping variable)
lmer_rs_1 <- function(formula, data,...) {
  require(lme4)
  require(broom.mixed)
  slopevars <- unlist(strsplit(formula, " ~ "))[2]
  formula <- paste0(formula, "+ (1 + ", slopevars, "|country)" )
  lmer(formula, data)
}

# Random intercept model (lifeExp is nested in both countries and years)
lmer_ri_2 <- function(formula, data,...) {
  require(lme4)
  require(broom.mixed)
  slopevars <- unlist(strsplit(formula, " ~ "))[2]
  formula <- paste0(formula, "+ (1|country) + (1|year)")
  lmer(formula, data)
}

# Including random slopes (intercept and slopes are nested in both countries and years)
lmer_rs_2 <- function(formula, data,...) {
  require(lme4)
  require(broom.mixed)
  slopevars <- unlist(strsplit(formula, " ~ "))[2]
  formula <- paste0(formula, "+ (1 + ", slopevars, "|country) + (", slopevars, "|year)" )
  lmer(formula, data)
}

#############################
# Mean-centering function
#############################

mc <- function(x, center = T, scale = F) {
  if(center == T) {x <- x - mean(x, na.rm = T)}
  if(scale == 'std') {x <-  x / sd(x, na.rm = T)}
  if(scale == 'gelman') {x <-  x / (2*(sd(x, na.rm = T)))}
  
  return(x)
}

#############################
# Filter flow
#############################

filter_flow <- function(dat, ..., na_fail = T, sequential = T) {
  
  conds <- enquos(...) # Enquote condition arguments
  
  # Create tibble to store results
  
  tib <- tibble(condition = character(length = length(conds)),
                num_true = numeric(length = length(conds)),
                num_false = numeric(length = length(conds))
  )
  
  dat_all <- dat
  
  num = 1 # Counter
  
  for (i in conds) {
    
    tib[num, 'condition'] <- quo_name(i)
    tib[num, 'num_true'] <- nrow(dat %>% filter(!!i))
    
    if (na_fail == F) {
      tib[num, 'num_false'] <- nrow(dat %>% filter(!(!!i)))
    } else if (na_fail == T) {
      tib[num, 'num_false'] <- nrow(dat) - nrow(dat %>% filter(!!i))
    }
    
    if (sequential == T) { # If require sequential filtering
      dat <- dat %>% filter(!!i)
    } else {}
    
    num <- num + 1
  }
  
  tib <- rbind(tib, c(condition = 'Total',
                      num_true = nrow(dat %>%
                                        filter(!!!conds)),
                      num_false = nrow(dat_all) - nrow(dat %>%
                                                         filter(!!!conds))))
  
  for (n in (1:nrow(tib))) {
    cat(paste0(tib[n, 'condition'], ':'),
        'excluded', tib[n, 'num_false'] %>% as_vector(), '\n')
    if (n == nrow(tib)) {
      cat(paste0(tib[n, 'condition'], ':'),
          'included', tib[n, 'num_true'] %>% as_vector(), '\n')
    }
  }
  
  return(list(summary = tib, final_data = dat))
  
}

# ff <- filter_flow(delta, 
#             duration < 300, 
#             intro == 1)
# 
# ff$summary
# ff$final_data

#############################
# ggplot violin plot boilerplate
#############################

ggplot(dat_sum, aes(x = , y =)) +
  geom_violin(width = 0.5, fill = alpha('dodgerblue', 0.7)) +
  geom_boxplot(width = 0.1, fill = 'white') + # Add boxplot (median and .25 + .75 percentiles)
  labs(title = '', x = '', y = '',
       caption = str_wrap('')) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(labels = ~ percent(., accuracy = 1),
                     breaks = seq(0, 1, .1)) +
  theme(panel.border = element_rect(fill = NA),
        plot.caption=element_text(hjust = 0, size = 12)) + # Add panel border and left-align caption
  easy_text_size(14)
#stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1),
#             geom='crossbar', width = 0.1, fill = 'white') +
#stat_summary(fun.data=mean_cl_normal, geom='crossbar', width = 0.25, fill = 'white') 
#geom_dotplot(binaxis = 'y', stackdir='center', dotsize = .1, position = position_dodge(1)) #+ # Add dots

#############################
# Read variable labels from second line of .csv
# Required libraries: tidyverse, labelled

#############################

read_csv_qual <- function(file, skip = 3) {
  
  df <- read_csv(file,
                 skip = skip, 
                 col_names = as_vector(read.csv(file,nrows=1,header=F)[1,]))
  
  lab <- remove_attributes(as_vector(read.csv(file, skip = 1, nrows = 1, header = F)[1,]),
                           'names')
  
  df <- df %>% set_variable_labels(.labels = lab)
  
  return(df)
}

#############################
# Dual density plot function
#############################

duoDens <- function (dat, iv1, iv2, lab1 = NULL, lab2 = NULL, name = "Type") {
  
  sv1 <- deparse(substitute(iv1))
  sv2 <- deparse(substitute(iv2))
  
  long <- dat %>%
    pivot_longer(cols = c(all_of(sv1), all_of(sv2)))
  
  return(list(ggplot(long, aes(x = value, fill = name)) +
                geom_density(alpha = 0.7) +
                scale_fill_discrete(),
              pirateplot(value ~ name, long, xlab = name)))
}


#############################
#Marginal Means Function
#############################

split.mean <- function(dat, iv1, iv2, dv, stat = "median", 
                       cut = 2, include = "low", drop.mid = F) {
  
  sv1 <- deparse(substitute(iv1))
  sv2 <- deparse(substitute(iv2))
  sdv <- deparse(substitute(dv))
  
  if (cut == 2) {
    if (stat == "median") {
      cutv.1 <- median(dat[,sv1])
      cutv.2 <- median(dat[,sv2])
    } else if (stat == "mean") {
      cutv.1 <- mean(dat[,sv1])
      cutv.2 <- mean(dat[,sv2])
    }
  } else {
    cutv.1 <- quantile(dat[,sv1], probs = seq(0, 1, (1/cut)))
    cutv.2 <- quantile(dat[,sv2], probs = seq(0, 1, (1/cut)))
  }
  
  qv1 <- enquo(iv1)
  qv2 <- enquo(iv2)
  qdv <- enquo(dv)
  
  #We gotta make sense of median split. But later.
  
  iv1.s <- quo_name(qv1)
  iv2.s <- quo_name(qv2)
  dv.s <- quo_name(qdv)
  
  if (cut == 2) {
    
    if (include == "low") {
      
      sumdat <- dat %>% 
        mutate(iv1.bin = factor(ifelse(!! qv1 <= cutv.1, "low","high")),
               iv2.bin = factor(ifelse(!! qv2 <= cutv.2, "low","high"))) %>% 
        group_by(iv1.bin, iv2.bin) %>% 
        summarize(mean = mean(!! qdv, na.rm = T), num = n(),
                  sd = sd(!! qdv, na.rm = T),
                  se = (sd(!! qdv, na.rm = T)/ sqrt(n()))
        ) 
      
    } else if (include == "high") {
      sumdat <- dat %>% 
        mutate(iv1.bin = factor(ifelse(!! qv1 < cutv.1, "low","high")),
               iv2.bin = factor(ifelse(!! qv2 < cutv.2, "low","high"))) %>% 
        group_by(iv1.bin, iv2.bin) %>% 
        summarize(mean = mean(!! qdv, na.rm = T), num = n(),
                  sd = sd(!! qdv, na.rm = T),
                  se = (sd(!! qdv, na.rm = T) / sqrt(n()))
        ) 
    }
    
    print(sumdat[c("num","iv1.bin","iv2.bin")])
    
  } else if (cut == 3) {
    
    if (include == "low") {
      
      sumdat <- dat %>% 
        mutate(iv1.tri = factor(ifelse(!! qv1 <= cutv.1[2], "low",
                                       ifelse(!! qv1 < cutv.1[3], "avg","high"))),
               iv2.tri = factor(ifelse(!! qv2 <= cutv.2[2], "low",
                                       ifelse(!! qv2 < cutv.2[3], "avg","high")))) %>% 
        group_by(iv1.tri, iv2.tri) %>% 
        summarize(mean = mean(!! qdv, na.rm = T), num = n(),
                  sd = sd(!! qdv, na.rm = T),
                  se = (sd(!! qdv, na.rm = T)/ sqrt(n()))
        ) 
      
    } else if (include == "high") {
      
      sumdat <- dat %>% 
        mutate(iv1.tri = factor(ifelse(!! qv1 < cutv.1[2], "low",
                                       ifelse(!! qv1 < cutv.1[3], "avg","high"))),
               iv2.tri = factor(ifelse(!! qv2 < cutv.2[2], "low",
                                       ifelse(!! qv2 < cutv.2[3], "avg","high")))) %>% 
        group_by(iv1.tri, iv2.tri) %>% 
        summarize(mean = mean(!! qdv, na.rm = T), num = n(),
                  sd = sd(!! qdv, na.rm = T),
                  se = (sd(!! qdv, na.rm = T) / sqrt(n()))
        ) 
    }
    
    print(sumdat[c("num","iv1.tri","iv2.tri")])
    
  }
  
  if (drop.mid == T) {
    sumdat <- filter(sumdat, iv1.tri != "avg", iv2.tri != "avg")
  }
  
  if (cut == 2) {
    
    ggplot(sumdat, aes(x = iv1.bin, y = mean, group = iv2.bin, fill = iv2.bin)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, 
                    position = position_dodge(0.9)) +
      labs(x = iv1.s, fill = iv2.s, y = dv.s)
    
  } else if (cut == 3) {
    
    ggplot(sumdat, aes(x = iv1.tri, y = mean, group = iv2.tri, fill = iv2.tri)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, 
                    position = position_dodge(0.9)) +
      labs(x = iv1.s, fill = iv2.s, y = dv.s)
    
  }
}

#############################
# Split correlation function
#############################

split.cor <- function(dat, iv1, iv2, dv, stat = "median", 
                      cut = 2, include = "low") {
  
  sv1 <- deparse(substitute(iv1))
  sv2 <- deparse(substitute(iv2))
  sdv <- deparse(substitute(dv))
  
  if (cut == 2) {
    if (stat == "median") {
      cutv.1 <- median(dat[,sv1])
      cutv.2 <- median(dat[,sv2])
    } else if (stat == "mean") {
      cutv.1 <- mean(dat[,sv1])
      cutv.2 <- mean(dat[,sv2])
    }
  } else {
    cutv.1 <- quantile(dat[,sv1], probs = seq(0, 1, (1/cut)))
    cutv.2 <- quantile(dat[,sv2], probs = seq(0, 1, (1/cut)))
  }
  
  qv1 <- enquo(iv1)
  qv2 <- enquo(iv2)
  qdv <- enquo(dv)
  
  #We gotta make sense of median split. But later.
  
  iv1.s <- quo_name(qv1)
  iv2.s <- quo_name(qv2)
  dv.s <- quo_name(qdv)
  
  if (cut == 2) {
    
    if (include == "low") {
      
      dat <- dat %>% 
        mutate(iv1.bin = factor(ifelse(!! qv1 <= cutv.1, "low","high")),
               iv2.bin = factor(ifelse(!! qv2 <= cutv.2, "low","high")))
      
    } else if (include == "high") {
      dat <- dat %>% 
        mutate(iv1.bin = factor(ifelse(!! qv1 < cutv.1, "low","high")),
               iv2.bin = factor(ifelse(!! qv2 < cutv.2, "low","high")))
    }
    
    sumdat <- dat %>%
      group_by(iv1.bin, iv2.bin) %>% 
      summarize(num = n())
    
    print(sumdat[c("num","iv1.bin","iv2.bin")])
    
  } else if (cut == 3) {
    
    if (include == "low") {
      
      dat <- dat %>% 
        mutate(iv1.tri = factor(ifelse(!! qv1 <= cutv.1[2], "low",
                                       ifelse(!! qv1 < cutv.1[3], "avg","high"))),
               iv2.tri = factor(ifelse(!! qv2 <= cutv.2[2], "low",
                                       ifelse(!! qv2 < cutv.2[3], "avg","high")))) 
    } else if (include == "high") {
      
      dat <- dat %>% 
        mutate(iv1.tri = factor(ifelse(!! qv1 < cutv.1[2], "low",
                                       ifelse(!! qv1 < cutv.1[3], "avg","high"))),
               iv2.tri = factor(ifelse(!! qv2 < cutv.2[2], "low",
                                       ifelse(!! qv2 < cutv.2[3], "avg","high")))) 
    }
    
    sumdat <- dat %>%
      group_by(iv1.tri, iv2.tri) %>% 
      summarize(num = n())
    
    print(sumdat[c("num","iv1.tri","iv2.tri")])
    
  }
  
  #####
  
  # Graphs
  
  #####
  
  if (cut == 2) {
    
    ggplot(sumdat, aes(x = iv1.bin, y = mean, group = iv2.bin, fill = iv2.bin)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, 
                    position = position_dodge(0.9)) +
      labs(x = iv1.s, fill = iv2.s, y = dv.s)
    
  } else if (cut == 3) {
    
    ggplot(sumdat, aes(x = iv1.tri, y = mean, group = iv2.tri, fill = iv2.tri)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, 
                    position = position_dodge(0.9)) +
      labs(x = iv1.s, fill = iv2.s, y = dv.s)
    
  }
  
}

