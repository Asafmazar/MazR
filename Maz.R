library(tidyverse)
library(stringr)

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
source_url
