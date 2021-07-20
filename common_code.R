
# Packages
library(tidyverse) #ggplot2, forcats, dplyr
library(knitr)
library(sjPlot)
library(kableExtra)
library(questionr)
library(gridExtra)
library(scales)


# Global options
opts_chunk$set(echo = FALSE, warning=FALSE, message=FALSE, cache=FALSE, 
               fig.height=4, fig.width=6, fig.align = 'center')
sjPlot::set_theme(base = theme_bw())

# Define color palettes
# CHC branded colors
# blue 1 587db6
# blue 2 5b81a3
# green b5d43b
# yellow ecf0b7

likert.pal.5 <- brewer_pal("div")(5)
likert.pal.5[3] <- "#b2b5a8"

likert.pal.3 <- brewer_pal("div")(3)
likert.pal.3[2] <- "#b2b5a8"

likert.pal.6 <- brewer_pal("div")(6)
likert.pal.6[3] <- "#b2b5a8"

primary.chc.col <- "#336699"

campus.colors <- c("#b5d43b", "#587db6", "#ecf0b7")

# Helper functions

## Display text answers without blank lines
display.text <- function(data, var){
  data %>% filter(!!as.symbol(var)!="") %>% select(!!as.symbol(var)) %>%
    kable(col.names=NULL) %>% 
    kable_styling(full_width = FALSE, 
                  bootstrap_options =c("striped", "responsive", "hover", "condensed"))
}

## Print number of respondents and percent they compose (non-missing) - use this for general bns questions.
print_n_reporting <- function(x) {
  paste0("(n=", 
         sum(!is.na(bns[[x]])), ", ", 
         percent(mean(!is.na(bns[[x]])), accuracy=1), " reporting)."
  )
}

## Print number of respondents and percent they compose (non-missing) - use this for tmp data frames.
print_n_reporting_tmp <- function(x) {
  paste0("(n=", 
         sum(!is.na(tmp[[x]])), ", ", 
         percent(mean(!is.na(tmp[[x]])), accuracy=1), " reporting)."
  )
}

## Plot histogram with a box plot overlay
hist_with_box <- function(question, bpos, bwidth, jwidth, gsize, ylim, xlab) {
  plot_frq(bns[[question]], type='histogram', show.mean = TRUE, ylim=ylim, show.mean.val = FALSE, show.sd = FALSE, geom.size = gsize) +
    geom_boxplot(data=bns, aes_string(x=question, y=bpos), alpha=0.8, width=bwidth, fill="#587db6") +
    geom_jitter(data=bns, aes_string(x=question, y=bpos), position=position_jitter(width=jwidth, height=floor(bwidth/2.3)), alpha=0.7) + 
    xlab(xlab)
}

## Get percent for variable for given value (ex: Housing - Sleeping Places)
get_perct <- function(x, value) {
  paste0(sum(x==value, na.rm=TRUE), "/", sum(!is.na(x)),
         " (", percent(mean(x==value, na.rm = TRUE), accuracy=.1), ")")
}

## Create table of percentages for single multiple choice question (ex: Housing - Current Housing Situation)
question_table <- function(question, values, cnames) {
  temp_df <- data.frame(qs=character(), prc=character(), frq=numeric())
  for (idx in 1:length(values)) {
    temp_df[idx, 1] <- values[idx]
    temp_df[idx, 2] <- get_perct(bns[[question]], values[idx])
    temp_df[idx, 3] <- sum(bns[[question]] == values[idx], na.rm = TRUE)
  }
  temp_df <- temp_df %>% arrange(desc(frq)) %>% select(-frq)
  colnames(temp_df) <- cnames
  temp_df %>% kable() %>% kable_styling(bootstrap_options = "striped") %>% column_spec(2, width='3.5cm')
}

## Show percentage of students who selected given value of multiple binary variables (ex: Student Demographics - Identifiers)
binary_table <- function(var, value, rnames, punc) {
  tmp <- as.data.frame(t(bns[var]))
  tmp2 <- data.frame(Freq=apply(tmp, 1, function(x, value) sum(x == value, na.rm=TRUE), value))
  n.s <- apply(tmp, 1, function(x) sum(!is.na(x)))
  tmp2$label <- paste0(tmp2$Freq, " (", unname(percent(tmp2$Freq/n.s, accuracy=.1)), ")")
  tmp2 <- tmp2 %>% select(-Freq)
  rownames(tmp2) <- paste0(rnames, " (n = ", n.s, ")", punc)
  colnames(tmp2) <- "Yes (%)"
  tmp2 %>% kable() %>% kable_styling(bootstrap_options = "striped") %>% column_spec(2, width='3.5cm')
}

## Plot columns from multiple questions (plots the value from binary table iver)(ex: Food Secutiry - More Eating Situations)
binary_plot <- function(var, value, rnames) {
  tmp <- as.data.frame(t(bns[var]))
  tmp2 <- data.frame(Freq=apply(tmp, 1, function(x) sum(ifelse(x == value, 1, 0), na.rm = TRUE)))
  tmp2$labels <- rnames
  tmp2 <- tmp2 %>% arrange(desc(Freq))
  ggplot(tmp2, aes(x=reorder(labels, -Freq), y=Freq, label=Freq)) + geom_col(fill=primary.chc.col) +
    geom_text(aes(y=Freq+4)) + 
    scale_x_discrete(labels=label_wrap(28)) + ylab('') + xlab('')
}

## Plot "select all" question indicator variables (ex: Personal Demographics - Ethnicity)
prep_binary_vars <- function(question, xlabels) {
  bns %>%  summarize(across(contains(question),
                            list(x = ~ sum(.x, na.rm=TRUE),  # count how many 1's
                                 n = ~ sum(!is.na(.x))), # count how many non-missing values
                            .names = "{.fn}_{.col}")) %>% # specify the new variable names according to which function it's using
    pivot_longer(everything()) %>% # rshape to long format to have one variable for name, and one for value
    mutate(str = substr(name, 1, 1), # extract x and n from the 'name', 
           name = gsub(paste0("x_", question, "_|n_", question, "_"), "", name)) %>% # clean variable name (keep actual variable in context)
    pivot_wider(id_cols = name, values_from = value, names_from = str) %>% # pivot back wide so one column for x and one for n
    mutate(pct = x/n, # calculate percent and label
           pct_lab = paste0(x, "\n(", percent(pct, accuracy=.1),")"), 
           xlab = xlabels, 
           xlab = fct_reorder(xlab, desc(x))) %>% arrange(desc(x))
}

# Load data
load("../data/BNSp2_clean.Rdata")
bns <- BNSp2
rm(BNSp2)
