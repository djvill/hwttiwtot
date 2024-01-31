pValLessThan <- function(pvals, digits=2, leadingEq=TRUE) {
  library(dplyr)
  eq <- if (leadingEq) "=\u00A0" else character(0L)
  case_when(pvals < .0001 ~ "<\u00A0.0001",
            pvals < .001 ~ "<\u00A0.001",
            pvals < .005 ~ "<\u00A0.005",
            pvals < .01 ~ "<\u00A0.01",
            pvals < .05 ~ "<\u00A0.05",
            TRUE ~ paste0(eq, sprintf(paste0("%.", digits, "f"), pvals) %>%
                            dropLeadingZero()))
}

sigStars <- function(model, 
                     digits=c(estimate=5, SE=5, tz=3, df=0, p=2),
                     pVal=c("lessthan", "stars", "round", "asis")[1],
		     italics=TRUE) {
  library(dplyr)
  library(stringr)
  
  ##Get coefficient matrix
  if (any(class(model) %in% c("summary.merMod", "summary.lmerModLmerTest"))) {
    smry <- coef(model) %>% as.data.frame()
  } else if (any(class(model) %in% c("merModLmerTest","lmerModLmerTest"))) {
    smry <- coef(summary(model)) %>% as.data.frame()
  } else if (class(model)=="glmerMod") {
    smry <- coef(summary(model)) %>% as.data.frame()
  }
  
  ##Flesh out digits, providing defaults for missing values
  if (length(digits)==1 & is.integer(digits)) {
    digits <- 
      rep(digits, 5) %>% 
      set_names(c("estimate", "SE", "tz", "df", "p"))
  }
  if (!(all(c("estimate", "SE", "tz", "df", "p") %in% names(digits)))) {
    if (!("estimate" %in% names(digits))) digits <- c(digits, estimate=5)
    if (!("SE" %in% names(digits))) digits <- c(digits, SE=5)
    if (!("tz" %in% names(digits))) digits <- c(digits, tz=3)
    if (!("df" %in% names(digits))) digits <- c(digits, df=0)
    if (!("p" %in% names(digits))) digits <- c(digits, p=2)
  }
  
  ##Define pFunc() based on pVal
  if (pVal=="lessthan") {
    pFunc <- function(x) pValLessThan(x, digits=digits$p, leadingEq=FALSE)
  }
  if (pVal=="stars") {
    pFunc <- function(x) paste0(x %>% format.pval(digits=digits$p) %>% dropLeadingZero(),
                               cut(x, 
                                   breaks=c(-0.001, 0.001, 0.01, 0.05, 0.1, 1), 
                                   labels=c("***", "**", "*", ".", "")))
  }
  if (pVal=="round") {
    pFunc <- function(x) format.pval(x, digits=digits$p)
  }
  if (pVal=="asis") {
    pFunc <- function(x) x
  }
  
  ##Reformat coefficient matrix
  df <- 
    smry %>%
		##Add term names
		rownames_to_column("Term") %>%
    ##Round estimate, SE, t/z, and df
    mutate(across(Estimate, ~ sprintf(str_glue("%.{digits$estimate}f"), .x)),
		       across(Std. Error, ~ sprintf(str_glue("%.{digits$SE}f"), .x)), 
					 across(matches("[tz] value"), ~ sprintf(str_glue("%.{digits$tz}f"), .x)),
					 across(matches("df"), ~ sprintf(str_glue("%.{digits$df}f"), .x)),
					 ##Apply pFunc() to p-value
					 across(matches("Pr\\(>\\|[tz]\\|\\)"), pFunc),
					 ##Replace hyphens with proper minus signs
					 across(everything(), ~ str_replace(.x, "-", "\u2212")))
	
	##Optionally italicize "t/z" and "p"
	if (italicize) {
	  df <- df %>%
		  mutate(across(matches("[tz] value"), ~ str_replace(.x, "([tz])", "\\*\\1\\*"))) %>%					
      rename_with(~ "*p*", matches("Pr\\(>\\|[tz]\\|\\)"))
	}
  
  df
}
