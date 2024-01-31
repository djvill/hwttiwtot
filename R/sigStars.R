dropLeadingZero <- function(x) {
  xStrList <- strsplit(as.character(x), "")
  xStrList <-
    sapply(xStrList, 
           function(xStr) {
             if (xStr[1]=="0" & xStr[2]==".") {
               paste0(xStr[-1], collapse="")
             } else if (xStr[1] %in% c("-", "\u2212", "+") & xStr[2]=="0" & xStr[3]==".") {
               paste0(xStr[-2], collapse="")
             } else {
               paste0(xStr, collapse="")
             }
           })
  
  xStrList
}

pValLessThan <- function(pvals, digits=2, leadingEq=TRUE) {
  require(dplyr)
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
                     digits=list(estimate=5, SE=5, tz=3, df=0, p=2),
                     pVal=c("lessthan", "stars", "round", "asis")[1]) {
  require(dplyr)
  require(stringr)
  
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
    pFunc <- function(x) str_c(x %>% format.pval(digits=digits$p) %>% dropLeadingZero(),
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
    ##Round estimate, SE, t/z, and df
    mutate_at(vars("Estimate"), ~ sprintf(str_c("%.", digits$estimate, "f"), .)) %>%
    mutate_at(vars("Std. Error"), ~ sprintf(str_c("%.", digits$SE, "f"), .)) %>%
    mutate_at(vars(matches("[tz] value")), ~ sprintf(str_c("%.", digits$tz, "f"), .)) %>%
    mutate_at(vars(matches("df")), ~ sprintf(str_c("%.", digits$df, "f"), .)) %>%
    ##Apply pFunc() to p-value
    mutate_at(vars(matches("Pr\\(>\\|[tz]\\|\\)")), pFunc) %>% 
    ##Italicize "t/z" and "p"
    rename_at(vars(matches("[tz] value")), str_replace, "([tz])", "\\*\\1\\*") %>%
    rename_at(vars(matches("Pr\\(>\\|[tz]\\|\\)")), ~ "*p*") %>% 
    ##Replace hyphens with proper minus signs
    mutate_all(str_replace, "-", "\u2212")
  ##Add term names
  rownames(df) <- rownames(smry)
  
  df
}
