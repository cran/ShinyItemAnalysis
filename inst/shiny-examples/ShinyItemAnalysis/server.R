################
#GLOBAL LIBRARY
################
#
library(CTT)
library(moments)
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(ltm)
library(psychometric)
#library(difR)
library(deltaPlotR)
library(gridExtra)
library(difNLR)



data('GMAT', package = 'difNLR')
data('GMATtest', package = 'difNLR')
data('GMATkey', package = 'difNLR')

#############
# FUNCTIONS #
#############

####################
#Distractor analysis
#edited version of function from CTT package
############################################

DistractorAnalysis <-  function (items, key, scores, p.table = FALSE, write.csv, g = 3)
{
  items <- as.data.frame(items)
  if (length(key) == 1)
    key <- c(rep(key, ncol(items)))
  if (missing(scores))
    scores <- as.data.frame(score(items, key)$score)
  if (missing(key))
    warning("Answer key is not provided")
  else {
    if (!length(key) == ncol(items)) {
      warning("Answer key is not provided or some item keys are missing.")
    }
    key <- c(key)
  }
  score.level <- quantile(scores[, 1], seq(0,1,by=1/g))
  score.level <- cut(scores[, 1], score.level, include.lowest = TRUE,
                     labels = paste("Group", seq_along(1:g), sep=" "))
  itemtab <- function(response) {
    xtabs( ~ response + score.level)
  }
  itemtabp <- function(response) {
    round(prop.table(xtabs( ~ response + score.level), 2),
          3)
  }
  all.levels <- sort(unique(unlist(items)))
  for (i in 1:ncol(items)) {
    items[, i] <- factor(items[, i], levels = all.levels,
                         labels = ifelse(all.levels == key[i], paste("*", all.levels, sep = ""),
                                         paste(" ", all.levels, sep = "")))
  }
  out <- list()
  if (p.table == FALSE)
    for (i in 1:ncol(items)) {
      out[[i]] <- itemtab(items[, i])
    }
  else for (i in 1:ncol(items)) {
    out[[i]] <- itemtabp(items[, i])
  }
  names(out) <- colnames(items)
  if (!missing(write.csv)) {
    for (i in 1:ncol(items)) {
      tmpItem <- out[[i]]
      tmpItem <- rbind(colnames(tmpItem), tmpItem)
      tmpItem <- cbind(c("response", rownames(tmpItem)[-1]),
                       tmpItem)
      tmpItem <- rbind(c(names(out)[i], " ", " ", " "),
                       tmpItem, c(" ", " ", " ", " "))
      suppressWarnings(write.table(tmpItem, write.csv,
                                   row.names = FALSE, col.names = FALSE, na = " ",
                                   append = TRUE, sep = ","))
    }
  }
  out
}

################################
#PLOT OF DISTRACTOR ANALYSIS
################################

plotDistractorAnalysis <-  function (items, key, g = 3, k = 1)
{
  tabulky <- DistractorAnalysis(items, key, p.table = TRUE, g = g)
  tucna   <- ifelse(rep(is.numeric(key), length(key)), key + 1, match(key, LETTERS))

  linetype <- rep(2, length(unique(items[[k]])))
  linetype[tucna[k]] = 1
  plot(NA, xlim = c(1, g), ylim = c(0, 1),
       main = paste("Item", k),
       ylab = "Option selection percentage",
       xlab = "Group by total score",
       xaxt = "n")
  for (option in 1:length(unique(items[[k]]))){
    lines(1:g, tabulky[[k]][option, ],
          col = 1 + option,
          lty = ifelse(tucna[[k]] == option, 1, 2))
    points(1:g,tabulky[[k]][option, ],
           col = 1 + option,
           pch = ifelse(tucna[[k]] == option, 16, 1))
  }
  legend(1, 1, legend = sort(unique(items[[k]])),
        col = 1 + 1:length(unique(items[[k]])),
        lty = linetype)
  axis(1, at = c(1:g), labels = c(1:g))
}

##################################
#PLOT OF DIF
##################################
difDP <- function (x, pch = 2, pch.mult = 17, axis.draw = TRUE, thr.draw = FALSE,
                   second.thr = 0, dif.draw = c(1, 3), print.corr = FALSE, xlim = NULL, ylim = NULL,
                   xlab = NULL, ylab = NULL, main = NULL, save.plot = FALSE,
                   save.options = c("plot", "default", "pdf"))
{
  internalDP <- function() {
    ra <- range(x$Deltas)
    rm <- ifelse(round(ra[1]) <= 0, 0, round(ra[1]) - 1)
    rM <- ifelse(round(ra[2] + 2) >= 26, 26, round(ra[2]) + 2)

    if (is.null(xlim))
      xl <- c(rm, rM)
    else xl <- xlim
    if (is.null(ylim))
      yl <- c(rm, rM)
    else yl <- ylim
    if (is.null(xlab))
      xla <- "Reference group"
    else xla <- xlab
    if (is.null(ylab))
      yla <- "Focal group"
    else yla <- ylab
    plot(x$Deltas[, 1], x$Deltas[, 2], pch = pch, xlim = xl,
         ylim = yl, xlab = xla, ylab = yla, main = main)
    if (axis.draw) {
      if (is.null(dim(x$axis.par)))
        pars <- x$axis.par
      else pars <- x$axis.par[nrow(x$axis.par), ]
      abline(pars[1], pars[2])
    }
    if (nrow(x$Deltas) != nrow(unique(x$Deltas))) {
      N <- nrow(x$Deltas)
      id <- rep(0, N)
      for (i in 1:(N - 1)) {
        for (j in (i + 1):N) {
          if (sum(x$Deltas[i, ] - x$Deltas[j, ]) == 0)
            id[c(i, j)] <- id[c(i, j)] + 1
        }
      }
      points(x$Deltas[id > 0, 1], x$Deltas[id > 0, 2],
             pch = pch.mult)
    }
    if (!is.character(x$DIFitems))
      points(x$Deltas[x$DIFitems, 1], x$Deltas[x$DIFitems,
                                               2], pch = dif.draw[1], cex = dif.draw[2])
    if (thr.draw) {
      if (is.null(dim(x$axis.par)))
        pars <- x$axis.par
      else pars <- x$axis.par[nrow(x$axis.par), ]
      th <- x$thr[length(x$thr)]
      abline(pars[1] + th * sqrt(pars[2]^2 + 1), pars[2],
             lty = 2, lwd = 2, col = "red")
      abline(pars[1] - th * sqrt(pars[2]^2 + 1), pars[2],
             lty = 2, lwd = 2, col = "red")
      legend("bottomright", c("Normal threshold"),
             col = c("red"), lwd = c(2), lty = c(2), cex = 1)
    }
    if (second.thr > 0) {
      if (is.null(dim(x$axis.par)))
        pars <- x$axis.par
      else pars <- x$axis.par[nrow(x$axis.par), ]
      th <- second.thr
      abline(pars[1] + th * sqrt(pars[2]^2 + 1), pars[2],
             lty = 2, col = "black", lwd = 2)
      abline(pars[1] - th * sqrt(pars[2]^2 + 1), pars[2],
             lty = 2, col = "black", lwd = 2)
      legend("bottomright", c("Fixed threshold", "Normal threshold"),
             col = c("black", "red"), lwd = c(2, 2), lty = c(2, 1), cex = 1)
    }
    if (print.corr) {
      rho <- cor(x$Deltas[, 1], x$Deltas[, 2])
      legend(xl[1], yl[2], substitute(r[Delta] == x, list(x = round(rho,
                                                                    3))), bty = "n")
    }
  }
  internalDP()
  if (save.plot) {
    plotype <- NULL
    if (save.options[3] == "pdf")
      plotype <- 1
    if (save.options[3] == "jpeg")
      plotype <- 2
    if (is.null(plotype))
      cat("Invalid plot type (should be either 'pdf' or 'jpeg').",
          "\n", "The plot was not captured!", "\n")
    else {
      if (save.options[2] == "default")
        wd <- file.path(getwd())
      else wd <- save.options[2]
      nameF <- paste(save.options[1], switch(plotype, `1` = ".pdf",
                                             `2` = ".jpg"), sep = "")
      nameFile <- file.path(wd, nameF)
      if (plotype == 1) {
        {
          pdf(file = nameFile)
          internalDP()
        }
        dev.off()
      }
      if (plotype == 2) {
        {
          jpeg(filename = nameFile)
          internalDP()
        }
        dev.off()
      }
      cat("The plot was captured and saved into", "\n",
          " '", nameFile, "'", "\n", "\n", sep = "")
    }
  }
  else cat("The plot was not captured!", "\n", sep = "")
}


#################
#SERVER SCRIPT
#################
function(input, output,session) {

  ############################
  ### hits counter
  ############################
  output$counter <- renderText({
    if (!file.exists("counter.Rdata"))
      {counter <- 0}
    else {load(file = "counter.Rdata")}
    counter <- counter + 1
    save(counter, file = "counter.Rdata")
    paste0("Hits:", counter)
  })


  #################################
  #Reactive elements for future use
  #################################

  #load tests
   test_answers <- reactive ({
      ifelse (is.null(input$data),answ <- GMATtest[ , 1:20], {
        answ <- read.csv(input$data$datapath, header = input$header,
                         sep = input$sep, quote = input$quote)})
      answ
      })

   #load key
   test_key <- reactive({
     ifelse (is.null(input$key), k <-GMATkey, {
       k <- read.csv(input$key$datapath,header = FALSE)
       k <- k[[1]]
       })
    k
    })

   DIF_groups <- reactive({
     ifelse (is.null(input$key),k <- GMATtest[ , 21], {
       k <- read.csv(input$groups$datapath,header = TRUE)
       k <- k[[1]]})
      as.vector(k)
      })


   #scoring test calculates total score of each student
   scored_test <- reactive({
     sco <- score(test_answers(), test_key())$score
     sco
     })

   #correct answers assigns 1 and 0 to correct and incorect answers
   correct_answ <- reactive({
     correct <- score(test_answers(), test_key(), output.scored = TRUE)$scored
     correct
     })

   DPdata <- reactive ({
     dataset <- data.frame(correct_answ(), DIF_groups())
     colnames(dataset)[ncol(dataset)] <- 'group'
     dataset
     })

  #################
  #CHECK DATA
  #################
   output$headdata <- renderTable({
      head(test_answers())
     })

  ##################
  # KEY CONTROL
  ###################
  output$key <- renderTable({
    t(as.data.frame(test_key()))
  }, include.rownames = FALSE, include.colnames = FALSE)


  ####################
  # SCORE 0-1
  ####################
  output$sc01 <- renderTable({
    a <- test_answers()
    k <- test_key()

    sc <- data.frame(scored_test())
    colnames(sc) <- "Score"
    correct <- correct_answ()
    name <- c()
    for (i in 1:ncol(a)) {
      name[i] <- paste("i", i, sep = "")
    }
    colnames(correct) <- name

    out <- (cbind(correct,sc))
    out
  }, digits = 0)

  ########################
  #SLIDER FOR STUDENTS PAGE
  ########################
  output$slider2 <- renderUI({
    sliderInput(
      "inSlider2", "Cut-Score", min = 0, max = ncol(test_answers()),
      value = round(median(scored_test())), step = 1
    )
  })

  ########################
  #HISTOGRAM BY POINTS
  ########################
  output$histbyscore <- renderPlot ({

    a <- test_answers()
    k <- test_key()

    sc  <- scored_test()
    h   <- hist(sc, breaks = -1:ncol(a))
    bin <- as.numeric(cut(input$inSlider2,h$breaks))
    color <- rep(c("red", "grey", "blue"),
                 c(bin - 1, 1, length(h$breaks) - bin - 1))
    plot(h,
         col = color,
         main = "Histogram of Total Scores",
         xlab = "Total Score",
         ylab = "Number of Students",
         xlim = c(0, ncol(a))
    )
  })

  #########################
  #TABLE OF PERCENTILES BY SCORE
  #########################
  output$percentile <- renderTable({

    a  <- test_answers()
    k  <- test_key()
    sc <- scored_test()

    tabulka <-
      matrix(
        NA, ncol = dim(table(sc)), nrow = 4, dimnames = list(
          rownames = c("Percentile", "Succes Rate", "Z-score", "T-score"),
          colnames = names(table(sc))
        )
      )

    for (i in as.numeric(names(table(sc))))
    {
      tabulka["Percentile", as.character(i)] <-
        cumsum(prop.table(table(sc)))[as.character(i)]
      tabulka["Succes Rate", as.character(i)] <- (i / max(sc)) * 100
      tabulka["Z-score", as.character(i)] <- (i - mean(sc)) / sd(sc)
      tabulka["T-score", as.character(i)] <- 50 + 10 * (i - mean(sc)) / sd(sc)
    }
    t(round(tabulka, 2))

    tab <- cbind(names(table(sc)), t(round(tabulka, 2)))
    colnames(tab) <- c("Total Score", "Percentile", "Succes Rate", "Z-score", "T-score")
    tab
  }, include.rownames = FALSE)

  ##########################
  #PLOT OF DIFFICULTY
  ##########################
  output$difplot <- renderPlot({
    a <- test_answers()
    k <- test_key()

    correct <- correct_answ()

    difc <- item.exam(correct, discr = T)[, "Difficulty"]
    disc <- item.exam(correct, discr = T)[, "Discrimination"]

    value <- c(rbind(difc, disc)[, order(difc)])
    parameter <- rep(c("Difficulty", "Discrimination"), ncol(a))
    item <- factor(rep(order(difc), rep(2,ncol(a))),
                   levels = factor(order(difc)))

    pars.trad <- data.frame(item, parameter, value)

    ggplot(pars.trad, aes(x = item, y = value, fill = parameter),
           color = parameter) +
      stat_summary(fun.y = mean, position = position_dodge(), geom = "bar") +
      ylim(0, 1) +
      geom_hline(yintercept = 0.2) +
      xlab("Item number (ordered by difficulty)") +
      ylab("") +
      theme_bw() + geom_blank() +
      theme(legend.title = element_blank()) +
      theme(legend.position = c(0.85, 0.9)) +
      theme(text = element_text(size = 18),
            plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
            # panel.border = element_rect(linetype = "solid", colour = "black"),
            # plot.background = element_rect(colour = "black", size = 1))
            # axis.line  = element_line(colour = "black"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank())
  })

  output$uidifplot <- renderUI({
    plotOutput("difplot", height = 1000)})


  ########################
  #ITEM EXAM
  ########################
  output$itemexam <- renderTable({
    a <- test_answers()
    k <- test_key()

    correct <- correct_answ()
    alphadrop <- psych::alpha (correct)$alpha.drop[, 1]
    tab <- item.exam(correct, discr = TRUE)[, 1:5]
    tab <- cbind(c(1:ncol(test_answers())),
                 tab[, c(4, 1, 5, 2, 3)], alphadrop)
    colnames(tab) <- c("Item", "Difficulty", "SD", "Discrimination ULI",
                       "Discrimination RIT", "Discrimination RIR", "Alpha Drop")
    tab

  }, include.rownames = FALSE)

  #####################
  #Distractor explanation
  #####################
  output$distractor_expl <- renderUI({


    txt1 <- paste ('Respondents are divided into ')
    txt2 <- paste ("<b>", input$gr, "</b>")
    txt3 <- paste (
      "groups by their total score. Subsequently, we display percentage
      of students in each group who selected given answer (correct answer or distractor).
      The correct answer should be more often selected by strong students than by students
      with lower total score, i.e."
    )
    txt4 <- paste ("<b>",'solid line should be increasing.',"</b>")
    txt5 <- paste('The distractor should work in opposite direction, i.e. ')
    txt6 <- paste ("<b>",'dotted lines should be decreasing.',"<b>")
    HTML(paste(txt1, txt2, txt3, txt4, txt5, txt6))
  })

  ######################
  #DISTRACTOR ANALYSIS TABLES
  ######################
  output$contents <- renderPrint({
    a <- test_answers()
    k <- test_key()

    DistractorAnalysis(a, k, g = input$gr)
  })

  ######################
  #CLASSICAL
  ######################
  output$dist_group <- renderTable({
    scores <- scored_test()
    score.level <- quantile(scores, seq(0, 1, by = 1/input$gr))
    v <- c()
    l <- c()
    pomocna <- -1
    pocet   <- c()
    for (i in 1:input$gr){
      v[i] <- score.level[[i+1]]
      l[i] <- paste("Group", i, sep = " ")
      pocet [i] <- length(scores[scores <= v[i] & scores > pomocna])
      pomocna <- v[i]
    }
    tab <- rbind(l, v, pocet)
    rownames(tab) <- c('', 'Max Points', 'Count')
    tab
  }, include.colnames = FALSE)


  output$graf <- renderPlot({
    a <- test_answers()
    k <- test_key()

    plotDistractorAnalysis(a, k, g <- input$gr, k <- input$inSlider)
  })

  ############################
  #RESPONSIVE SLIDER FOR WITH MAX AS NUMBER OF ITEMS
  ############################
  output$slider <- renderUI({
    a <- test_answers()

    sliderInput(
      "inSlider", "Item", animate = TRUE, min = 1, max = ncol(a), value = 1, step = 1
    )
  })

  ########################
  #CLASSICAL TABLE
  ########################
  output$distr <- renderTable({
    a <- test_answers()
    k <- test_key()

    DistractorAnalysis(a, k, g = input$gr)[[input$inSlider]]
  })

  ########################
  #CLASSICAL P TABLE
  ########################
  output$distrproc <- renderTable({
    a <- test_answers()
    k <- test_key()

    DistractorAnalysis(a, k, g = input$gr, p.table = TRUE)[[input$inSlider]]
  })

  ###################
  #CLASSICAL ALL
  ###################
  output$plots <- renderUI({
    a <- test_answers()
    k <- test_key()

    plot_output_list <- lapply(1:ncol(a), function(i) {
      plotname <- paste("plot", i, sep = "")
      plotOutput(plotname, height = 350, width = 600)
    })
    do.call(tagList, plot_output_list)
  })

  for (i in 1:300) {
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep = "")

      output[[plotname]] <- renderPlot({
        a <- test_answers()
        k <- test_key()
        plotDistractorAnalysis(a, k, g <- input$gr, k <- my_i)
      })
    })
  }

  #########################
  #RESULTS
  #########################
  #descriptive statistics
  output$results <- renderTable({

    sc <- scored_test()
    res <- rbind(c("Min", "Max", "Mean", "Median", "SD", "Skewness", "Kurtosis"),
                 round(c(min(sc), max(sc), mean(sc), median(sc), sd(sc),
                         skewness(sc), kurtosis(sc)), 2))
  }, digits = 2, include.rownames = FALSE, include.colnames = FALSE)

  ######################
  #LOGREG GRAPH
  ######################
  logistic_reg <- reactive ({
    model <- glm(correct_answ()[, input$inSlider] ~ scored_test(), family = binomial)
  })

  output$logreg <- renderPlot({
    sc <- scored_test()
    correct <- correct_answ()
    b0 = summary(logistic_reg())$coef[1, 1]
    b1 = summary(logistic_reg())$coef[2, 1]


    plot(
      x = levels(as.factor(sc)), y = tapply(correct[, input$inSlider], sc, mean),
      xlab = "Total score",
      ylab = "Probability of correct answer",
      main = paste("Item", input$inSlider)
    )
    curve(exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x)), add = T, col = "forestgreen")
  })

  ######################
  #LOGREG TABLE
  ######################
  output$logregtab <- renderTable({

    tab <- summary(logistic_reg())$coef[1:2, 1:2]
    colnames(tab) <- c("Estimate", "SD")
    rownames(tab) <- c("b0", "b1")
    tab
  })

  ######################
  #LOGREG INTERPRETATION
  ######################

  output$logisticint <- renderUI({

    b1 <- summary(logistic_reg())$coef[2, 1]
    b1 <- round(b1, 2)
    txt1 <- paste ("<b>", "Interpretation:","</b>")
    txt2 <- paste (
      "A one-unit increase in the total
      score is associated with the increase in the log
      odds of answering the item correctly
      vs. not correctly in the amount of"
    )
    txt3 <- paste ("<b>", b1, "</b>")
    HTML(paste(txt1, txt2, txt3))
  })

  ########################
  #LOGISTIC EQUATION
  ########################


  ######################
  #EQUATION PLAIN
  ######################


  ######################
  #Z-LOGREG GRAPH
  ######################
  z_logistic_reg <- reactive({
    scaledsc <- scale(scored_test())
    model    <- glm(correct_answ()[, input$inSlider] ~ scaledsc, family = binomial)
  })

  output$zlogreg <- renderPlot({
    scaledsc = scale(scored_test())

    b0 = summary(z_logistic_reg())$coef[1, 1]
    b1 = summary(z_logistic_reg())$coef[2, 1]

    plot(
      x = levels(as.factor(scaledsc)), y = tapply(correct_answ()[, input$inSlider], scaledsc, mean),
      xlab = "Standardized total score (Z-score)",
      ylab = "Probability of correct answer", main = paste("Item", input$inSlider)
    )
    curve(exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x)), add = T, col = "forestgreen")

  })


  ######################
  #z-LOGREG TABLE
  ######################
  output$zlogregtab <- renderTable({

    tab <- summary(z_logistic_reg())$coef[1:2, 1:2]
    colnames(tab) <- c("Estimate", "SD")
    rownames(tab) <- c("b0", "b1")
    tab

  })
  ######################
  #z-LOGREG INTERPRETATION
  ######################
  output$zlogisticint <- renderUI({

    b1 <- summary(z_logistic_reg())$coef[2, 1]
    b1 <- round(b1, 2)
    b0 <- round(summary(z_logistic_reg())$coef[1, 1], 2)

    txt1 <- paste ("<b>", "Interpretation:", "</b>")
    txt2 <-
      paste (
        "A one-unit increase in the z-score (one SD increase in original scores)
        is associated with the increase in the log
        odds of answering the item correctly
        vs. not correctly in the amount of"
      )
    txt3 <- paste ("<b>", b1, "</b>")
    HTML(paste(txt1, txt2, txt3))
  })


  ######################
  #Z-LOGREG IRT GRAPH
  ######################


  output$zlogreg_irt <- renderPlot({
    scaledsc = scale(scored_test())

    b0 = summary(z_logistic_reg())$coef[1, 1]
    b1 = summary(z_logistic_reg())$coef[2, 1]

    plot(
      x = levels(as.factor(scaledsc)), y = tapply(correct_answ()[, input$inSlider], scaledsc, mean),
      xlab = "Standardized total score (Z-score)",
      ylab = "Probability of correct answer", main = paste("Item", input$inSlider)
    )
    curve(exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x)), add = T, col = "forestgreen")

  })


  ######################
  #z-LOGREG IRT TABLE
  ######################
  output$zlogregtab_irt <- renderTable({

    tab <- as.table(summary(z_logistic_reg())$coef[1:2, 1])

    
    varfit <- vcov(z_logistic_reg())
    delfit <- sqrt(c(c(-1/tab[2], tab[1]/tab[2]^2) %*% varfit %*% c(-1/tab[2], tab[1]/tab[2]^2), varfit[2, 2]))
    
    est <- c(-tab[1]/tab[2], tab[2])
    
    est <- cbind(est, delfit)
    est <- est[2:1, ]
    colnames(est) <- c("Estimate", "SD")
    rownames(est) <- c("a", "b")
    est

  }, include.rownames = T)
  ######################
  #z-LOGREG IRT INTERPRETATION
  ######################
  output$zlogisticint_irt <- renderUI({

    b1 <- summary(z_logistic_reg())$coef[2, 1]
    b1 <- round(b1, 2)
    b0 <- round(summary(z_logistic_reg())$coef[1, 1], 2)

    txt1 <- paste ("<b>", "Interpretation:", "</b>")
    txt2 <-
      paste (
        "A one-unit increase in the z-score (one SD increase in original scores)
        is associated with the increase in the log
        odds of answering the item correctly
        vs. not correctly in the amount of"
      )
    txt3 <- paste ("<b>", b1, "</b>")
    HTML(paste(txt1, txt2, txt3))
  })

  ######################
  #NLS PLOT
  ######################
  nls_model <- reactive({
    regFce_noDIF <- deriv3(
      ~ c + (1 - c) / (1 + exp(-a * (x - b))),
      namevec = c("a", "b", "c"),
      function.arg = function(x, a, b, c) {
      }
    )

    regFce_klasik <-
      deriv3(
        ~ c + (1 - c) / (1 + exp(-(b0 + b1 * x))),
        namevec = c("b0", "b1", "c"),
        function.arg = function(x, b0, b1, c) {
        }
      )

    scaledsc = c(scale(scored_test()))

    Q3 <- cut(scaledsc, quantile(scaledsc, (0:3) / 3),
              c("I", "II", "III"),
              include.lowest = TRUE)

    x <-
      cbind(mean(scaledsc[Q3 == "I"]), apply(correct_answ()[Q3 == "I",], 2, mean))
    y <-
      cbind(mean(scaledsc[Q3 == "III"]), apply(correct_answ()[Q3 == "III",], 2, mean))
    u1 <- y[, 1] - x[, 1]
    u2 <- y[, 2] - x[, 2]
    ### intercept of line
    c <- -(-u1 * y[, 2] + u2 * y[, 1]) / u1
    ### slope of line
    t <- u2 / u1
    g <- apply(cbind(0, t * (-4) + c), 1, max)

    b <- ((1 + g) / 2 - c) / t

    alpha <- 4 * t / (1 - g)

    discr <- alpha
    diffi <- b
    guess <- g
    i <- input$inSlider

    start <- cbind(discr, diffi, guess)
    colnames(start) <- c("a", "b", "c")

    estim_klasik1 <-
      nls(
        correct_answ()[, i] ~ regFce_noDIF(scaledsc, a, b, c),
        algorithm = "port", start = start[input$inSlider,],
        lower = c(-20,-20, 0), upper = c(20, 20, 1)
      )

    estim_klasik1
  })


  output$nlsplot <- renderPlot({

    regFce_klasik <-
      deriv3(
        ~ c + (1 - c) / (1 + exp(-a * (x - b))),
        namevec = c("a", "b", "c"),
        function.arg = function(x, a, b, c) {
        }
      )
    scaledsc = c(scale(scored_test()))

    plot(
      x = levels(as.factor(scaledsc)),
      y = tapply(correct_answ()[, input$inSlider], as.factor(scaledsc), mean),
      xlab = "Standardized total score (Z-score)", ylab = "Probability of correct answer",
      main = paste("Item", input$inSlider)
    )
    lines(xx <-
            seq(-3, 3, length = 81), regFce_klasik(
              xx, coef(nls_model())[1],
              coef(nls_model())[2],
              coef(nls_model())[3]
            ), col = "forestgreen")

  })
  ######################
  #NLS TABULKA
  ######################
  output$nonlinearztab <- renderTable({

    tabulka <- summary(nls_model())$parameters[, 1:2]
    colnames(tabulka) <- c("Estimate", "SD")
    tabulka
  })

  #####################
  #NLS INTERPRETATION
  #####################

  output$nonlinearint <- renderUI({

    a <- summary(nls_model())$coef[1, 1]
    a <- round(a, 2)
    b <- round(summary(nls_model())$coef[2, 1], 2)

    txt1 <- paste ("<b>", "Interpretation:", "</b>")
    txt2 <- paste (
        "A one-unit increase in the z-score (one SD increase in original scores) is associated
        with the increase in the log odds of answering the item correctly vs. not correctly
        in the amount of "
      )
    txt3 <- paste ("<b>", a, "</b>")
    HTML(paste(txt1, txt2, txt3))

  })

  ######################
  #MULTINOMIAL
  ######################
  multinomial_model <- reactive({
    stotal <- c(scale(scored_test()))
    k      <- t(as.data.frame(test_key()))
    fitM   <- multinom(relevel(as.factor(test_answers()[, input$inSlider]),
                               ref = k[input$inSlider]) ~ stotal)
    fitM
  })

  output$multiplot <- renderPlot({
    stotal  <- c(scale(scored_test()))
    pp      <- fitted(multinomial_model())
    stotals <- rep(stotal, length(levels(as.factor(test_answers()[, input$inSlider]))))
    d       <- cbind(melt(pp), stotals)
    print(
      ggplot(d, aes(x = stotals , y = value, colour = Var2)) +
        geom_line() +
        ylim(0,1) +
        labs(title = paste("Item", input$inSlider),
             x = "Standardized total score",
             y = "Probability of correct answer") +
        theme_bw() + geom_blank() +
        theme(legend.title = element_blank()) +
        theme(text = element_text(size = 18),
              plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
    )
  })

  output$multieq <- renderUI ({
    cor_option <- test_key()[input$inSlider]
    withMathJax(
      sprintf(
        '$$\\mathrm{P}(Y = i|Z, b_{i0}, b_{i1}) =
        \\mathrm{P}(Y = %s|Z, b_{%s0}, b_{%s1})\\cdot e^{\\left( b_{i0} + b_{i1} Z\\right) }, \\\\ \\text{where i is one of the wrong options and %s is the correct one.}$$'
          ,cor_option, cor_option, cor_option, cor_option
      )
    )
  })

  ######################
  #MULTINOMIAL TABLE
  ######################
  output$multitab <- renderTable({

    koef <- summary(multinomial_model())$coefficients
    std  <- summary(multinomial_model())$standard.errors
    tab  <- cbind(koef, std)
    colnames(tab) <-
      c("Intercept", "Score", "SD Intercept", "SD Score")
    tab
  })

  ######################
  #MULTINOMIAL INTERPRETATION
  ######################
  output$multiint <- renderUI({

    koef <- summary(multinomial_model())$coefficients
    txt  <- c()

    for (i in 1:nrow(koef)){
    txt[i] <- paste (
      "A one-unit increase in the z-score (one SD increase in original
        scores)  is associated with the decrease in the log odds of
        answering the item "
    ,"<b>", row.names(koef)[i], "</b>", "vs.", "<b>", test_key()[input$inSlider],
    "</b>","in the amount of ",
    "<b>", round(koef[i, 2], 2), "</b>", '<br/>')
    }
  HTML(paste(txt))
  })


  ######################
  #RASCH
  ######################
  rasch_model <- reactive({
    fitRasch <- rasch(correct_answ())
  })


  output$rasch <- renderPlot({
    plot(rasch_model())
  })

  ######################
  #RASCH IIC
  ######################
  output$raschiic <- renderPlot({
    plot(rasch_model(), type = "IIC")
  })

  ######################
  #RASCH TIF
  ######################
  output$raschtif <- renderPlot({
    plot(rasch_model(),items = 0, type = "IIC")
  })

  ######################
  #RASCH COEFFICIENTS
  ######################
  output$raschcoef <- renderTable({

    tabulka <- coef(rasch_model())
    colnames(tabulka) <- c("b", "a")
    tabulka
  })

  #####################
  #RASCH FACTOR
  #####################
  output$raschFactor <- renderPlot({
    fit1 <- rasch_model()
    df1  <- ltm::factor.scores(fit1, return.MIvalues = T)$score.dat
    FS   <- as.vector(df1[, "z1"])
    df2  <- df1
    df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
    STS <- as.vector(scale(apply(df2, 1, sum)))
    df  <- data.frame(FS, STS)


    ggplot(df, aes_string("STS", "FS")) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_bw() +
      theme(text = element_text(size = 18),
            plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
            axis.line  = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      theme(legend.box.just = "left",
            legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.margin = unit(0, "lines"),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  ######################
  #2PARAM
  ######################
  two_param_irt <- reactive({
    fit2PL <- ltm(correct_answ() ~ z1, IRT.param = TRUE)
  })

  output$twoparam <- renderPlot({
    plot(two_param_irt())
  })

  ######################
  #2PARAM IIC
  ######################
  output$twoparamiic <- renderPlot({
    plot(two_param_irt(), type = "IIC")
  })

  ######################
  #2PARAM TIF
  ######################
  output$twoparamtif <- renderPlot({
    plot(two_param_irt(), items = 0, type = "IIC")
  })

  ######################
  #2PARAM COEFFICIENTS
  ######################
  output$twoparamcoef <- renderTable({
    tabulka <- coef(two_param_irt())
    colnames(tabulka) <- c("b", "a")
    tabulka
  })

  #####################
  #2PARAM FACTOR SCORE
  #####################
  output$twoFactor <- renderPlot({
    fit2 <- two_param_irt()
    df1  <- ltm::factor.scores(fit2, return.MIvalues = T)$score.dat
    FS   <- as.vector(df1[, "z1"])
    df2  <- df1
    df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
    STS  <- as.vector(scale(apply(df2, 1, sum)))
    df   <- data.frame(FS, STS)

    ggplot(df, aes_string("STS", "FS")) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_bw() +
      theme(text = element_text(size = 18),
            plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
            axis.line  = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      theme(legend.box.just = "left",
            legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.margin = unit(0, "lines"),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  ######################
  #3PARAM
  ######################
  three_param_irt <- reactive({
    fit3PL <- tpm(correct_answ(), IRT.param = TRUE)
  })

  output$threeparam <- renderPlot({
    plot(three_param_irt())
  })

  ######################
  #3PARAM IIC
  ######################
  output$threeparamiic <- renderPlot({
    plot(three_param_irt(), type = "IIC")
  })

  ######################
  #3PARAM TIF
  ######################
  output$threeparamtif <- renderPlot({
    plot(three_param_irt(), items = 0, type = "IIC")
  })

  ######################
  #3PARAM COEFFICIENTS
  ######################
  output$threeparamcoef <- renderTable({
    tab <- coef(three_param_irt())
    colnames(tab) <- c("c", "b", "a")
    tab
  })

  ######################
  #3PARAM FACTOR SCORE
  ######################
  output$threeFactor <- renderPlot({
    fit3 <- three_param_irt()
    df1  <- ltm::factor.scores(fit3, return.MIvalues = T)$score.dat
    FS   <- as.vector(df1[, "z1"])
    df2  <- df1
    df2$Obs <- df2$Exp <- df2$z1 <- df2$se.z1 <- NULL
    STS  <- as.vector(scale(apply(df2, 1, sum)))
    df   <- data.frame(FS, STS)

    ggplot(df, aes_string("STS", "FS")) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_bw() +
      theme(text = element_text(size = 18),
            plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
            axis.line  = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      theme(legend.box.just = "left",
            legend.justification = c(1, 0),
            legend.position = c(1, 0),
            legend.margin = unit(0, "lines"),
            legend.box = "vertical",
            legend.key.size = unit(1, "lines"),
            legend.text.align = 0,
            legend.title.align = 0)
  })

  ######################
  #Groups
  ######################
  output$resultsgroup <- renderTable({
    sc_one  <- scored_test()[DIF_groups() == 1]
    sc_zero <- scored_test()[DIF_groups() == 0]
    res <- rbind(c("Group", "Min", "Max", "Mean", "Median", "SD", "Skewness", "Kurtosis"),
                 round(c(1, min(sc_one), max(sc_one), mean(sc_one), median(sc_one),
                         sd(sc_one), skewness(sc_one), kurtosis(sc_one)), 2),
                 round(c(0, min(sc_zero), max(sc_zero), mean(sc_zero), median(sc_zero),
                         sd(sc_zero), skewness(sc_zero), kurtosis(sc_zero)), 2))
  }, digits = 2, include.rownames = FALSE, include.colnames = FALSE)


  output$slider2group <- renderUI({
    sliderInput(
      "inSlider2group", "Cut-Score", min = 0, max = ncol(test_answers()),
      value = round(median(scored_test()[DIF_groups() == 1])), step = 1
    )
  })

  ########################
  #HISTOGRAM BY POINTS FOR GROUPS
  ########################
  output$histbyscoregroup1 <- renderPlot ({

    a <- test_answers()
    k <- test_key()

    sc  <- scored_test()[DIF_groups() == 1]
    h   <- hist(sc, breaks = -1:ncol(a))
    bin <- as.numeric(cut(input$inSlider2group, h$breaks))
    color <- rep(c("red", "grey", "blue"), c(bin - 1, 1, length(h$breaks) - bin - 1))

    plot(
      h, col = color,
      main = "Histogram of Total Scores Group = 1",
      xlab = "Total Score",
      ylab = "Number of Students",
      xlim = c(0, ncol(a))
    )
  })
  #######################################
  output$histbyscoregroup0 <- renderPlot ({

    a <- test_answers()
    k <- test_key()

    sc  <- scored_test()[DIF_groups() == 0]
    h   <- hist(sc, breaks = -1:ncol(a))
    bin <- as.numeric(cut(input$inSlider2group,h$breaks))
    color <- rep(c("red", "grey", "blue"), c(bin - 1, 1, length(h$breaks) - bin - 1))

    plot(
      h, col = color, main = "Histogram of Total Scores Group = 0",
      xlab = "Total Score",
      ylab = "Number of Students",
      xlim = c(0, ncol(a))
    )
  })

  #######################
  #DeltaPlot
  #######################
  deltaGpurn <- reactive({
    deltaPlot(DPdata(), group = "group",
              focal.name = 1, purify = T, thr = "norm")
  })

  deltaGpur <- reactive({
    deltaPlot(DPdata(), group = "group",
              focal.name = 1, purify = T, thr = 1.5)
  })

  output$deltaplot <- renderPlot({
    difDP(deltaGpurn(), xlab = "Men", ylab = "Women",
          thr.draw = T, second.thr = 0, pch = 16, xlim = c(8, 16), ylim = c(8, 16))
    text(deltaGpur()$Deltas[, 1],  deltaGpur()$Deltas[, 2], labels = 1:20, cex = 0.8, pos = 3)
  })

  output$dp_text_normal <- renderPrint({
    deltaGpurn()
  })

  ########################
  #DIF logistic
  ########################
  printLR <- function(Fval, Pval, type.of.test){
    m <- length(Fval)
    title <- switch(type.of.test,
                    both = paste("Detection of both types of Differential Item Functioning using
                             logistic regression method with Benjamini-Hochberg correction method"),
                    udif  = paste("Detection of uniform Differential Item Functioning using
                              logistic regression method with Benjamini-Hochberg correction method"),
                    nudif = paste("Detection of non-uniform Differential Item Functioning using
                              logistic regression method with Benjamini-Hochberg correction method"))
    cat(paste(strwrap(title, width = 60), collapse = "\n"))
    cat("\n\nLinear regression DIF statistic:\n")
    ### table of Fvalues and pvalues
    tab  <- format(round(cbind(Fval, Pval), 4))
    sign <- ifelse(Pval < 0.001, "***",
                   ifelse(Pval < 0.01, "**",
                          ifelse(Pval < 0.05, "*",
                                 ifelse(Pval < 0.1, ".", " "))))
    tab <- matrix(cbind(tab, sign), ncol = 3)
    rownames(tab) <- paste("Item", 1:m)
    colnames(tab) <- c("Statistic", "P-value", "")
    print(tab, quote = F, digits = 4, zero.print = F)
    cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
    DIF <- which(Pval < 0.05)
    if (length(DIF) == 0){
      switch(type.of.test,
             both  = cat("\nNone of items is detected as DIF"),
             udif   = cat("\nNone of items is detected as uniform DIF"),
             nudif = cat("\nNone of items is detected as non-uniform DIF"))
    } else {
      switch(type.of.test,
             both  = cat("\nItems detected as DIF items:\n"),
             udif   = cat("\nItems detected as uniform DIF items:\n"),
             nudif = cat("\nItems detected as non-uniform DIF items:\n"))
      cat("\n", paste("Item ", DIF, "\n", sep = ""))
    }
  }


  output$Log_vto <- renderPrint({
    TS    <- scored_test()
    data  <- correct_answ()
    group <- DIF_groups()

    pval_LR_DIF    <- pval_LR_nonuDIF <- pval_LR_uDIF <- c()
    fit1coef       <- fit2coef <- fit3coef <- c()
    testval_LR_DIF <- testval_LR_nonuDIF <- testval_LR_uDIF <- c()

    type = input$type_summary_logistic

    for (i in 1:ncol(data)){
      fit1 <- glm(data[, i] ~ TS + group + TS:group, family = "binomial")
      fit1coef <- rbind(fit1coef, fit1$coefficients)
      fit2 <- glm(data[, i] ~ TS + group, family = "binomial")
      fit2coef <- rbind(fit2coef, fit2$coefficients)
      fit3 <- glm(data[, i] ~ TS, family = "binomial")
      fit3coef <- rbind(fit3coef, fit3$coefficients)

      ### any DIF
      anov_LR_DIF <- anova(fit1, fit3, test = "LR")
      pval_LR_DIF <- as.vector(c(pval_LR_DIF, as.numeric(anov_LR_DIF[[5]])[2]))
      testval_LR_DIF <- as.vector(c(testval_LR_DIF, as.numeric(anov_LR_DIF[[4]])[2]))

      ### nonuniform DIF
      anov_LR_nonuDIF <- anova(fit1, fit2, test = "LR")
      pval_LR_nonuDIF <- as.vector(c(pval_LR_nonuDIF, as.numeric(anov_LR_nonuDIF[[5]])[2]))
      testval_LR_nonuDIF <- as.vector(c(testval_LR_nonuDIF, as.numeric(anov_LR_nonuDIF[[4]])[2]))

      ### uniform DIF
      anov_LR_uDIF <- anova(fit2, fit3, test = "LR")
      pval_LR_uDIF <- as.vector(c(pval_LR_uDIF, as.numeric(anov_LR_uDIF[[5]])[2]))
      testval_LR_uDIF <- as.vector(c(testval_LR_uDIF, as.numeric(anov_LR_uDIF[[4]])[2]))
    }

    p.adjust.method <- "BH"

    pval_LR_DIF_adj <- p.adjust(pval_LR_DIF, method = "BH")
    DIFitem_DIF <- which(pval_LR_DIF_adj < 0.05)

    pval_LR_nonuDIF_adj <- p.adjust(pval_LR_nonuDIF, method = "BH")
    DIFitem_nonuDIF <- which(pval_LR_nonuDIF_adj < 0.05)

    pval_LR_uDIF_adj <- p.adjust(pval_LR_uDIF, method = "BH")
    DIFitem_uDIF <- which(pval_LR_uDIF_adj < 0.05)

    DIFitem <- switch(type,
                      both  = DIFitem_DIF,
                      nudif = DIFitem_nonuDIF,
                      udif  = DIFitem_uDIF)
    Fval <- switch(type,
                   both  = testval_LR_DIF,
                   nudif = testval_LR_nonuDIF,
                   udif  = testval_LR_uDIF)

    Pval <- switch(type,
                   both  = pval_LR_DIF_adj,
                   nudif = pval_LR_nonuDIF_adj,
                   udif  = pval_LR_uDIF_adj)

    printLR(Fval,Pval,input$type_summary_logistic)
  })


  LRcoef <- reactive({
    TS   <- (scored_test() - mean(scored_test()))/sd(scored_test())

    pval_LR_DIF <- pval_LR_nonuDIF <- pval_LR_uDIF <- c()
    fit1coef <- fit2coef <- fit3coef <- c()

    type = input$type_plot_logistic

    data  <- correct_answ()
    group <- DIF_groups()

    for (i in 1:ncol(data)){
      fit1 <- glm(data[, i] ~ TS + group + TS:group, family = "binomial")
      fit1coef <- rbind(fit1coef, fit1$coefficients)
      fit2 <- glm(data[, i] ~ TS + group, family = "binomial")
      fit2coef <- rbind(fit2coef, fit2$coefficients)
      fit3 <- glm(data[, i] ~ TS, family = "binomial")
      fit3coef <- rbind(fit3coef, fit3$coefficients)

      ### any DIF
      anov_LR_DIF <- anova(fit1, fit3, test = "LR")
      pval_LR_DIF <- as.vector(c(pval_LR_DIF, as.numeric(anov_LR_DIF[[5]])[2]))

      ### nonuniform DIF
      anov_LR_nonuDIF <- anova(fit1, fit2, test = "LR")
      pval_LR_nonuDIF <- as.vector(c(pval_LR_nonuDIF, as.numeric(anov_LR_nonuDIF[[5]])[2]))

      ### uniform DIF
      anov_LR_uDIF <- anova(fit2, fit3, test = "LR")
      pval_LR_uDIF <- as.vector(c(pval_LR_uDIF, as.numeric(anov_LR_uDIF[[5]])[2]))
    }
    ### metoda pro multiple comparison correction, muze byt jako volba uzivatele
    p.adjust.method <- "BH"

    ### any DIF
    pval_LR_DIF_adj <- p.adjust(pval_LR_DIF, method = "BH")
    DIFitem_DIF <- which(pval_LR_DIF_adj < 0.05)

    ### nonuniform DIF
    pval_LR_nonuDIF_adj <- p.adjust(pval_LR_nonuDIF, method = "BH")
    DIFitem_nonuDIF <- which(pval_LR_nonuDIF_adj < 0.05)

    ### uniform DIF
    pval_LR_uDIF_adj <- p.adjust(pval_LR_uDIF, method = "BH")
    DIFitem_uDIF <- which(pval_LR_uDIF_adj < 0.05)

    DIFitem <- switch(type,
                      both  = DIFitem_DIF,
                      nudif = DIFitem_nonuDIF,
                      udif  = DIFitem_uDIF)


    LR_coef             <- switch(type,
                                  both  = cbind(fit3coef, 0, 0),
                                  nudif = cbind(fit2coef, 0),
                                  udif  = cbind(fit3coef, 0, 0))

    LR_coef[DIFitem, ]  <- switch(type,
                                  both  = fit1coef[DIFitem, ],
                                  nudif = fit1coef[DIFitem, ],
                                  udif  = c(fit2coef[DIFitem, ],0) )
    LR_coef

  })


  ###############################
  #Plot
  ###############################

  output$logistic_DIF_plot <- renderPlot({

    group <- DIF_groups()
    data <- correct_answ()

    LR_plot <- function(TS, group, beta0, beta1, beta2, beta3){
      return(1/(1 + exp(-(beta0 + beta1*TS + beta2*group + beta3*TS*group))))
    }

    ### data
    TS_R <- apply(correct_answ()[group == 0, ], 1, sum)
    TS_R <- (TS_R - mean(TS_R))/sd(TS_R)
    TS_F <- apply(correct_answ()[group == 1, ], 1, sum)
    TS_F <- (TS_F - mean(TS_F))/sd(TS_F)
    max_TS <- max(TS_R, TS_F)
    min_TS <- min(TS_R, TS_F)

    col   <- c("tomato", "turquoise")
    alpha <- .5
    shape <-  21
    size  <- .8
    linetype <- c(1, 2)

    i <- input$inSlider

      hv_R <- data.frame(cbind(as.numeric(levels(as.factor(TS_R))),
                               tapply(data[group == 0, i], as.factor(TS_R), mean)))
      hv_F <- data.frame(cbind(as.numeric(levels(as.factor(TS_F))),
                               tapply(data[group == 1, i], as.factor(TS_F), mean)))
      hv   <- data.frame(rbind(cbind(hv_R, Group = "Reference"), cbind(hv_F, Group = "Focal")))
      rownames(hv) <- 1:dim(hv)[1]
      hv$size <- c(table(TS_R), table(TS_F))
      plot_CC <- ggplot(hv, aes(X1, X2)) +
        ### points
        geom_point(aes(colour = Group, fill = Group,
                       size = size),
                   alpha = alpha, shape = shape) +
        ### lines
        stat_function(aes(colour = "Reference"),
                      fun = function(xx) LR_plot(xx, 0, LRcoef()[i, 1], LRcoef()[i, 2], LRcoef()[i, 3], LRcoef()[i, 4]),
                      size = size, linetype = linetype[1], geom = "line") +
        stat_function(aes(colour = "Focal"),
                      fun = function(xx) LR_plot(xx, 1, LRcoef()[i, 1], LRcoef()[i, 2], LRcoef()[i, 3], LRcoef()[i, 4]),
                      size = size, linetype = linetype[2], geom = "line")  +
        ### style
        scale_size_continuous(name = "Counts")  +
        scale_colour_manual(breaks = hv$Group, values = col) +
        scale_fill_manual(values = col) +
        scale_linetype_manual(values = linetype,
                              guide = FALSE) +
        ### theme
        ggtitle(paste("Item", i)) +
        labs(x = "Standardized total Score", y = "Probability of correct answer") +
        ylim(c(0, 1)) + theme_bw() +
        theme(text = element_text(size = 18),
              plot.title = element_text(size = 18, face = "bold", vjust = 1.5),
              axis.line  = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank()) +
        ### legend
        theme(legend.box.just = "left",
              legend.justification = c(1, 0),
              legend.position = c(1, 0),
              legend.margin = unit(0, "lines"),
              legend.box = "vertical",
              legend.key.size = unit(1, "lines"),
              legend.text.align = 0,
              legend.title.align = 0)
      print(plot_CC)




  })

  output$LRcoefDIF <- renderTable({
    coefs <- LRcoef()
    colnames(coefs) <- c('b0', 'b1', 'b2', 'b3')
    koef_old <- t(as.table(coefs[input$inSlider, ]))
    koef <- koef_old
    colnames(koef) <- c('a', 'b', 'aDIF', 'bDIF')
    koef[1,1] <- koef_old[1, 2]
    koef[1,2] <- -(koef_old[1, 1]/koef_old[1, 2])
    koef[1,3] <- koef_old[1, 4]
    koef[1,4] <- -(koef_old[1, 3] + (koef_old[1, 4] * (-koef_old[1, 1] / koef_old[1, 2]))) /
      (koef_old[1, 2] + koef_old[1, 4])
    koef
  }, include.rownames = FALSE)

  #######################################
  #NLR DIF
  #######################################
   model_summary_NLR <- reactive({
     group <- DIF_groups()
     data <- correct_answ()
     mod <- difNLR(data = data,group = group, type = input$type_summary_NLR)
     mod
   })

   output$NLR_sum_verbatim <- renderPrint({
     print(model_summary_NLR())
   })

   model_plot_NLR <- reactive({
     group <- DIF_groups()
     data <- correct_answ()
     mod <- difNLR(data = data,group = group, type = input$type_plot_NLR)
     mod
   })

   output$plotNLR_DIF <- renderPlot({
     plot(model_plot_NLR(),item = input$inSlider)
   })

   output$coef_tab_NLR <- renderTable({
     t(as.table(model_plot_NLR()$coef[input$inSlider, ]))
   }, include.rownames = FALSE)


#    lordpr <- reactive({
#         difLord(Data = correct_answ(), group = DIF_groups(),
#                 focal.name = 1,
#                 model = input$model_lord)
#       })
# 
#    output$Lord_vto <- renderPrint({
#      validate(
#        need(is(try(lordpr(), silent = T), "try-error") != T, "Data could not be fitted.")
#      )
#      lordpr()
#    })
# 
#    rajupr <- reactive ({
#      difRaju(Data = correct_answ(), group = DIF_groups(),
#              focal.name = 1, model = input$model_raju)
#    })
#    output$Raju_vto <- renderPrint({
#      validate(
#        need(is(try(rajupr(), silent = T), "try-error") != T, "Data could not be fitted.")
#      )
#      rajupr()
#    })

}
