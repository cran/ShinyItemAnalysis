fluidPage(titlePanel("TEST AND ITEM ANALYSIS"),
          #####################
          #SIDE PANEL UPLOADS
          #####################
          sidebarLayout(
            sidebarPanel(
              uiOutput("slider"),

              p("ShinyItemAnalysis Version 0.2"),
              # p('Older version 0.1 ',
              #   a("is available here.", href = "https://shiny.cs.cas.cz/ShinyItemAnalysisV01/",
              #     target = "_blank")),
              p(" Project was supported by grant funded by Czech Science Foundation under number ",
                a("GJ15-15856Y",
                  href = "http://www.cs.cas.cz/martinkova/psychometrics.html",
                  target = "_blank")),
              # p(" Download ShinyItemAnalysis R package from ",
              #   a("CRAN", href = "https://cran.rstudio.com/web/packages/ShinyItemAnalysis/",
              #     target = "_blank")),
              p(" Copyright 2016  Patricia Martinkova, Ondrej Leder and Adela Drabinova"),
              # p(textOutput('counter')),
              p("Try ShinyItemAnalysis ",
                a("online.", href = "https://shiny.cs.cas.cz/ShinyItemAnalysis", target = "_blank")),

              width = 3
              ),
            #########################
            #MAIN PANEL
            #########################

            mainPanel(
              tabsetPanel(

                ########################
                #SUMMARY
                ########################
                tabPanel(
                  "Summary",
                  tabsetPanel(
                    tabPanel("Total Scores",
                  br(),
                  p('ShinyItemAnalysis provides analysis of educational tests (such as admission tests)
                    and its items. For demonstration purposes, 20-item dataset',strong('GMAT'),' from
                    R ', code('library(difNLR)'),' is used. You can change the dataset
                    (and try your own one) on page',strong('Data.')),
                  h3("Analysis of Total Scores"),
                  h4("Summary Table"),
                  tableOutput('results'),
                  h4("Histogram of Total Score"),
                  uiOutput("slider2"),
                  p('For selected cut-score, blue part of histogram shows students with total score
                    above the cut-score, grey column shows students with Total Score equal
                    to cut-score and red part of histogram shows students below the cut-score.'),
                  plotOutput('histbyscore')),
                  tabPanel("Standard Scores",
                           h4("Table by Score"),
                           tableOutput('percentile'),
                           p(""))
                  )
                ),


                ######################
                #ITEM ANALYSIS
                ######################
                tabPanel('Traditional Analysis',
                tabsetPanel(
                tabPanel(
                  "Item Analysis",
                  br(),
                  h3("Traditional Item Analysis"),
                  p('Traditional item analysis uses proportions of correct answers or correlations to estimate item properties.'),
                  h4("Item Difficulty/Discrimination Graph"),
                  p("Displayed is difficulty (red) and discrimination (blue)
                    for all items. Items are ordered by difficulty. "),
                  p(strong("Difficulty"),' of items is estimated as percent of students who answered correctly to that item.'),
                  p(strong("Discrimination"),' is described by difference of percent correct
                    in upper and lower third of students (Upper-Lower Index, ULI). By rule of thumb it should not be lower than 0.2
                    (borderline in the plot), except for very easy or very difficult items.'),

                  plotOutput('difplot'),

                  #uiOutput('uidifplot'),

                  h4("Traditional Item Analysis Table"),
                  p(strong('Explanation:Difficulty'),'Difficulty of item is estimated as percent
                    of students who answered correctly to that item. ',strong('SD'),' standard deviation, ',
                    strong('RIT'),' Pearson correlation between item and Total score, ',strong('RIR'),'
                     Pearson correlation between item and rest of items, ',strong('ULI'),'
                     - Upper-Lower Index, ',strong('Alpha Drop'),' - Cronbach\'s alpha of test without given item.'),
                  tableOutput('itemexam'),
                  # verbatimTextOutput('contents')
                  br()
                ),


                #####################
                #DISTRACTORS
                #####################
                tabPanel(
                   "Distractors",
                   br(),
                   p('Traditional item analysis uses proportions of correct answers or correlations to estimate item properties.'),
                   h3("Distractor Analysis"),
                   p('In distractor analysis, we are interested in how test takers select
                     the correct answer and how the distractors (wrong answers) were able
                     to function effectively by drawing the test takers away from the correct answer.'),
                  #br(),
                  htmlOutput("distractor_expl"),

                  #br(),
                  #h4("Groups"),
                  sliderInput(
                    'gr','Number of groups:',
                    min   = 1,
                    max   = 5,
                    value = 3
                  ),
                  fluidRow(column(12, align = "center", tableOutput('dist_group'))),
                  h4("Distractors Plot"),
                  plotOutput('graf'),
                  h4("Table with Counts"),
                  fluidRow(column(12, align = "center", tableOutput('distr'))),
                  h4("Table with Proportions"),
                  fluidRow(column(12, align = "center", tableOutput('distrproc'))),
                  br()
                  )
                )
                ),

                #####################
                #CLASSICAL ALL
                #####################
                tabPanel("Regression",
                         tabsetPanel(

                ########################
                #LOGISTIC
                ########################
                tabPanel(
                  "Logistic",
                  h3("Logistic Regression on Total Scores"),
                  #br(),
                  p('Various regression models may be fitted to describe
                    item properties in more detail.',
                  strong('Logistic regression'),'can model dependency of probability of correct answer on total score by
                    s-shaped logistic curve. Parameter',strong( "b0"),' describes horizontal position of the fitted curve,
                    parameter ',strong( 'b1'),' describes its slope.'),

                  h4("Plot with estimated logistic curve"),
                  plotOutput('logreg'),
                  h4("Equation"),
                  withMathJax(),
                  ('$$\\mathrm{P}(Y = 1|X, b_0, b_1) = \\mathrm{E}(Y|X, b_0, b_1) = \\frac{e^{\\left( b_{0} + b_1 X\\right)}}{1+e^{\\left( b_{0} + b_1 X\\right) }} $$'),

                 # uiOutput('eqlog'),
                  h4("Table of parametres"),
                  fluidRow(column(12, align = "center", tableOutput('logregtab'))),
                  htmlOutput("logisticint"),
                 br()
                ),

                ########################
                #LOGISTIC Z
                ########################
                tabPanel(
                  "Logistic Z",
                  h3("Logistic Regression on Standardized Total Scores"),
                  p('Various regression models may be fitted to describe
                    item properties in more detail.',
                  strong('Logistic regression'),'can model dependency of probability of correct answer on
                    standardized total score (Z-score) by s-shaped logistic curve. Parameter ',strong( 'b0'),' describes
                    horizontal position of the fitted curve (difficulty), parameter ',strong( 'b1'),' describes its slope at
                    inflection point (discrimination).'),
                  h4("Plot with estimated logistic curve"),
                  plotOutput('zlogreg'),
                  h4("Equation"),
                  ('$$\\mathrm{P}(Y = 1|Z, b_0, b_1) = \\mathrm{E}(Y|Z, b_0, b_1) = \\frac{e^{\\left( b_{0} + b_1 Z\\right) }}{1+e^{\\left( b_{0} + b_1 Z\\right) }} $$'),
                  h4("Table of parametres"),
                  fluidRow(column(12,align = "center",tableOutput('zlogregtab'))),
                  htmlOutput("zlogisticint"),
                  br()
                ),
                ###################
                #LOGISTIC IRT
                ###################
                tabPanel(
                  "Logistic IRT Z",
                  h3("Logistic Regression on Standardized Total Scores"),
                  p('Various regression models may be fitted to describe
                    item properties in more detail.',
                    strong('Logistic regression'),'can model dependency of probability of correct answer on
                    standardized total score (Z-score) by s-shaped logistic curve. Note change in parametrization - the IRT parametrization
                    used here corresponds to the parametrization used in IRT models.
                    Parameter', strong('b') ,'describes horizontal position of the fitted curve (difficulty),
                    parameter' , strong('a') ,' describes its slope at inflection point (discrimination).'),
                  h4("Plot with estimated logistic curve"),
                  plotOutput('zlogreg_irt'),
                  h4("Equation"),
                  ('$$\\mathrm{P}(Y = 1|Z, a, b) = \\mathrm{E}(Y|Z, a, b) = \\frac{e^{ a\\left(Z - b\\right) }}{1+e^{a\\left(Z - b\\right)}} $$'),
                  h4("Table of parametres"),
                  fluidRow(column(12,align = "center",tableOutput('zlogregtab_irt'))),
                  htmlOutput("zlogisticint_irt"),
                  br()
                ),
                ###################
                #NONLINEAR Z
                ###################

                tabPanel(
                  "Nonlinear IRT Z",
                  h3("Nonlinear Regression on Standardized Total Scores"),
                  p('Various regression models may be fitted to describe
                    item properties in more detail.',
                  strong('Nonlinear regression'),'can model dependency of probability of correct answer on
                  standardized total score (Z-score) by s-shaped logistic curve. The IRT parametrization used here corresponds
                  to the parametrization used in IRT models. Parameter ',strong( 'b'),' describes horizontal position of the fitted curve (difficulty),
                  parameter ',strong( 'a'),' describes its slope at inflection point (discrimination). This model allows for nonzero lower left asymptote ',strong( 'c'),'
                  (pseudo-guessing).'),
                  h4("Plot with estimated nonlinear curve"),
                  plotOutput('nlsplot'),
                  h4("Equation"),
                  ('$$\\mathrm{P}(Y = 1|Z, b_0, b_1, c) = \\mathrm{E}(Y|Z, b_0, b_1, c) = c + \\left( 1-c \\right) \\cdot \\frac{e^{a\\left(Z-b\\right) }}{1+e^{a\\left(Z-b\\right) }} $$'),
                  h4("Table of parametres"),
                  fluidRow(column(12,align = "center",tableOutput('nonlinearztab'))),
                  htmlOutput("nonlinearint"),
                  br()
                ),

                ###################
                #MULTINOMIAL
                ###################

                tabPanel(
                  "Multinomial",
                  h3("Multinomial Regression on Standardized Total Scores"),
                  p('Various regression models may be fitted to describe
                    item properties in more detail.',
                  strong('Multinomial regression'),'allows for simultaneous modelling of probability of choosing
                  given distractors on standardized total score (Z-score).'),
                  h4("Plot with estimated curves of multinomial regression"),
                  plotOutput('multiplot'),
                  h4("Equation"),
                  # ('$$\\mathrm{P}(Y = A|Z, b_{A0}, b_{A1}) = \\mathrm{P}(Y = D|Z, b_{D0}, b_{D1})\\cdot e^{\\left( b_{A0} + b_{A1} Z\\right) }$$'),
                  # ('$$\\mathrm{P}(Y = B|Z, b_{B0}, b_{B1}) = \\mathrm{P}(Y = D|Z, b_{D0}, b_{D1})\\cdot e^{\\left( b_{B0} + b_{B1} Z\\right) }$$'),
                  # ('$$\\mathrm{P}(Y = C|Z, b_{C0}, b_{C1}) = \\mathrm{P}(Y = D|Z, b_{D0}, b_{D1})\\cdot e^{\\left( b_{C0} + b_{C1} Z\\right) }$$'),
                   uiOutput('multieq'),
                  h4("Table of parametres"),
                  fluidRow(column(12,align = "center",tableOutput('multitab'))),
                  strong("Interpretation:"),
                  htmlOutput("multiint"),
                  br()
                  ),
id = "reg"
)),

    tabPanel("IRT models",
              tabsetPanel(

                ###################
                #IRT RASCH
                ###################
                tabPanel(
                  "1PL (Rasch)",
                  h3("One Parameter Item Response Theory Model"),
                  p('Item Response Theory (IRT) models are mixed-effect regression models in which
                    student ability (theta) is assumed to be a random effect and is estimated together with item
                    paramters. Ability (theta) is often assumed to follow normal distibution.'),
                  p('In',
                    strong('1PL IRT model,'), 'all items are assumed to have the same slope in inflection point – the
                    same discrimination', strong('a.'), 'Items can differ in location of their inflection point – in item difficulty',
                    strong('b.'), 'More restricted version of this model, the
                    ',strong('Rasch model,'),'assumes discrimination', strong('a'), 'is equal to 1.'),
                  h4("Equation"),
                  ('$$\\mathrm{P}\\left(Y_{ij} = 1\\vert \\theta_{i}, a, b_{j} \\right) =  \\frac{e^{a\\left(\\theta_{i}-b_{j}\\right) }}{1+e^{a\\left(\\theta_{i}-b_{j}\\right) }} $$'),
                  h4("Item Characteristic Curves"),
                  plotOutput('rasch'),
                  h4("Item information curves"),
                  plotOutput('raschiic'),
                  h4("Test information function"),
                  plotOutput('raschtif'),
                  h4("Table of coefficients"),
                  tableOutput('raschcoef'),
                  h4('Factor Scores vs. Standardized Total Scores'),
                  plotOutput('raschFactor'),
                  br()
                ),

                ###################
                #IRT 2PARAM
                ###################
                tabPanel(
                  "2PL ",
                  h3("Two Parameter Item Response Theory Model"),
                  p('Item Response Theory (IRT) models are mixed-effect regression models in which
                    student ability (theta) is assumed to be a random effect and is estimated together with item
                    paramters. Ability (theta) is often assumed to follow normal distibution.'),
                  p(strong('2PL IRT model,'), 'allows for different slopes in inflection point – different
                    discriminations', strong('a.'), 'Items can also differ in location of their inflection point – in item difficulty',
                    strong('b.')),
                  h4("Equation"),
                  ('$$\\mathrm{P}\\left(Y_{ij} = 1\\vert \\theta_{i}, a_{j}, b_{j}\\right) =  \\frac{e^{a_{j}\\left(\\theta_{i}-b_{j}\\right) }}{1+e^{a_{j}\\left(\\theta_{i}-b_{j}\\right) }} $$'),

                  h4("Item Characteristic Curves"),
                  plotOutput('twoparam'),
                  h4("Item information curves"),
                  plotOutput("twoparamiic"),
                  h4("Test information function"),
                  plotOutput("twoparamtif"),
                  h4("Table of coefficients"),
                  tableOutput('twoparamcoef'),
                  h4('Factor Scores vs. Standardized Total Scores'),
                  plotOutput('twoFactor'),
                  br()
                ),
                ###################
                #IRT 3PARAM
                ###################
                tabPanel(
                  "3PL ",
                  h3("Three Parameter Item Response Theory Model"),
                  p('Item Response Theory (IRT) models are mixed-effect regression models in which
                    student ability (theta) is assumed to be a random effect and is estimated together with item
                    paramters. Ability (theta) is often assumed to follow normal distibution.'),
                  p(strong('3PL IRT model,'), 'allows for different discriminations of items', strong('a,'),
                    'different item difficulties',
                    strong('b,'), 'and allows also for nonzero left asymptote – pseudo-guessing', strong('c.')),
                  h4("Equation"),
                  ('$$\\mathrm{P}\\left(Y_{ij} = 1\\vert \\theta_{i}, a_{j}, b_{j}, c_{j} \\right) = c_{j} + \\left(1 - c_{j}\\right) \\cdot \\frac{e^{a_{j}\\left(\\theta_{i}-b_{j}\\right) }}{1+e^{a_{j}\\left(\\theta_{i}-b_{j}\\right) }} $$'),

                  h4("Item characterisic curves"),
                  plotOutput('threeparam'),
                  h4("Item information curves"),
                  plotOutput("threeparamiic"),
                  h4("Test information function"),
                  plotOutput("threeparamtif"),
                  h4("Table of coefficients"),
                  tableOutput("threeparamcoef"),
                  h4('Factor Scores vs. Standardized Total Scores'),
                  plotOutput('threeFactor'),
                  br()
                ),
                id = "irt"
              )),
###################
# DIF
###################
                tabPanel("DIF/Fairness",
                     tabsetPanel(
                       tabPanel('Summary',
                          h3('Differential Item Functioning / Item Fairness'),
                          p('Differential item functioning (DIF) occurs when people from different groups (commonly gender or ethnicity) with the same underlying true ability have a different probability of answering the item correctly. If item functions differently for two groups, it is potentially unfair.'),
                          br(),
                          strong('Figure 1: Uniform DIF '), '(item has different difficulty for given two groups)',
                          br(),

                          ###   HTML('<div style="clear: left;"><img src="http://kylehamilton.com/wp-content/uploads/2014/11/kyle80.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
                          img(src='fig_NLR_uniformDIF.png', width = 320),
                          br(),
                          br(),
                          strong('Figure 2: Non-uniform DIF'), '(item has different discrimination and possibly also different difficulty for given two groups)',
                          br(),
                          img(src='fig_NLR_nonuniformDIF.png', width = 320),
                          br(),
                          br()
                          ),

                          tabPanel("Total Scores",
                                   h3("Total Scores"),
                                   p('DIF is not about total scores! Two groups may have the same distribution of total scores, yet, some item may function differently dor the two groups. Also, one of the groups may have signifficantly lower total score, yet, it may happen that there is no DIF item!'),
                                   h4("Summary of Total Scores for Groups"),
                                    tableOutput('resultsgroup'),
                                    h4("Histograms of Total Scores for Groups"),
                                    uiOutput("slider2group"),
                                    p('For selected cut-score, blue part of histogram shows students with total score
                    above the cut-score, grey column shows students with Total Score equal
                    to cut-score and red part of histogram shows students below the cut-score.'),
                                    #h4('Group = 1'),
                                    plotOutput('histbyscoregroup1'),
                                    #h4('Group = 0'),
                                    plotOutput('histbyscoregroup0'),
                                   br()
                                    ),
                           tabPanel("Delta Plots",
                                    h3("Delta Plot"),
                                    p('Delta plot (Angoff and Ford, 1993) compares the proportions of correct answers per
                                      item in the two groups. It displays non-linear transformation of these proportions using
                                      quantiles of standard normal distributions (so called delta scores) for each item for the two
                                      genders in a scatterplot called diagonal plot or delta plot (see Figure). Item is under
                                      suspicion of DIF if the delta point considerably departs from the diagonal.'),

                                    plotOutput('deltaplot'),
                                   # verbatimTextOutput("dp_text"),
                                    verbatimTextOutput("dp_text_normal"),
                                   br()),

                           tabPanel("Logistic Z",
                                    tabsetPanel(
                                      tabPanel('Summary',
                                               h3('Logistic regression'),
                                               p('Logistic regression allows for detection of uniform and non-uniform DIF (Swaminathan and Rogers, 1990) by adding a group
                                                 specific intercept', strong('bDIF'), '(uniform DIF) and group specific interaction', strong('aDIF'), '(non-uniform DIF) into model and
                                                 by testing for their significance.'),
                                               h4("Equation"),
                                               ('$$\\mathrm{P}\\left(Y_{ij} = 1 | Z_i, G_i, a_j, b_j, a_{\\text{DIF}j}, b_{\\text{DIF}j}\\right) = \\frac{e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)
                                                \\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}}{1+e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)
                                                \\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}} $$'),
                                               radioButtons(
                                                 'type_summary_logistic', 'Type',
                                                 c(
                                                   "H0: Any DIF vs. H1: No DIF" = 'both',
                                                   "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                                   "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'

                                                 ),
                                                 'both'
                                               ),
                                               verbatimTextOutput('Log_vto'),
                                               br()),
                                      tabPanel('Items',
                                               h3('Logistic regression'),
                                               p('Logistic regression allows for detection of uniform and non-uniform DIF by adding a group
                                                 specific intercept', strong('bDIF'), '(uniform DIF) and group specific interaction', strong('aDIF'), '(non-uniform DIF) into model and
                                                 by testing for their significance.'),
                                               h4("DIF logistic plot"),
                                   radioButtons(
                                      'type_plot_logistic', 'Type',
                                      c(
                                        "H0: Any DIF vs. H1: No DIF" = 'both',
                                        "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                        "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'

                                      ),
                                      'both'
                                    ),
                                    plotOutput('logistic_DIF_plot'),
                                   h4("Equation"),
                                   ('$$\\mathrm{P}\\left(Y_{ij} = 1 | Z_i, G_i, a_j, b_j, a_{\\text{DIF}j}, b_{\\text{DIF}j}\\right) =
                                                \\frac{e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}}
                                    {1+e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}} $$'),
                                   h4("Table of parametres"),
                                   fluidRow(column(12,align = "center",tableOutput('LRcoefDIF'))),
                                   br()
                                    ))),
                           tabPanel("Nonlinear Z",
                                    tabsetPanel(
                                      tabPanel('Summary',
                                               h3('Nonlinear regression'),
                                               p('Nonlinear regression model allows for nonzero lower asymptote - pseudoguessing', strong('c.'), 'Similarly to logistic regression, also nonlinear regression allows for detection of uniform
                                                and non-uniform DIF by adding a group
                                                 specific intercept', strong('bDIF'), '(uniform DIF) and group specific interaction', strong('aDIF'), '(non-uniform DIF) into the model and by testing for their significance.'),
                                               h4("Equation"),
                                               ('$$\\mathrm{P}\\left(Y_{ij} = 1 | Z_i, G_i, a_j, b_j, c_j, a_{\\text{DIF}j}, b_{\\text{DIF}j}\\right) =
                                                c_j + \\left(1 - c_j\\right) \\cdot \\frac{e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}}
                                                {1+e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}} $$'),
                                               radioButtons(
                                                 'type_summary_NLR', 'Type',
                                                 c(
                                                   "H0: Any DIF vs. H1: No DIF" = 'both',
                                                   "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                                   "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'

                                                 ),
                                                 'both'
                                               ),
                                               #tableOutput('NLR_Summary'),
                                               verbatimTextOutput('NLR_sum_verbatim'),
                                               br()
                                               ),
                                      tabPanel('Items',
                                               h3('Nonlinear regression'),
                                               p('Nonlinear regression model allows for nonzero lower asymptote - pseudoguessing', strong('c.'), 'Similarly to logistic regression, also nonlinear regression allows for detection of uniform
                                                and non-uniform DIF (Drabinova and Martinkova, 2016) by adding a group
                                                 specific intercept', strong('bDIF'), '(uniform DIF) and group specific interaction', strong('aDIF'), '(non-uniform DIF) into the model and by testing for their significance.'),
                                          h4("DIF NLR Plot"),
                                          radioButtons(
                                            'type_plot_NLR', 'Type',
                                            c(
                                              "H0: Any DIF vs. H1: No DIF" = 'both',
                                              "H0: Uniform DIF vs. H1: No DIF" = 'udif',
                                              "H0: Non-Uniform DIF vs. H1: Uniform DIF" = 'nudif'
                                              ),
                                            'both'
                                            ),

                                    plotOutput('plotNLR_DIF'),
                                    h4("Equation"),
                                    ('$$\\mathrm{P}\\left(Y_{ij} = 1 | Z_i, G_i, a_j, b_j, c_j, a_{\\text{DIF}j}, b_{\\text{DIF}j}\\right) =
                                                c_j + \\left(1 - c_j\\right) \\cdot \\frac{e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}}
                                     {1+e^{\\left(a_j + a_{\\text{DIF}j} G_i\\right)\\left(Z_i -\\left(b_j + b_{\\text{DIF}j} G_i\\right)\\right)}} $$'),

                                    h4("Table of parametres"),
                                    fluidRow(column(12,align = "center",tableOutput('coef_tab_NLR'))),
                                    br()
                                    )
                                    )
                                    ),

#                            tabPanel("IRT Lord",
#                                     tabsetPanel(tabPanel('Summary',
#                                                          h3('Lord statistic'),
#                                                          p('Lord statistic (Lord, 1980) is based on IRT model (1PL, 2PL, or 3PL with the same guessing). It uses the difference between item parameters for the two groups to detect DIF. In statistical terms, Lord statistic is equal to Wald statistic.'),
#                                                          br(),
#                                                          img(src='lord_udif.png', width = 320),
#                                                          img(src='lord_nudif.png', width = 320),
#                                                          radioButtons(
#                                                            'model_lord', 'Model',
#                                                            c(
#                                                              "1PL" = '1PL',
#                                                              "2PL" = '2PL',
#                                                              "3PL" = '3PL'
#                                                              ),
#                                                            '2PL'
#                                                          ),
#                                                          verbatimTextOutput('Lord_vto'),
#                                                          br())
#                                                 )
#                                     ),
#                            tabPanel("IRT Raju",
#                                     tabsetPanel(tabPanel('Summary',
#                                                          h3('Raju statistic'),
#                                                          p('Raju statistic (Raju, 1988, 1990) is based on IRT model (1PL, 2PL, or 3PL with the same guessing). It uses the area between the item charateristic curves for the two groups to detect DIF.'),
#                                                          br(),
#                                                          img(src='raju_udif.png', width = 320),
#                                                          img(src='raju_nudif.png', width = 320),
#                                                          radioButtons(
#                                                            'model_raju', 'Model',
#                                                            c(
#                                                              "1PL" = '1PL',
#                                                              "2PL" = '2PL',
#                                                              "3PL" = '3PL'
#                                                            ),
#                                                            '2PL'
#                                                          ),
#                                                          verbatimTextOutput('Raju_vto'),
#                                                          br())
#                                                 )),
                       id = "dif"
                       )),



###################
#DATA DESCRIPTIONS
###################
tabPanel(
  "Data",

h3("Data"),

p('For demonstration purposes, 20-item dataset ' ,code("GMAT"),'
and datasets' ,code("GMATkey"),'
and ' ,code("GMATgroups"),' from
R ', code('library(difNLR)'),' are used. On this page, you may select your own dataset (see below).
To return to demonstration dataset, refresh this page in your browser' ,strong("(F5)"),'.'),

  p('Used dataset ' ,code("GMAT"),' is generated based on parameters of real Graduate Management
    Admission Test (GMAT) data set (Kingston et al., 1985). However, first two items were
    generated to function differently in non-uniform and uniform way respectively.
    The data set represents responses of 1,000 subjects to multiple-choice test of 20 items.'),

 br(),
 h4("Upload your own datasets"),
 p('Main dataset should contain responses of individual students (rows) to given items (collumns). Header may contain item names, no row names should be included. If responses are in ABC format, the key provides correct respomse for each item. If responses are scored 0-1, key is vecor of 1s.'),
  fluidRow(
    column(4, offset = 0, fileInput(
      'data', 'Choose data (csv file)',
      accept = c(
        'text/csv',
        'text/comma-separated-values',
        'text/tab-separated-values',
        'text/plain',
        '.csv',
        '.tsv'
      )
    )),
    column(4,offset = 1,fileInput(
      'key', 'Choose key (csv file)',
      accept = c(
        'text/csv',
        'text/comma-separated-values',
        'text/tab-separated-values',
        'text/plain',
        '.csv',
        '.tsv'
      )
    ))
     ,
     column(4,fileInput(
       'groups', 'Choose groups for DIF (optional)',
       accept = c(
         'text/csv',
         'text/comma-separated-values',
         'text/tab-separated-values',
         'text/plain',
         '.csv',
         '.tsv'
       )
    ))
  ),

  tags$hr(),
  h4("Data Specification"),

  fluidRow(
    column(1, offset = 0, checkboxInput('header', 'Header', TRUE)),
    column(3,offset = 1,radioButtons(
      'sep', 'Separator',
      c(
        Comma = ',',
        Semicolon = ';',
        Tab = '\t'
      ),
      ','
    )),
    column (3,offset = 0,radioButtons(
      'quote', 'Quote',
      c(
        None = '',
        'Double Quote' =
          '"',
        'Single Quote' =
          "'"
      ),
      '"'
    ))
  ),

  tags$hr(),

  h4("Data Check (first 6 respondents only)"),
  tableOutput('headdata'),

  h4("Key (correct answers)"),
  tableOutput('key'),
  h4("Scored Test"),
  tableOutput('sc01')
  ),

########################
#ABOUT
########################
tabPanel(
  "About",
  strong('Version'),
  p("ShinyItemAnalysis Version 0.2"),
  p('ShinyItemAnalysis Version 0.1 is available',
    a("here.", href = "https://shiny.cs.cas.cz/ShinyItemAnalysisV01/",
      target = "_blank")),

  br(),
  strong('Description'),
  p('ShinyItemAnalysis provides
    analysis of tests and their items.  It is based on the',
    a("Shiny", href = "http://www.rstudio.com/shiny/", target = "_blank"), 'R package. '),

  br(),
  strong('Data'),
  p('For demonstration purposes, practice dataset from',
    code('library(difNLR)'),'is used.
    On page',
    strong("Data"), 'you may select your own dataset '),
  br(),

  strong('List of Packages Used'), br(),

  code('library(CTT)'), br(),
  code('library(deltaPlotR)'), br(),
  code('library(difNLR)'), br(),
  #code('library(difR)'), br(),
  code('library(ggplot2)'), br(),
  code('library(gridExtra)'), br(),
  code('library(ltm)'), br(),
  code('library(moments)'), br(),
  code('library(nnet)'), br(),
  code('library(psychometric)'), br(),
  code('library(reshape2)'), br(),
  code('library(shiny)'), br(),
  code('library(shinyAce)'), br(),

  br(),
  strong('Authors'),
  br(),

  ###   HTML('<div style="clear: left;"><img src="http://kylehamilton.com/wp-content/uploads/2014/11/kyle80.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
  img(src = 'patricia.jpg', width = 70),
  p(
    a("Patricia Martinkova, Institute of Computer Science, Czech Academy of Sciences",
      href = "http://www.cs.cas.cz/martinkova/", target = "_blank")
  ),
  img(src = 'leder.png', width = 70),
  p(
    a("Ondrej Leder", href = "https://www.linkedin.com/in/ond%C5%99ej-leder-3864b1119",
      target = "_blank")
  ),
  img(src = 'adela.jpg', width = 70),
  p("Adela Drabinova"),

  strong('Bug Reports'),

  p(
    "If you discover a problem with this application please contact the project maintainer at martinkova(at)cs.cas.cz "
  ),
  strong('Acknowledgments'),
  p(
    " Project was supported by grant funded by Czech Science foundation under number ",
    a("GJ15-15856Y", href = "http://www.cs.cas.cz/martinkova/psychometrics.html",
      target = "_blank")
  ),


  br(),

  strong('License'),

  p(" Copyright 2016  Patricia Martinkova, Ondrej Leder and Adela Drabinova"),

  p(
    " This program is free software you can redistribute it and or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation either version 3 of the License or
    at your option any later version."
  ),

  p(
    "This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details."
  )
  )
)
)
))
