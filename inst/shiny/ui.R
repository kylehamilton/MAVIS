
shinyUI(navbarPage("MAVIS: Meta Analysis Via Shiny v1.0.1",

      tabPanel("Main",
               sidebarPanel(
                 
                 radioButtons("type", strong("Analysis and data input type:"),
                              list("Mean Differences (n, M, SD)" = "mdms",
                                   "Mean Differences (n, Effect size d)" = "mdes",
                                   "Correlations (n, r)" = "cor"
                              ),
                 ),
                 helpText("Click here to update your results, you need to do this after you change the data, model, or setting"),
                 submitButton("Update View"),
                 br(),
                 helpText("Press Quit to exit the application"),
                 actionButton("quit", "Quit")
               
                 
               ),
               br(),

               p('Note: Input values must be separated by tabs. Copy and paste from Excel.'),
               p("Your data needs to have exactly the same header (variable names) in the first row."),

               strong('Option:'),
               checkboxInput("moderator", label = strong("The data contains a categorical moderator (subgroup) variable."), value = T),

               br(),

               aceEditor("text", value="Study\tModerator\tN1\tM1\tSD1\tN2\tM2\tSD2\nStudy 01\tJH\t30\t51.57\t16.5\t30\t72.97\t13.23\nStudy 02\tUNI\t23\t75.09\t23.01\t24\t81.63\t14.42\nStudy 03\tSH\t83\t30.08\t14.29\t81\t35.38\t16.13\nStudy 04\tSH\t21\t2.95\t1.28\t21\t3.48\t0.68\nStudy 05\tSH\t15\t53.8\t17.4\t15\t60.47\t17.37\nStudy 06\tSH\t7\t15.7\t4.1\t7\t27.3\t4.1\nStudy 07\tSH\t28\t27.9\t9.57\t28\t33.2\t15.65\nStudy 08\tUNI\t40\t17.53\t8.87\t40\t19.23\t9.55\nStudy 09\tUNI\t18\t11.86\t13.24\t17\t29.92\t16.67\nStudy 10\tUNI\t21\t29.76\t16\t25\t27.98\t16.52\nStudy 11\tUNI\t26\t8.23\t3.59\t26\t9.65\t2.99\nStudy 12\tUNI\t49\t13.71\t4.07\t48\t16\t3.47\nStudy 13\tUNI\t27\t2.8\t1.7\t27\t5.9\t1.4\nStudy 14\tSH\t41\t10.05\t2.52\t34\t11.03\t1.78\nStudy 15\tUNI\t58\t3.62\t1.79\t57\t4.26\t1.61\nStudy 16\tSH\t60\t7.36\t2.8\t63\t8.82\t2.5\nStudy 17\tUNI\t15\t5.93\t3.55\t15\t12.27\t4.95\nStudy 18\tJH\t37\t13.68\t3.68\t142\t17.53\t4.34\nStudy 19\tJH\t27\t3.3\t2.3\t54\t12.98\t7.67\nStudy 20\tJH\t35\t5.49\t3.88\t39\t12.36\t7.68\nStudy 21\tJH\t32\t5.81\t3.14\t34\t12.44\t5.66\nStudy 22\tJH\t62\t17.84\t4.09\t60\t18.18\t4.09\nStudy 23\tSH\t39\t8.77\t5\t39\t13.72\t5.32\nStudy 24\tSH\t213\t59.8\t15.3\t39\t79.8\t9.5\nStudy 25\tUNI\t34\t14.32\t2.79\t42\t16\t2.05\nStudy 26\tUNI\t77\t70.85\t11.74\t56\t78.17\t9.94\nStudy 27\tUNI\t28\t80.83\t22.47\t28\t85.06\t23.52\nStudy 28\tUNI\t33\t25.38\t4.71\t36\t25.02\t3.36\nStudy 29\tUNI\t66\t0.45\t0.29\t66\t0.93\t0.59",
                         mode="r", theme="mono_industrial"),

               br(),

               h3("Effect size and sampling variance"),

               verbatimTextOutput("data.out"),

               br(),

               h3("Fixed effects model"),
               verbatimTextOutput("fe.out"),

               br(),

               h3("Random effects model"),
               verbatimTextOutput("re.out"),

               p('[Criteria for checking heterogeneity]',
                 br(),
                 br(),
                 'I^2 (How much effect sizes across studies differ)', br(),
                 "25-50: Little different", br(),
                 '50-75: Quite different', br(),
                 '75-100: Considerably different', br(),
                 br(),
                 'Test for Heterogeneity: p-val < .05 (not homogeneous)', br(),
                 br(),
                 'H (sqrt(H^2)) > 1: There is unexplained heterogeneity.'
               ),

               br(),
               br(),


               h3("Forest plot (Fixed effects model)"),
               downloadButton('downloadfePlot', 'Download the plot as pdf'),
               plotOutput("fePlot", height = "550px"),



               br(),

               h3("Forest plot (Random effects model)"),
               downloadButton('downloadrePlot', 'Download the plot as pdf'),
               plotOutput("rePlot", height = "550px"),

               br(),

               h3("Funnel plot (Fixed effects model)"),
               downloadButton('downloadFunFixPlot', 'Download the plot as pdf'),
               plotOutput("FunFixPlot"),
               p('Open circles (if any) on the right side show missing NULL studies estimated with the trim-and-fill method, added in the funnel plot.'),

               br(),

               h3("Funnel plot (Random effects model)"),
               downloadButton('downloadFunRandPlot', 'Download the plot as pdf'),
               plotOutput("FunRandPlot"),
               p('Open circles (if any) on the right side show missing NULL studies estimated with the trim-and-fill method, added in the funnel plot.'),
               br(),
               br(),

               verbatimTextOutput("asy.out"), # regression tests for funnel plot asymmetry
               p('Fail-safe N is the number of nonsignificant studies necessary to make the result nonsignificant. "When the fail-safe N is high, that is interpreted to mean that even a large number of nonsignificant studies may not influence the statistical significance of meta-analytic results too greatly."',
                 a('(Oswald & Plonsky, 2010, p. 92)', href='http://dx.doi.org/10.1017/S0267190510000115', target="_blank"), '.'),

               br(),

               br(),

               # Display this only if "moderator" is checked
               conditionalPanel(condition = "input.moderator == true",
                                h3("Moderator (subgroup) analysis"),
                                verbatimTextOutput("modAnalysis.out")
               ),

               br(),

               # Display this only if "moderator" is checked
               conditionalPanel(condition = "input.moderator == true",
                                h4("Categorical moderator graph (Fixed effects model)"),
                                plotOutput("ModFixGraph")
               ),

               br(),

               # Display this only if "moderator" is checked
               conditionalPanel(condition = "input.moderator == true",
                                h4("Categorical moderator graph (Random effects model)"),
                                plotOutput("ModRandGraph")
               ),

               br(),
               br(),

               strong('R session info'),
               verbatimTextOutput("info.out")
      ),



      tabPanel("Input Examples",

               p('Note: Input values must be separated by tabs. Copy and paste from Excel.'),

               p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have exactly the same header (variable names) in the first row.</div></b>")),

               br(),

               p(strong("Mean Differences (n, M, SD)")),
               aceEditor("text1", value="Study\tModerator\tN1\tM1\tSD1\tN2\tM2\tSD2\nStudy 01\tJH\t30\t51.57\t16.5\t30\t72.97\t13.23\nStudy 02\tUNI\t23\t75.09\t23.01\t24\t81.63\t14.42\nStudy 03\tSH\t83\t30.08\t14.29\t81\t35.38\t16.13\nStudy 04\tSH\t21\t2.95\t1.28\t21\t3.48\t0.68\nStudy 05\tSH\t15\t53.8\t17.4\t15\t60.47\t17.37\nStudy 06\tSH\t7\t15.7\t4.1\t7\t27.3\t4.1\nStudy 07\tSH\t28\t27.9\t9.57\t28\t33.2\t15.65\nStudy 08\tUNI\t40\t17.53\t8.87\t40\t19.23\t9.55\nStudy 09\tUNI\t18\t11.86\t13.24\t17\t29.92\t16.67\nStudy 10\tUNI\t21\t29.76\t16\t25\t27.98\t16.52\nStudy 11\tUNI\t26\t8.23\t3.59\t26\t9.65\t2.99\nStudy 12\tUNI\t49\t13.71\t4.07\t48\t16\t3.47\nStudy 13\tUNI\t27\t2.8\t1.7\t27\t5.9\t1.4\nStudy 14\tSH\t41\t10.05\t2.52\t34\t11.03\t1.78\nStudy 15\tUNI\t58\t3.62\t1.79\t57\t4.26\t1.61\nStudy 16\tSH\t60\t7.36\t2.8\t63\t8.82\t2.5\nStudy 17\tUNI\t15\t5.93\t3.55\t15\t12.27\t4.95\nStudy 18\tJH\t37\t13.68\t3.68\t142\t17.53\t4.34\nStudy 19\tJH\t27\t3.3\t2.3\t54\t12.98\t7.67\nStudy 20\tJH\t35\t5.49\t3.88\t39\t12.36\t7.68\nStudy 21\tJH\t32\t5.81\t3.14\t34\t12.44\t5.66\nStudy 22\tJH\t62\t17.84\t4.09\t60\t18.18\t4.09\nStudy 23\tSH\t39\t8.77\t5\t39\t13.72\t5.32\nStudy 24\tSH\t213\t59.8\t15.3\t39\t79.8\t9.5\nStudy 25\tUNI\t34\t14.32\t2.79\t42\t16\t2.05\nStudy 26\tUNI\t77\t70.85\t11.74\t56\t78.17\t9.94\nStudy 27\tUNI\t28\t80.83\t22.47\t28\t85.06\t23.52\nStudy 28\tUNI\t33\t25.38\t4.71\t36\t25.02\t3.36\nStudy 29\tUNI\t66\t0.45\t0.29\t66\t0.93\t0.59", mode="r", theme="monokai"),


               br(),
               p(strong("Mean Differences (n, Effect size d)")),
               aceEditor("text2", value="Study\tModerator\tN1\tN2\td\nStudy 01\tJH\t30\t30\t-1.431\nStudy 02\tUNI\t23\t24\t-0.3423\nStudy 03\tSH\t83\t81\t-0.3481\nStudy 04\tSH\t21\t21\t-0.5171\nStudy 05\tSH\t15\t15\t-0.3837\nStudy 06\tSH\t7\t7\t-2.8293\nStudy 07\tSH\t28\t28\t-0.4086\nStudy 08\tUNI\t40\t40\t-0.1845\nStudy 09\tUNI\t18\t17\t-1.2039\nStudy 10\tUNI\t21\t25\t0.1093\nStudy 11\tUNI\t26\t26\t-0.4298\nStudy 12\tUNI\t49\t48\t-0.605\nStudy 13\tUNI\t27\t27\t-1.9907\nStudy 14\tSH\t41\t34\t-0.4422\nStudy 15\tUNI\t58\t57\t-0.3758\nStudy 16\tSH\t60\t63\t-0.5508\nStudy 17\tUNI\t15\t15\t-1.4719\nStudy 18\tJH\t37\t142\t-0.9136\nStudy 19\tJH\t27\t54\t-1.5079\nStudy 20\tJH\t35\t39\t-1.111\nStudy 21\tJH\t32\t34\t-1.4368\nStudy 22\tJH\t62\t60\t-0.0831\nStudy 23\tSH\t39\t39\t-0.9588\nStudy 24\tSH\t213\t39\t-1.3729\nStudy 25\tUNI\t34\t42\t-0.6976\nStudy 26\tUNI\t77\t56\t-0.6642\nStudy 27\tUNI\t28\t28\t-0.1839\nStudy 28\tUNI\t33\t36\t0.0886\nStudy 29\tUNI\t66\t66\t-1.0326", mode="r", theme="monokai"),


               br(),
               p(strong("Correlations (n, r)")),
               aceEditor("text3", value="Study\tN\tr\tModerator\nIzumi (2000)\t175\t0.78\tcollege\nYu (2009)\t53\t0.38\tJS high\nThuy (1996)\t250\t0.69\tcollege\nOckey (2002)\t90\t0.89\tcollege\nAraru (2005)\t86\t0.52\tJS high\nWee (1997)\t182\t0.98\tcollege\nOzoda (2007)\t591\t0.91\tcollege\nHala (2004)\t30\t0.95\tcollege\nTapio (2008)\t37\t0.47\tJS high\nAndarani (2008)\t107\t0.84\tcollege\nDavis (1999)\t74\t0.99\tcollege\nPlonsky (2002)\t217\t0.86\tcollege\nGassel (1993)\t203\t0.99\tcollege",mode="r", theme="monokai"),

               br()

      ),
 navbarMenu("Correlation model measures",
            tabPanel("Correlation model options",
                     
                     radioButtons("cormeasures", strong("Correlation model measures"),
                                  c("raw correlation coefficient" = "COR",
                                    "raw correlation coefficient corrected for its slight negative bias" = "UCOR",
                                    "Fisher’s r-to-z transformed correlation coefficient" = "ZCOR"
                                  ), selected = "ZCOR"),
                     p(h6('Fisher’s r-to-z transformed correlation coefficient is the default estimator for the metafor package')),
                     
                     verbatimTextOutput('cormeasures.out')
                     
            ) ),
 navbarMenu("Model Estimators",
      tabPanel("Random-effects model estimators",

               radioButtons("model", strong("Random-effects model estimators"),
                            c("DerSimonian-Laird" = "DL",
                              "Hedges" = "HE",
                              "Hunter-Schmidt" = "HS",
                              "Sidik-Jonkman" = "SJ",
                              "Maximum-likelihood" = "ML",
                              "Restricted maximum-likelihood" = "REML",
                              "Empirical Bayes (Paule-Mandel)" = "EB",
                              "Generalized Q-statistic" = "GENQ"
                            ), selected = "REML"),
               p(h6('Restricted maximum-likelihood is the default estimator for the metafor package')),
               
               verbatimTextOutput('model.out')

     ) ),
 navbarMenu("Publication Bias Options",
      tabPanel("Trim and Fill Options",

               radioButtons("trimfillopt", strong("Trim and Fill Estimator"),
                            c("L0" = "L0",
                              "R0" = "R0",
                              "Q0" = "Q0"
                            ), selected = "L0"),
               p(h6('Three different estimators for the number of missing studies were proposed by Duval and Tweedie (2000a, 2000b). The default estimator for the metafor package is L0')),
               
               verbatimTextOutput('trimfillopt.out')

      ),

      tabPanel("Regression Test for Funnel Plot Asymmetry",
               
               radioButtons("regtestpredictor", strong("Predictor"),
                            c("standard error" = "sei",
                              "sampling variance" = "vi",
                              "sample size" = "ni",
                              "inverse of the sample size" = "ninv"
                            ), selected = "sei"),
               p(h6('Predictor used for the regression test. The default in the metafor package is standard error')),
               
               verbatimTextOutput('regtestpredictor.out')
               
      ),
      
      tabPanel("File Drawer Analysis",
               
               radioButtons("filedraweranalysis", strong("File Drawer Analysis"),
                            c("Rosenthal" = "Rosenthal",
                              "Orwin" = "Orwin",
                              "Rosenberg" = "Rosenberg"
                            ), selected = "Rosenthal"),
               p(h6('Method for running file drawer analysis. The default in the metafor package is Rosenthal')),
               p('The Rosenthal method (sometimes called a ‘file drawer analysis’) calculates the number of studies
averaging null results that would have to be added to the given set of observed outcomes to reduce
the combined significance level (p-value) to a target alpha level (e.g., .05). The calculation is based
on Stouffer’s method to combine p-values and is described in Rosenthal (1979).'),
p('The Orwin method calculates the number of studies averaging null results that would have to be
added to the given set of observed outcomes to reduce the (unweighted) average effect size to a
target (unweighted) average effect size. The method is described in Orwin (1983).'),
p('The Rosenberg method calculates the number of studies averaging null results that would have to be
added to the given set of observed outcomes to reduce significance level (p-value) of the (weighted)
average effect size (based on a fixed-effects model) to a target alpha level (e.g., .05). The method is
described in Rosenberg (2005).'),
               
               verbatimTextOutput('filedraweranalysis.out')
               
      )),
navbarMenu("Effect Size Calculator",
          tabPanel("One Study with Means, SDs, Ns",
         
                   verticalLayout(
                     
                     wellPanel(
                       fluidRow(
                         column(3,
                      p(strong("Group 1:")),
                     
                     numericInput("nx", " Sample size (n)", 21),
                     
                     numericInput("mx", " Mean", 61.33),
                     
                     numericInput("sdx", " SD", 16.43),
                     
                     p(br())
                         ),
                     column(4, offset = 1,
                     p(strong("Group 2:")),
                     
                     numericInput("ny", " Sample size (n)", 24),
                     
                     numericInput("my", " Mean", 59.79),
                     
                     numericInput("sdy", " SD", 18.50),
                     
                     p(br())
                       ),
                     column(4,
                     strong('Option:'),
                     
                     
                     checkboxInput("varequal", "t-test with equal variances assumed", FALSE),
                     
                     
                     checkboxInput("vartest", "Show test for equality of variances", FALSE),
                     helpText("Click here to update your results."),
                     submitButton("Update View")
                     )
                     
                   )),
                   
                     h3("Checking the input data"),
                     tableOutput("values"),
                                
                    br(),
                                
                     h3("Mean of the differences and 95% CI"),
                     verbatimTextOutput("difference.out"),
                                
                     br(),
                                
                     h3("t-test"),
                     verbatimTextOutput("ttest.out"),
                     h3(""),
                     verbatimTextOutput("vartest.out"),
                                
                     br(),
                                
                     h3("Effect size indices"),
                     verbatimTextOutput("es.out"),                   
                     br()
                                
                       )
         
          ),
          tabPanel("ANCOVA F-statistic to Effect Size",
                   verticalLayout(
                     
                     wellPanel(
                       fluidRow(
                         column(3,
                                p(strong("ANCOVA F-statistic to Effect Size")),
                                
                                numericInput("ancovaf", " F value from ANCOVA", 21),
                                
                                numericInput("ancovafn1", " Treatment group sample size", 50),
                                
                                numericInput("ancovafn2", " Comparison group sample size", 50),
                                
                                p(br())
                         ),
                         column(4, offset = 1,
                                
                                numericInput("anovafcovar", " Covariate outcome correlation or multiple correlation", 0.24),
                                
                                numericInput("anovafcovarnum", " Number of covariates", 3),
                                
                                numericInput("sdy", " SD", 18.50),
                                helpText("Click here to update your results"),
                                submitButton("Update View"),
                                p(br())
                         )

                         
                       )),
                     
                     
                     h3("Effect size indices"),
                     verbatimTextOutput("ancovaf.out"),
                     p(br())
                     
                   )
                   
          ),
          tabPanel("Mean Values from ANCOVA F-statistic to Effect Size",
                   verticalLayout(
                     
                     wellPanel(
                       fluidRow(
                         column(3,
                                p(strong("Mean Values from ANCOVA F-statistic to Effect Size")),
                                
                                numericInput("ancovamean1", " Adjusted mean of treatment group from ANCOVA", 21.7),
                                
                                numericInput("ancovamean2", " Adjusted mean of comparison group from ANCOVA", 33.5),
                                
                                numericInput("ancovameansd", " Adjusted standard deviation", 50),
                                
                                p(br())
                         ),
                         column(4, offset = 1,
                                
                                numericInput("ancovameann1", " Treatment group sample size", 142),
                                
                                numericInput("ancovameann2", " Comparison group sample size", 133),
                                
                                numericInput("ancovameancovar", " Covariate outcome correlation or multiple correlation", 0.24),

                                numericInput("ancovameancovarnumber", " Number of covariate", 3),
                                
                                
                                helpText("Click here to update your results"),
                                submitButton("Update View"),
                                p(br())
                         )
                         
                         
                       )),
                     
                     
                     h3("Effect size indices"),
                     verbatimTextOutput("ancovamean.out"),
                     p(br())
                     
                   )
                   
          ),
tabPanel("Chi-Squared Statistic to Effect Size",
         verticalLayout(
           
           wellPanel(
             fluidRow(
               column(3,
                      p(strong("Chi-Squared Statistic to Effect Size")),
                      
                      numericInput("chisquaredstat", " Chi squared statistic from primary study.", 5.3),
                      
                      numericInput("chisquaredn1", " Sample size in primary study.", 50),
                      
                      p(br())
               ),
               column(4, offset = 1,
                      helpText("Click here to update your results"),
                      submitButton("Update View"),
                      p(br())
               )
               
               
             )),
           
           
           h3("Effect size indices"),
           verbatimTextOutput("chisquared.out"),
           p(br())
           
         )
         
),
tabPanel("p-value to Effect Size",
         verticalLayout(
           
           wellPanel(
             fluidRow(
               column(3,
                      p(strong("p-value to Effect Size")),
                      
                      numericInput("pvaluenum", " p-value.", 0.01),
                      numericInput("pvaluen1", " Sample size of treatment group.", 50),                      
                      numericInput("pvaluen2", " Sample size of comparison group.", 50),
                      
                      radioButtons("pvaluetail", strong("One or two-tailed p-value."),
                                   c("One tail" = "one",
                                     "Two tail" = "two"
                                   ), selected = "two"),
                      
                      p(br())
               ),
               column(4, offset = 1,
                      helpText("Click here to update your results"),
                      submitButton("Update View"),
                      p(br())
               )
               
               
             )),
           
           
           h3("Effect size indices"),
           verbatimTextOutput("pvaluees.out"),
           p(br())
           
#          ),
#          tabPanel("single case designs",
#                   verticalLayout(
#                     
#                     wellPanel(
#                       fluidRow(
#                         column(3,
#                                p(strong("single case designs")),
#                                
#                                numericInput("pvaluenum", " p-value.", 0.01),
#                                numericInput("pvaluen1", " Sample size of treatment group.", 50),                      
#                                numericInput("pvaluen2", " Sample size of comparison group.", 50),
#                                
#                                radioButtons("pvaluetail", strong("One or two-tailed p-value."),
#                                             c("One tail" = "one",
#                                               "Two tail" = "two"
#                                             ), selected = "two"),
#                                
#                                p(br())
#                         ),
#                         radioButtons("scd", strong("Type of single-case design"),
#                                      c("AB" = "AB",
#                                        "ABA" = "ABA",
#                                        "ABAB" = "ABAB",
#                                        "Completely Random Design" = "CRD",
#                                        "Randomized Block Design" = "RBD",
#                                        "Alternating Treatments Design" = "ATD",
#                                        "Multiple-baseline AB design" = "MBD"
#                                      ), selected = "AB"),
#                         p(h6('Type of single-case design')),
#                         
#                         verbatimTextOutput('scd.out')
#                         column(4, offset = 1,
#                                helpText("Click here to update your results"),
#                                submitButton("Update View"),
#                                p(br())
#                         )
#                         
#                         
#                       )),
#    
#                     p(br())
                    
                  )
         
)),

      tabPanel("About",

               strong('MAVIS: Meta Analysis Via Shiny'),
               p("The goal of this project is to help students and researchers run a meta-analysis as easily as possible."),
               p('This application is developed with',
                 a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
                 ''),
               p('The code for this application is available at this',
                 a('GitHub.', href='https://github.com/kylehamilton/meta', target="_blank")),


               br(),

               strong('List of Packages Used'), br(),
               code('library(shiny)'),br(),
               code('library(shinyAce)'),br(),
               code('library(metafor)'),br(),
               code('library(meta)'),br(),
               code('library(MAd)'),br(),
               code('library(MAc)'),br(),
               code('library(quantreg)'),br(),
               code('library(ggplot2)'),br(),
               code('library(compute.es)'),br(),


               br(),

               h4('Acknowledgments and Authors'),

               strong('Acknowledgments'),

               p('William Kyle Hamilton would like to thank the ',
                 a("Health Communications and Interventions Lab at UC Merced", href="http://cameronhcilab.com/", target="_blank"),
                 'for their comments and beta testing efforts on this application ', 'as well as',
                 a("Kathleen Coburn", href="http://psychology.ucmerced.edu/content/kathleen-coburn", target="_blank"),
                 'for her feedback and evaluation of the statistical methods related to this project.'),

               p('Atsushi Mizumoto would like to thank',
                 a("Dr. Luke Plonsky", href="http://oak.ucc.nau.edu/ldp3/", target="_blank"), 'and',
                 a("Dr. Yo In'nami", href="https://sites.google.com/site/yoinnami/", target="_blank"),
                 'for their support and feedback to create this web application.'),

               br(),



               h5('Authors'),

               HTML('<div style="clear: left;"><img src="http://kylehamilton.com/wp-content/uploads/2014/11/kyle80.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
               p(a("William Kyle Hamilton - University of California, Merced", href="http://www.kylehamilton.com", target="_blank")),
               p("William Kyle Hamilton maintains this application and has authored new features."),

               br(),
               HTML('<div style="clear: left;"><img src="http://kylehamilton.com/wp-content/uploads/2014/11/atsushi80.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
               p(a("Atsushi Mizumoto, PhD - Kansai University", href="http://mizumot.com", target="_blank"),br(),
                 p("Atsushi Mizumoto wrote the first version of this application; this application is a fork of the original which can be found", a("here", href="https://github.com/mizumot/meta", target="_blank"))


               ),



               p(br())

      )
    )
  )

