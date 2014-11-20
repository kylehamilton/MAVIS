
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinyAce)
library(meta)
library(metafor)
library(MAd)
library(MAc)
library(quantreg)
library(ggplot2)


shinyServer(function(input, output, session) {
  
  options(warn=-1)
  
  # 
  #reme <- reactive({input$model})
  
  
  
  # First calculation to be used later
  W.data <- reactive({
    
    dat <- read.csv(text=input$text, sep="\t")
    
    
    if (input$type == "mdms") {
      
      dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
                    m1i=M1, m2i=M2,
                    sd1i=SD1, sd2i=SD2,
                    data=dat, append=TRUE)
      
      dat$ES <- dat$yi
      dat$yi <- NULL
      dat$SV <- dat$vi # SV=sampling variances
      dat$vi <- NULL
      
      list(dat = dat) # To be used later
    }
    
    
    else if (input$type == "mdes") {
      
      df <- (dat$N1 + dat$N2) - 2
      j <- 1 - (3/(4 * df - 1))
      g <- j * dat$d
      dat$ES <- g
      
      dat$SV <- (((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2))))
      
      list(dat = dat) # To be used later
    }
    
    
    else if (input$type == "cor") {
      
      dat <- escalc(measure="ZCOR", ni=N, ri=r, data=dat, append=TRUE)
      dat$FZ <- dat$yi
      dat$yi <- NULL
      dat$SV <- dat$vi # SV=sampling variances
      dat$vi <- NULL
      
      list(dat = dat) # To be used later
      
    }
  })
  
  
  
  
  
  # Fixed effects model to be used later
  FE.est <- reactive({
    
    if (input$type == "mdms") {
      
      dat <- read.csv(text=input$text, sep="\t")
      
      dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
                    m1i=M1, m2i=M2,
                    sd1i=SD1, sd2i=SD2,
                    data=dat, append=TRUE)
      
      dat$ES <- dat$yi 
      dat$yi <- NULL
      dat$SV <- dat$vi 
      dat$vi <- NULL
      
      
      
      FE.res <- rma(ES, SV, method="FE", data=dat, slab=paste(Study))

      list(FE.res = FE.res) # To be used later
    }
    
    else if (input$type == "mdes") {
      
      dat <- read.csv(text=input$text, sep="\t")
      
      df <- (dat$N1 + dat$N2) - 2
      j <- 1 - (3/(4 * df - 1))
      g <- j * dat$d
      dat$ES <- g  
      
      dat$SV <- (((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))) 
      
      FE.res <- rma(ES, SV, method="FE", data=dat, slab=paste(Study))

      list(FE.res = FE.res) # To be used later
    }
    
    
    else if (input$type == "cor") {
      
      dat <- read.csv(text=input$text, sep="\t")
      
      dat <- escalc(measure="ZCOR", ni=N, ri=r, data=dat, append=TRUE)
      dat$FZ <- dat$yi
      dat$yi <- NULL
      dat$SV <- dat$vi # SV=sampling variances
      dat$vi <- NULL
      
      FE.res <- rma(FZ, SV, data=dat, method = "FE", slab=paste(Study))

      list(FE.res = FE.res) # To be used later
    }
  })
  
  
  
  
  
  # Random effects model to be used later
  RE.est  <- reactive({
    
    if (input$type == "mdms") {
      
      dat <- read.csv(text=input$text, sep="\t")
      
      dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
                    m1i=M1, m2i=M2,
                    sd1i=SD1, sd2i=SD2,
                    data=dat, append=TRUE)
      
      dat$ES <- dat$yi 
      dat$yi <- NULL
      dat$SV <- dat$vi 
      dat$vi <- NULL
      
      RE.res <- rma(ES, SV, method=input$model, data=dat, slab=paste(Study))

      list(RE.res = RE.res) # To be used later
    }
    
    
    else if (input$type == "mdes") {
      
      dat <- read.csv(text=input$text, sep="\t")
      
      df <- (dat$N1 + dat$N2) - 2
      j <- 1 - (3/(4 * df - 1))
      g <- j * dat$d
      dat$ES <- g  
      
      dat$SV <- (((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))) 
      
      RE.res <- rma(ES, SV, method=input$model, data=dat, slab=paste(Study))

      list(RE.res = RE.res) # To be used later
    }
    
    
    else if (input$type == "cor") {
      
      dat <- read.csv(text=input$text, sep="\t")
      
      dat <- escalc(measure="ZCOR", ni=N, ri=r, data=dat, append=TRUE)
      dat$FZ <- dat$yi
      dat$yi <- NULL
      dat$SV <- dat$vi # SV=sampling variances
      dat$vi <- NULL
      
      RE.res <- rma(FZ, SV, data=dat, method =input$model, slab=paste(Study))

      list(RE.res = RE.res) # To be used later
      
    }
  })
  
  
  
  
  
  ################################################
  # Displaying the first calculation
  ################################################
  
  data <- reactive({
    
    dat <- read.csv(text=input$text, sep="\t")
    
    
    if (input$type == "mdms") {
      
      dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
                    m1i=M1, m2i=M2,
                    sd1i=SD1, sd2i=SD2,
                    data=dat, append=TRUE)
      
      dat$ES <- round(dat$yi, 3)
      dat$yi <- NULL
      dat$SV <- round(dat$vi, 3) # SV=sampling variances
      dat$vi <- NULL
      
      cat("\n","ES = Effect size [Hedges's g]", "\n",
          "SV = Sampling variance [sqrt(SV) = Std err]", "\n", "\n"
      ) # ," W = Inverse variance weight", "\n", "\n"
      cat("---","\n")

      print(dat)
    }
    
    
    else if (input$type == "mdes") {
      
      df <- (dat$N1 + dat$N2) - 2
      j <- 1 - (3/(4 * df - 1))
      g <- j * dat$d
      dat$ES <- round(g, 3)
      
      dat$SV <- round((((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))),3)
      
      cat("\n","ES = Effect size [Hedges's g]", "\n",
          "SV = Sampling variance [sqrt(SV) = Std err]", "\n", "\n"
      ) # , " W = Inverse variance weight", "\n", "\n"
      cat("---","\n")

      print(dat)
    }
    
    
    else if (input$type == "cor") {
      
      dat <- escalc(measure="ZCOR", ni=N, ri=r, data=dat, append=TRUE)
      dat$FZ <- round(dat$yi,3)
      dat$yi <- NULL
      dat$SV <- round(dat$vi, 3) # SV=sampling variances
      dat$vi <- NULL
      
      cat("\n","FZ = Fisher's Z", "\n",
          "SV = Sampling variance [sqrt(SV) = Std err]", "\n", "\n")
      cat("---","\n")

      print(dat)
      
    }
  })
  
  
  
  
  
  ################################################
  # FE & RE model result
  ################################################
  
  fe <- reactive({
    
    if (input$type == "mdms") {
  
      FE.res <- FE.est()$FE.res
      
      cat("The FE model is a description of the K studies (Kovalchik, 2013).","\n")
      cat("---","\n")
      withProgress(message = 'Calculating', detail = 'Fixed effects model', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.05)
        }
      })
      FE.res
    }
    
    
    else if (input$type == "mdes") {
      
      FE.res <- FE.est()$FE.res
      
      cat("The FE model is a description of the K studies (Kovalchik, 2013).","\n",
          "---","\n")
      withProgress(message = 'Calculating', detail = 'Fixed effects model', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.05)
        }
      })
      FE.res
      
    }
    
    
    else if (input$type == "cor") { # Using different function here.
      
      dat <- read.csv(text=input$text, sep="\t")
      
      FE.res <- metacor(dat$r, dat$N)
      withProgress(message = 'Calculating', detail = 'Fixed effects model', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.05)
        }
      })
      FE.res
    }
  })
  
  
  
  
  
  re <- reactive({
    
    if (input$type == "mdms") {
      
      RE.res <- RE.est()$RE.res
      
      cat("The RE model regards the K studies as a sample of","\n")
      cat(" a larger universe of studies (Kovalchik, 2013).","\n")
      cat("---","\n")
      withProgress(message = 'Calculating', detail = 'Random effects model', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.05)
        }
      })
      RE.res
    }
    
    
    else if (input$type == "mdes") {
      
      RE.res <- RE.est()$RE.res
      
      cat("The RE model regards the K studies as a sample of","\n")
      cat(" a larger universe of studies (Kovalchik, 2013).","\n")
      cat("---","\n")
      withProgress(message = 'Calculating', detail = 'Random effects model', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.05)
        }
      })
      RE.res
      
    }
    
    
    else if (input$type == "cor") {
      
      cat("Both FE and RE model results are reported above.","\n","\n")
      
      cat("---","\n")
      
      cat("The FE model is a description of the K studies.","\n")
      cat("The RE model regards the K studies as a sample of","\n")
      cat(" a larger universe of studies (Kovalchik, 2013).","\n")
      
    }
  })
  
  
  
  
  
  ################################################
  # Forest plot
  ################################################
  
  makefePlot <- function(){
    
    if (input$type == "mdms") {
      
      FE.res <- FE.est()$FE.res
      
      forest(FE.res)
    }
    
    
    else if (input$type == "mdes") {
      
      FE.res <- FE.est()$FE.res
      
      forest(FE.res)
    }
    
    
    else if (input$type == "cor") {
      
      FE.res <- FE.est()$FE.res
      
      forest(FE.res, transf=transf.ztor)
      
    }
  }
  
  
  output$fePlot <- renderPlot(
    { 
      withProgress(message = 'Rendering', detail = 'Forest plot - fixed effects', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.05)
        }
      })
    print(makefePlot())
  })
  
  
  
  
  
  makerePlot <- function(){
    
    if (input$type == "mdms") {
      
      RE.res <- RE.est()$RE.res
      
      forest(RE.res)
      
    }
    
    
    else if (input$type == "mdes") {
      
      RE.res <- RE.est()$RE.res
      
      forest(RE.res)
      
    }
    
    
    else if (input$type == "cor") {
      
      RE.res <- RE.est()$RE.res
      
      forest(RE.res, transf=transf.ztor)
      
    }
  }
  
  
  output$rePlot <- renderPlot(
    {  
  withProgress(message = 'Rendering', detail = 'Forest plot - random effects', value = 0, {
    for (i in 1:10) {
      incProgress(1/10)
       Sys.sleep(0.05)
    }
  })

    print(makerePlot())
  })
  
  
  
  
  
  ################################################
  # Funnel plot
  ################################################
  
  makeFunFixPlot <- function(){
    
    if (input$type == "mdms") {
      
      FE.res <- FE.est()$FE.res
      
      metafor::funnel(trimfill(FE.res))
      
    }
    
    
    else if (input$type == "mdes") {
      
      FE.res <- FE.est()$FE.res
      
      metafor::funnel(trimfill(FE.res))
      
    }
    
    
    else if (input$type == "cor") {
      
      FE.res <- FE.est()$FE.res
      
      metafor::funnel(trimfill(FE.res))
      
    }
  }
  
  
  output$FunFixPlot <- renderPlot(
{ 
  withProgress(message = 'Rendering', detail = 'Funnel plot - fixed effects', value = 0, {
    for (i in 1:10) {
      incProgress(1/10)
       Sys.sleep(0.05)
    }
  })
    print(makeFunFixPlot())
  })
  
  
  
  
  
  makeFunRandPlot <- function(){
    
    if (input$type == "mdms") {
      
      RE.res <- RE.est()$RE.res
      
      metafor::funnel(trimfill(RE.res))
      
    }
    
    
    else if (input$type == "mdes") {
      
      RE.res <- RE.est()$RE.res
      
      metafor::funnel(trimfill(RE.res))
      
    }
    
    
    else if (input$type == "cor") {
      
      RE.res <- RE.est()$RE.res
      
      metafor::funnel(trimfill(RE.res))
      
    }
  }
  
  
  output$FunRandPlot <- renderPlot(
{
  withProgress(message = 'Rendering', detail = 'Funnel plot - random effects', value = 0, {
    for (i in 1:10) {
      incProgress(1/10)
      Sys.sleep(0.05)
    }
  })
    print(makeFunRandPlot())
})
  
  
  
  
  
  ################################################
  # Test of asymmetry & Fail-safe N
  ################################################
  
  asy <- reactive({
    
    dat <- read.csv(text=input$text, sep="\t")
    
    
    if (input$type == "mdms") {
      
      RE.res <- RE.est()$RE.res
      
      regt <- regtest(RE.res, model="lm")
      value <- fsn(y = RE.res$yi, v = RE.res$vi)
      
      return(list('No publication bias if p > .05 (Nonsignificant)' = regt,
                  'File drawer analysis' = value))
    }
    
    
    else if (input$type == "mdes") {
      
      RE.res <- RE.est()$RE.res
      
      regt <- regtest(RE.res, model="lm")
      value <- fsn(y = RE.res$yi, v = RE.res$vi)
      
      return(list('No publication bias if p > .05 (Nonsignificant)' = regt,
                  'File drawer analysis' = value))
      
    }
    
    
    else if (input$type == "cor") {
      
      RE.res <- RE.est()$RE.res
      
      regt <- regtest(RE.res, model="lm")
      value <- fsn(y = RE.res$yi, v = RE.res$vi)
      
      return(list('No publication bias if p > .05 (Nonsignificant)' = regt,
                  'File drawer analysis' = value))
    }
  })
  
  
  
  
  
  ################################################
  # Moderator analysis
  ################################################
  
  modAnalysis <- reactive({
    
    #if (input$moderator == 1) {
    
    
    if (input$type == "mdms") {
      
      dat <- read.csv(text=input$text, sep="\t")
      
      dat <- escalc(measure="SMD", n1i=N1, n2i=N2,
                    m1i=M1, m2i=M2,
                    sd1i=SD1, sd2i=SD2,
                    data=dat, append=TRUE)
      
      dat$ES <- dat$yi 
      dat$yi <- NULL
      dat$SV <- dat$vi 
      dat$vi <- NULL
      
      fixed <- MAd::macat(ES, SV, mod = Moderator, data=dat, method= "fixed")
      random <- MAd::macat(ES, SV, mod = Moderator, data=dat, method= "random")
      
      cat("---", "\n", "Fixed effects model:", "\n")
      print(fixed)
      
      cat("\n", "\n", "---", "\n", "Random effects model:", "\n")
      print(random)
      
    }
    
    
    else if (input$type == "mdes") {
      
      dat <- read.csv(text=input$text, sep="\t")
      
      df <- (dat$N1 + dat$N2) - 2
      j <- 1 - (3/(4 * df - 1))
      g <- j * dat$d
      dat$ES <- g 
      
      dat$SV <- (((dat$N1+dat$N2)/(dat$N1*dat$N2))+((dat$ES*dat$ES)/(2*(dat$N1+dat$N2)))) 
      
      fixed <- MAd::macat(ES, SV, mod = Moderator, data=dat, method= "fixed")
      random <- MAd::macat(ES, SV, mod = Moderator, data=dat, method= "random")
      
      cat("---", "\n", "Fixed effects model:", "\n")
      print(fixed)
      
      cat("\n", "\n", "---", "\n", "Random effects model:", "\n")
      print(random)
      
    }
    
    
    else if (input$type == "cor") {
      
      dat <- read.csv(text=input$text, sep="\t")
      
      dat <- escalc(measure="ZCOR", ni=N, ri=r, data=dat, append=TRUE)
      dat$FZ <- dat$yi 
      dat$yi <- NULL
      dat$SV <- dat$vi 
      dat$vi <- NULL
      
      
      dat$var.z <- var_z(dat$N) 
      
      # Fixed effects
      fixed <- MAc::macat(FZ, var.z, mod = Moderator, data=dat, ztor = TRUE, method= "fixed")
      z.fixed <- MAc::macat(FZ, var.z, mod = Moderator, data=dat, ztor = FALSE, method= "fixed") # Accurate z and p
      
      # Random effects
      random <- MAc::macat(FZ, var.z, mod = Moderator, data=dat, ztor = TRUE, method= "random")
      z.random <- MAc::macat(FZ, var.z, mod = Moderator, data=dat, ztor = FALSE, method= "random") # Accurate z and p
      
      
      cat("---", "\n", "Fixed effects model:", "\n")
      print(fixed)
      
      cat("\n", "Accurate z and p values:", "\n")
      print(z.fixed$Model[8:9])
      
      
      cat("\n", "\n", "---", "\n", "Random effects model:", "\n")
      print(random)
      
      cat("\n", "Accurate z and p values:", "\n")
      print(z.random$Model[8:9])
      
      
    }
    
    #} else {
    
    #cat("No moderator (subgroup) analysis is conducted.","\n")
    
    #}
    
  })
  
  
  
  
  
  ################################################
  # Categorical Moderator Graph
  ################################################
  
  ModFixGraph <- function(){
    
    if (input$type == "mdms") {
      
      dat <- W.data()$dat
      
      MAd::plotcat(ES, SV, mod = Moderator, data = dat, method= "fixed", modname= "Moderator")
      
    }
    
    
    else if (input$type == "mdes") {
      
      dat <- W.data()$dat
      
      MAd::plotcat(ES, SV, mod = Moderator, data = dat, method= "fixed", modname= "Moderator")
      
    }
    
    
    else if (input$type == "cor") {
      
      dat <- W.data()$dat
      
      MAd::plotcat(FZ, SV, mod = Moderator, data = dat, method= "fixed", modname= "Moderator")
      
    }
    
  }
  
  
  output$ModFixGraph <- renderPlot({
    
    withProgress(message = 'Rendering', detail = 'Categorical Moderator - fixed effects', value = 0, {
      for (i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.05)
      }
    })
    print(ModFixGraph())
  })
  
  
  
  
  
  ModRandGraph <- function(){
    
    if (input$type == "mdms") {
      
      dat <- W.data()$dat
      
      MAd::plotcat(ES, SV, mod = Moderator, data = dat, method= "random", modname= "Moderator")
      
    }
    
    
    else if (input$type == "mdes") {
      
      dat <- W.data()$dat
      
      MAd::plotcat(ES, SV, mod = Moderator, data = dat, method= "random", modname= "Moderator")
      
    }
    
    
    else if (input$type == "cor") {
      
      dat <- W.data()$dat
      
      MAd::plotcat(FZ, SV, mod = Moderator, data = dat, method= "random", modname= "Moderator")
      
    }
    
  }
  
  
  output$ModRandGraph <- renderPlot({
    withProgress(message = 'Rendering', detail = 'Categorical Moderator - random effects', value = 0, {
      for (i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.05)
      }
    })
    print(ModRandGraph())
  })
  
  
  
  
  
  ################################################
  # R session info
  ################################################
  
  info <- reactive({
    info1 <- paste("This analysis was performed on ", format(Sys.time(), "%A, %B %d %Y at %I:%M:%S %p"), ".", sep = "")
    info2 <- paste(strsplit(R.version$version.string, " \\(")[[1]][1], " was used for this session.", sep = "")
    info3 <- paste("Package version infomation for this session:")
    info4 <- paste("ggplot2", packageVersion("ggplot2"))
    info5 <- paste("MAc", packageVersion("MAc"))
    info6 <- paste("MAd", packageVersion("MAd"))
    info7 <- paste("meta", packageVersion("meta"))
    info8 <- paste("metafor", packageVersion("metafor"))
    info9 <- paste("quantreg", packageVersion("quantreg"))
    info10 <- paste("shiny", packageVersion("shiny"))
    info11 <- paste("shinyAce", packageVersion("shinyAce"))
    
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
    cat(sprintf(info3), "\n")
    cat(sprintf(info4), "\n")
    cat(sprintf(info5), "\n")
    cat(sprintf(info6), "\n")
    cat(sprintf(info7), "\n")
    cat(sprintf(info8), "\n")
    cat(sprintf(info9), "\n")
    cat(sprintf(info10), "\n")
    cat(sprintf(info11), "\n")
    withProgress(message = 'Rendering', detail = 'R session info', value = 0, {
      for (i in 1:10) {
        incProgress(1/10)
      }
    })
  })

  
  ################################################
  # server.R and ui.R connection
  ################################################
  output$model.out <- renderPrint({ input$model })

  output$info.out <- renderPrint({
    info()
  })
  
  output$data.out <- renderPrint({
    data()
  })
  
  output$fe.out <- renderPrint( {
    fe()
  })
  
  output$re.out <- renderPrint({
    re()
  })
  
  output$asy.out <- renderPrint({
    asy()
  })
  
  output$modAnalysis.out <- renderPrint({
    modAnalysis()
  })
  
})