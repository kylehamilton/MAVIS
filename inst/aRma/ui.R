library("shiny")
library("shinyAce")
library("shinyBS")
library("meta")
library("metafor")
#library("metamisc")
library("MAd")
library("MAc")
library("quantreg")
library("ggplot2")
library("compute.es")
library("SCMA")
library("SCRT")
library("weightr")
library("irr")

shinyUI(navbarPage("aRma:Altyapısı R'a dayalı Meta Analiz  ", windowTitle="aRma v1.1.3",
   
                   
                                   
                   tabPanel("Giriş", icon = icon("floppy-save", lib = "glyphicon"),
                            wellPanel(width = 5,
                              
                              radioButtons("type", strong("Analiz ve Veri Giriş Seçenekleri:"),
                                           
                                           list("Ham Ortalamaların Farkı (n, M, SD)" = "mdms",
                                                "Standartlaştırılmış Ortalamaların Farkı (n, Etki büyüklüğü d)" = "mdes",
                                                "Korelasyon (n, r)" = "cor",
                                                "İkili veriler"="or"
                                           ),
                              ),
                              bsTooltip("type", "Analizlerini koşmadan önce araç çubuğunda yer alan seçenekleri kontrol ediniz.",
                                        "right", trigger = "click", options = list(container = "body")),
                              helpText("İkili veriler seçeneğini kontrol ediniz, varsayılan seçenek logaritmik olasılık oranıdır (log odds ratio)"),
                              checkboxInput("moderator", label = ("Altgrup verisi mevcut ise işaretleyiniz."), value = T),
                              br(),
                              
                              submitButton("Yenile"),
                              helpText("Eğer datayı, modeli yada seçenekleri değiştirdiyseniz sonuçları yenileyiniz"),
                              br(),
                              actionButton("quit", "Çıkış", icon("sign-out")),
                              helpText("Uygulamadan çıkış")
                              
                              
                            ),
                            br(),
                            
                            p('Not: Sütünlar birbirinden tab ile ayrılmalıdır.Daha kolay kullanım için lütfen verileri excel dosyası olarak hazırlayın kopyalayın ve yapıştırın.'),
                            p("Excel veri dosyasının ilk satırı (başlıkları) örnek verilerin başlıkları ile birebir aynı olmalıdır."),
                            p("Örnek veri girişi için lütfen Örnek Veriler sekmesine gidiniz"),
                            
aceEditor("text", value="Veri\tModerator\tN1\tM1\tSD1\tN2\tM2\tSD2
Burhman ve ark. (2013)\tInternet\t38\t55.84\t18.23\t38\t43.58\t16.58
Fledderus ve ark. (2012)\tKitap\t125\t134.73\t16.17\t126\t115.7\t18.76
Gluck & Maerker (2011)\tInternet\t28\t38.77\t5.38\t21\t40.67\t6.78
Hesser ve ark. (2012)\tInternet\t33\t44.27\t9.69\t32\t36.81\t10.95
Hjeff/Hayes (2012)\tKitap\t103\t135.81\t18.72\t109\t123.18\t19.39
Johnson ve ark. (2012)\tKitap\t5\t77.4\t9.1\t8\t62.3\t24.8
Lappalainen ve ark. (2013)\tInternet\t12\t55.73\t6.25\t12\t53.67\t9.6
Morledge ve ark. (2013)\tInternet\t184\t3.86\t0.82\t184\t3.65\t0.89
Muto ve ark. (2011)\tKitap\t30\t44.3\t6.67\t31\t43.48\t8.63
Thorsell ve ark. (2011)\tKitap\t52\t62.3\t20.91\t38\t50\t19.11", mode="r", theme="monokai"),
h6("Bu örneğin veri seti için bakınız:", align = "right"),
h6("Cavanagh, K., Strauss, C., Forder, L., & Jones, F. (2014). Can mindfulness and acceptance be learnt by self-help ?: A systematic review and meta-analysis of mindfulness and acceptance-based self-help interventions. Clinical Psychology Review", align = "right"),
                            
br(),
                            
                            h3("Etki Büyüklüğü ve Örneklem Varyansı (iç varyans)"),
                            
                            verbatimTextOutput("data.out"),
                            
                            br(),
                            
                            h3("Sabit Etki Modeli (FE Model)"),
                            verbatimTextOutput("fe.out"),
                            
                            br(),
                            
                            h3("Rastgele Etkiler Modeli"),
                            verbatimTextOutput("re.out"),
                            
                            p('[Heterojenliği Tanımlama Çabaları]',
                              br(),
                              br(),
                              'I^2 (Etki büyüklüğü çalışmalar arasında ne kadar değişiyor?)', br(),
                              "25-50: Az Değisken", br(),
                              '50-75: Değisken', br(),
                              '75-100: Çok Değişken', br(),
                              br(),
                              'Heterojenlik testi: p-val < .05 (homojen degil)', br(),
                              br(),
                              'H > 1: Açıklanmamış heterojenlik var.'
                            ),
                            
                            br(),
                            br(),
                            
                            
                            h3("Diyagram (Sabit Etki Modeli)"),
                            downloadButton('downloadfePlot', 'Diyagrami pdf olarak indir'),
                            plotOutput("fePlot", height = "550px"),
                            
                            
                            
                            br(),
                            
                            h3("Diyagram (Rastgele Etkiler Modeli)"),
                            downloadButton('downloadrePlot', 'Diyagrami pdf olarak indir'),
                            plotOutput("rePlot", height = "550px"),
                            
                            br(),
                            
                            h3("Huni grafiği (Sabit Etki Modeli)"),
                            downloadButton('downloadFunFixPlot', 'Grafiği pdf olarak indir'),
                            plotOutput("FunFixPlot"),
                            p('Eğer içi beyaz olan daireler varsa bunlar grafiğe kırpma ve doldurma metodu ile eklenmiştir.'),
                            
                            br(),
                            
                            h3("Huni grafigi (Rastgele Etkiler Modeli)"),
                            downloadButton('downloadFunRandPlot', 'Grafiği pdf olarak indir'),
                            plotOutput("FunRandPlot"),
                            p('Eğer içi beyaz olan daireler varsa bunlar grafiğe kırpma ve doldurma metodu ile eklenmiştir.'),
                            br(),
                            
br(),

                            h3("Yayın Yanlılığı"),
                            verbatimTextOutput("asy.out"), # regression tests for funnel plot asymmetry
                            p('Korumalı N: Bir meta-analiz çalışması yapılsın ve bulunan p değeri istatistiksel olarak anlamlı olsun. Korumalı N , çıkan anlamlı sonucun (p<.05), istatistiksel olarak anlamsızlaşması için (p>.05) kaç adet etkisiz çalışmanın eklenmesini gerektiğini verir. Eğer bu sayı büyük ise , bulunan anlamlı farkın yayın yanlılığına karşı dirençli olduğu söylenebilir. Daha detali yorumlar için bknz: ',
                              a('(Oswald & Plonsky, 2010, p. 92)', href='http://dx.doi.org/10.1017/S0267190510000115', target="_blank"), '.'),
                            br(),
h3("Yayın yanlılığı için ağırlıklandırmalı (weight-function) model"),
p('p-değeri kesme noktasını değiştirmek için ağırlıklandırmalı model ayarlarına gidiniz'),
verbatimTextOutput("wfm.out"), # weightr output
p('Anlamlı bir en çok olabilirlik oran testi p-değeri, yayın yanlılığına kanıt teşkil eder' ,
  a('(Vevea & Hedges, 1995)', href='http://dx.doi.org/10.1007/BF02294384', target="_blank"), '.'),
# selectizeInput(inputId = "steps", label="Select at least one p-value cutpoint to include in your model. To include a cutpoint not provided, type it in and press enter.", 
#                choices=c(0.001,
#                          0.005,
#                          0.010,
#                          0.020,
#                          0.025,
#                          0.050,
#                          0.100,
#                          0.200,
#                          0.250,
#                          0.300,
#                          0.350,
#                          0.500,
#                          0.600,
#                          0.650,
#                          0.700,
#                          0.750,
#                          0.800,
#                          0.900,
#                          0.950,
#                          0.975,
#                          0.980,
#                          0.990,
#                          0.995,
#                          0.999),
#                multiple=TRUE,
#                selected=c(0.025), options=list(create=TRUE,openOnFocus=TRUE)),p(),
br(),
br(),

# Display this only if "moderator" is checked
conditionalPanel(condition = "input.moderator == true",
h3("Alt grup analizleri"),
verbatimTextOutput("modAnalysis.out")
                            ),
                            
                            br(),
                            
                            # Display this only if "moderator" is checked
                            conditionalPanel(condition = "input.moderator == true",
                                             h4("Kategorik Moderator grafiği (Sabit Etki Modeli)"),
                                             plotOutput("ModFixGraph")
                            ),
                            
                            br(),
                            
                            # Display this only if "moderator" is checked
                            conditionalPanel(condition = "input.moderator == true",
                                             h4("Kategorik Moderator Grafiği(Rasal Etki Modeli)"),
                                             plotOutput("ModRandGraph")
                            ),
                            
                            br(),
                            br(),
                            
                            strong('Oturum Bilginiz'),
                            verbatimTextOutput("info.out")
                   ),
                   
                   
                   
                   tabPanel("Örnek Veriler", icon = icon("table", lib = "font-awesome"),
                            
                            p('Not: Sutunlar birbirinden tab ile ayrılmalıdır . Daha kolay kullanım için lütfen verileri excel dosyası olarak hazırlayın kopyalayın ve yapıştırın'),
                            
                            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Excel veri dosyasının ilk satırı (başlıkları) örnek verilerin başlıkları ile birebir aynı olmalıdır.</div></b>")),
                                                        
                            br(),
                            
                            p(strong("Ham Ortalamalar (n, M, SD)")),
p("Bu örneğin verileri için bakınız:"),
p("Cavanagh, K., Strauss, C., Forder, L., & Jones, F. (2014). Can mindfulness and acceptance be learnt by self-help?: A systematic review and meta-analysis of mindfulness and acceptance-based self-help interventions. Clinical Psychology Review."),
aceEditor("text1", value="Veri\tModerator\tN1\tM1\tSD1\tN2\tM2\tSD2
Burhman ve ark. (2013)\tInternet\t38\t55.84\t18.23\t38\t43.58\t16.58
Fledderus ve ark. (2012)\tKitap\t125\t134.73\t16.17\t126\t115.7\t18.76
Gluck & Maerker (2011)\tInternet\t28\t38.77\t5.38\t21\t40.67\t6.78
Hesser ve ark. (2012)\tInternet\t33\t44.27\t9.69\t32\t36.81\t10.95
Hjeff/Hayes (2012)\tKitap\t103\t135.81\t18.72\t109\t123.18\t19.39
Johnson ve ark. (2012)\tKitap\t5\t77.4\t9.1\t8\t62.3\t24.8
Lappalainen ve ark. (2013)\tInternet\t12\t55.73\t6.25\t12\t53.67\t9.6
Morledge ve ark. (2013)\tInternet\t184\t3.86\t0.82\t184\t3.65\t0.89
Muto ve ark. (2011)\tKitap\t30\t44.3\t6.67\t31\t43.48\t8.63
Thorsell ve ark. (2011)\tKitap\t52\t62.3\t20.91\t38\t50\t19.11", mode="r", theme="monokai"),
              
                            
                            br(),
                            p(strong("Standardlaştırılmış Ortalamalar  (n, Effect size d)")),
aceEditor("text2", value="Veri\tModerator\tN1\tN2\td\nStudy 01\tJH\t30\t30\t-1.431\nStudy 02\tUNI\t23\t24\t-0.3423\nStudy 03\tSH\t83\t81\t-0.3481\nStudy 04\tSH\t21\t21\t-0.5171\nStudy 05\tSH\t15\t15\t-0.3837\nStudy 06\tSH\t7\t7\t-2.8293\nStudy 07\tSH\t28\t28\t-0.4086\nStudy 08\tUNI\t40\t40\t-0.1845\nStudy 09\tUNI\t18\t17\t-1.2039\nStudy 10\tUNI\t21\t25\t0.1093\nStudy 11\tUNI\t26\t26\t-0.4298\nStudy 12\tUNI\t49\t48\t-0.605\nStudy 13\tUNI\t27\t27\t-1.9907\nStudy 14\tSH\t41\t34\t-0.4422\nStudy 15\tUNI\t58\t57\t-0.3758\nStudy 16\tSH\t60\t63\t-0.5508\nStudy 17\tUNI\t15\t15\t-1.4719\nStudy 18\tJH\t37\t142\t-0.9136\nStudy 19\tJH\t27\t54\t-1.5079\nStudy 20\tJH\t35\t39\t-1.111\nStudy 21\tJH\t32\t34\t-1.4368\nStudy 22\tJH\t62\t60\t-0.0831\nStudy 23\tSH\t39\t39\t-0.9588\nStudy 24\tSH\t213\t39\t-1.3729\nStudy 25\tUNI\t34\t42\t-0.6976\nStudy 26\tUNI\t77\t56\t-0.6642\nStudy 27\tUNI\t28\t28\t-0.1839\nStudy 28\tUNI\t33\t36\t0.0886\nStudy 29\tUNI\t66\t66\t-1.0326", mode="r", theme="monokai"),

                            
                            br(),
                            p(strong("Korelasyonlar (n, r)")),
p("Bu örneğin verileri için bakınız:"),
p("Widman, L., Noar, S. M., Choukas-Bradley, S., & Francis, D. B. (2014). Adolescent Sexual Health Communication and Condom Use : A Meta-Analysis. Health Psychology."),
aceEditor("text3", value="Veri\tN\tr\tModerator
          Abraham-1992\t351\t0.01\tmix
          Baele-2001\t163\t0.4\tmix
          Barthlow-1995\t328\t0.162\tmix
          Basen-Engquist-1999\t1718\t0.225\tmix
          Brown-2008\t1218\t0.334\tmix
          Bryan-2002\t170\t0.71\tboys
          Bryan-2002\t123\t0.78\tgirls
          Crosby-2002\t522\t0.102\tgirls
          Crosby-2003\t144\t0.218\tgirls
          Crosby-2008\t566\t0.175\tgirls
          Deardorff-2010\t377\t0.15\tboys
          Deardorff-2010\t462\t0.14\tgirls
          DePadilla-2011\t701\t0.535\tmix
          DiClemente-1991\t79\t0.418\tmix
          DiClemente-1996\t116\t0.311\tmix
          DiIorio-2001\t116\t0.177\tmix
          Donald-1994\t395\t0.054\tboys
          Donald-1994\t505\t0.098\tgirls
          Gallupe-2009\t863\t0.112\tboys
          Gallupe-2009\t1143\t0.298\tgirls
          Grossman-2008\t446\t0.498\tmix
          Gutiérrez-2000\t148\t0.14\tboys
          Gutiérrez-2000\t185\t0.29\tgirls
          Guzmán-2003\t34\t0.334\tmix
          Harrison-2012\t91\t0.491\tboys
          Harrison-2012\t64\t0.377\tgirls
          Hart-2005\t100\t0.07\tboys
          Magura-1994\t421\t0.4\tboys
          Maxwell-1995\t100\t0\tmix
          Overby-1994\t60\t0.414\tgirls
          Rickman-1994\t1439\t0.226\tmix
          Roye-1998\t452\t0\tgirls
          Shoop-1994\t45\t0.514\tmix
          Shrier-1999\t22\t0.492\tgirls
          Small-2010\t189\t0.329\tgirls
          Troth-2000\t26\t0.28\tboys
          Troth-2000\t50\t0.23\tgirls
          Tschann-2010\t393\t0.068\tmix
          van Empelen-2006\t108\t0.07\tmix
          Whitaker-1999\t372\t0.077\tmix
          Wilson-1994\t241\t0.012\tboys", mode="r", theme="monokai"),


                            br(),
                            p(strong("İkili veriler (olay görülme, olay görülmeme, n)")),
aceEditor("text4", value="Veri\tModerator\tupoz\tuneg\tNU\tkpoz\tkneg\tNK\nStudy 01\tra\t4\t119\t123\t11\t128\t139\nStudy 02\tra\t6\t300\t306\t29\t274\t303\nStudy 03\tra\t3\t228\t331\t11\t209\t220\nStudy 04\tsys\t17\t1699\t1716\t65\t1600\t1665\nStudy 05\tsys\t5\t2493\t2498\t3\t2338\t2341\nStudy 06\tra\t29\t7470\t7499\t45\t7232\t7277", mode="r", theme="monokai"),

                            br()
                            
                   ),
                   

#########################NZ start
tabPanel("Puanlayıcılar-arası güvenirlik", icon = icon("bullseye", lib = "font-awesome"),
         
         p('Not: Sütünlar birbirinden tab ile ayrılmalıdır.Daha kolay kullanım için lütfen verileri excel dosyası olarak hazırlayın kopyalayın ve yapıştırın.'),
         
         p("Excel veri dosyasının ilk satırı (başlıkları) örnek verilerin başlıkları ile birebir aynı olmalıdır"),
         
         br(),
         
         p(strong("PAG (iki puanlayıcı, kategorik puanlama)")),
         aceEditor("text5", value="Veri\tModerator\tPuanlayıcı 1\tPuanlayıcı 2\nVeri 01\tACT\t1\t1\nVeri 02\tACT\t3\t1\nVeri 03\tACT\t1\t1\nVeri 04\tACT\t1\t1\nVeri 05\tACT\t1\t1\nVeri 06\tACT\t2\t2\nVeri 07\tACT\t2\t2\nVeri 08\tACT\t1\t1\nVeri 09\tACT\t4\t4\nVeri 10\tACT\t1\t1\nVeri 11\tACT\t1\t1\nVeri 12\tACT\t1\t9\nVeri 13\tACT\t1\t1\nVeri 14\tACT\t1\t1\nVeri 15\tACT\t1\t1\nVeri 16\tACT\t1\t1", mode="r", theme="monokai"),
         
         verbatimTextOutput("cat2.out"),
         
         br(),
         br(),
         p(strong("PAG (3 veya daha fazla puanlayıcı, kategorik puanlama )")),
         aceEditor("text6", value="Veri\tModerator\tPuanlayıcı 1\tPuanlayıcı 2\tPuanlayıcı 3\nVeri 01\tACT\t1\t1\t1\nVeri 02\tACT\t3\t1\t3\nVeri 03\tACT\t1\t1\t1\nVeri 04\tACT\t1\t1\t3\nVeri 05\tACT\t1\t1\t1\nVeri 06\tACT\t2\t2\t2\nVeri 07\tACT\t2\t2\t2\nVeri 08\tACT\t1\t1\t1\nVeri 09\tACT\t4\t4\t4\nVeri 10\tACT\t1\t1\t1\nVeri 11\tACT\t1\t1\t1\nVeri 12\tACT\t1\t9\t4\nVeri 13\tACT\t1\t1\t1\nVeri 14\tACT\t1\t1\t1\nVeri 15\tACT\t1\t1\t1\nVeri 16\tACT\t1\t1\t1", mode="r", theme="monokai"),
         
         verbatimTextOutput("cat3.out"),
         
         
         br(),
         p(strong("PAG (2 puanlayıcı sürekli değişken)")),
         aceEditor("text7", value="Veri\tModerator\tPuanlayıcı 1\tPuanlayıcı 2\nVeri 01\tPF\t0\t.667\nVeri 02\tPF\t.667\t.667\nVeri 03\tPF\t0\t0\nVeri 04\tPF\t0\t0\nVeri 05\tPF\t.6\t.6\nVeri 06\tPF\t.333\t.333\nVeri 07\tPF\t1\t1\nVeri 08\tPF\t0\t0\nVeri 09\tPF\t0\t0\nVeri 10\tPF\t.444\t.269\nVeri 11\tPF\t.667\t.667\nVeri 12\tPF\t.667\t.667\nVeri 13\tPF\t.6\t.6\nVeri 14\tPF\t.667\t.667\nVeri 15\tPF\t0\t0\nVeri 16\tPF\t.566\t.566", mode="r", theme="monokai"),
         
         verbatimTextOutput("cont2.out"),
         
         
         br(),
         p(strong("PAG (3 veya daha fazla puanlayıcı, sürekli değişken)")),
         aceEditor("text8", value="Veri\tModerator\tPuanlayıcı 1\tPuanlayıcı 2\tPuanlayıcı 3\nVeri 01\tPF\t0\t.667\t1\nVeri 02\tPF\t.667\t.667\t.667\nVeri 03\tPF\t0\t0\t0\nVeri 04\tPF\t0\t0\t0\nVeri 05\tPF\t.6\t.6\t.6\nVeri 06\tPF\t.333\t.333\t0\nVeri 07\tPF\t1\t1\t1\nVeri 08\tPF\t0\t0\t0\nVeri 09\tPF\t0\t0\t0\nVeri 10\tPF\t.444\t.269\t.269\nVeri 11\tPF\t.667\t.667\t.667\nVeri 12\tPF\t.667\t.667\t.667\nVeri 13\tPF\t.6\t.6\t.6\nVeri 14\tPF\t.667\t.667\t.667\nVeri 15\tPF\t0\t0\t0\nVeri 16\tPF\t.566\t.566\t.566", mode="r", theme="monokai"),
         
         verbatimTextOutput("cont3.out"),
         
         br()
         
),

#########################NZ stop

navbarMenu("Model seçenekleri ve ayarları ", icon = icon("cog", lib = "font-awesome"),
                              #                       tabPanel("Bayesian Model Options", icon = icon("tasks", lib = "font-awesome"),
                              #                                
                              #                      strong('Bayesian Analysis Options'),
                              #                      selectInput("bayoption1", label = h3("Run Bayesian Analysis"), 
                              #                                  choices = list("No" = FALSE, "Yes" = TRUE)
                              #                                
                              #                       )),
                              tabPanel("Korelasyon modeli seçenekleri", icon = icon("line-chart", lib = "font-awesome"),
                                       
                                       radioButtons("cormeasures", strong("Korelasyon modeli seçenekleri"),
                                                    c("Ham korelasyon katsayısı " = "COR",
                                                      "Düzeltilmiş ham korelasyon, Olkin & Pratt, 1958" = "UCOR",
                                                      "Fisher Z dönüşümlü korelasyon" = "ZCOR"
                                                    ), selected = "ZCOR"),
                                       p(h6('Aksi belirtilmedikçe, sisteme sunulan korelasyon verisi Fisher Z dönüşümlü korelasyon olarak işlem görür.')),
                                       
                                       verbatimTextOutput('cormeasures.out')
                                       
                              ),
                              tabPanel("İkili veri seçenekleri ", icon = icon("ellipsis-v", lib = "font-awesome"),
                                       
                                       radioButtons("dichotomousoptions", strong("İndeks seçimi"),
                                                    c("log risk oranı" = "RR",
                                                      "log olasılık oranı" = "OR",
                                                      "Risk farkı" = "RD",
                                                      "arcsine karekök transorfmlu risk farkı (Rücker et al., 2009)." = "AS",
                                                      "Peto metodu ile tahminlenmiş olasılık oranı (Yusuf et al., 1985)." = "PETO",
                                                      "Standardlaştırılmış ortalama farkı tahmini olarak probit transformlu risk farkı." = "PBIT",
                                                      "Standardlaştırılmış ortalama farkı tahmini olarak transforme olasılık oranı (normal dağılım)." = "OR2DN",
                                                      "Standardlaştırılmış ortalama farkı tahmini olarak transforme olasılık oranı  (logistic dağılım)." = "OR2DL"
                                                    ), selected = "OR"),
                                       p(h6('Aksi belirtilmediğinde, analizlerin yapıldığı R paketi log olasılık oranı kullanır')),
                                       
                                       verbatimTextOutput('dichotomousoptions.out')
                                       
                              ),
                              tabPanel("Rassal etki modeli tahminleyici seçenekleri", icon = icon("random", lib = "glyphicon"),
                                       
                                       radioButtons("model", strong("Tahminleyici seçenekleri"),
                                                    c("DerSimonian-Laird" = "DL",
                                                      "Hedges" = "HE",
                                                      "Hunter-Schmidt" = "HS",
                                                      "Sidik-Jonkman" = "SJ",
                                                      "Maximum-likelihood" = "ML",
                                                      "Restricted maximum-likelihood" = "REML",
                                                      "Empirical Bayes (Paule-Mandel)" = "EB",
                                                      "Generalized Q-statistic" = "GENQ"
                                                    ), selected = "REML"),
                                       p(h6('Aksi belirtilmediğinde, analizlerin yapıldığı R paketi REML kullanır')),
                                       
checkboxInput("khadjust", label = "Knapp & Hartung Düzeltmesi", value = FALSE),
p(h6('Aksi belirtilmedikçe Knapp & Hartung düzeltmesi yapılmaz')),
p("The Knapp and Hartung (2003) metodunun standard hataları düzeltmesi beklenir."),


h3("References"),
p("Knapp, G., & Hartung, J. (2003). Improved tests for a random effects meta-regression with a single covariate. Statistics in Medicine, 22, 2693–2710.")

                                       
                                       )),
                   
navbarMenu("Yayın Yanlılığı ", icon = icon("book", lib = "font-awesome"),
                              tabPanel("Kırpma ve Doldurma", icon = icon("chevron-right", lib = "font-awesome"),
                                       
                                       radioButtons("trimfillopt", strong("Kırpma ve Doldurma Tahminleyiceleri"),
                                                    c("L0" = "L0",
                                                      "R0" = "R0",
                                                      "Q0" = "Q0"
                                                    ), selected = "L0"),
                                       p(h6('Bu üç farklı tahminleyici için bak. Duval ve Tweedie (2000a, 2000b). Aksi belirtilmediyse, L0. ')),
                                       
                                       verbatimTextOutput('trimfillopt.out'),
                                       h3("References"),
                                       p("Duval, S. J., & Tweedie, R. L. (2000a). Trim and fill: A simple funnel-plot-based method of testing and adjusting for publication bias in meta-analysis. Biometrics, 56, 455–463."),
                                       p("Duval, S. J., & Tweedie, R. L. (2000b). A nonparametric trim and fill method of accounting for publication bias in meta-analysis. Journal of the American Statistical Association, 95, 89–98."),
                                       p("Duval, S. J. (2005). The trim and fill method. In H. R. Rothstein, A. J. Sutton, & M. Borenstein (Eds.) Publication bias in meta-analysis: Prevention, assessment, and adjustments (pp. 127–144). Chichester, England: Wiley.")
                                       
                                       
                              ),
                              
tabPanel("Huni grafiğinin asitmeriliği için regresyon testi", icon = icon("chevron-right", lib = "font-awesome"),
                                       verticalLayout(
                                         
                                         wellPanel(
                                           fluidRow(
                                             column(4,
                                                    p(strong("Regresyon testi seçenekleri")),
                                                    radioButtons("regtestpredictor", strong("Yordayıcı"),
                                                                 c("standard hata" = "sei",
                                                                   "örneklem varyansı" = "vi",
                                                                   "örneklem sayısı" = "ni",
                                                                   "örneklem sayısının tersi (inverse)" = "ninv"
                                                                 ), selected = "sei"),
                                                    radioButtons("regtestmodeltype", strong("Model Seçimi "),
                                                                 c("Weighted Regression with a Multiplicative Dispersion Term" = "lm",
                                                                   "Meta-analytic Models" = "rma"
                                                                 ), selected = "lm"),
                                                    
                                                    
                                                    p(br())
                                             ),
                                             column(4,
                                                    strong('Huni grafiği seçenekleri'),
                                                    checkboxInput("contourenhancedbox", "Contour enhanced (Dış çizgisi geliştirilmiş) grafik", FALSE),
                                                    helpText("Eğer huni grafiğini dış çizgisi geliştirilmiş (Peters et al., 2008) olarak rapor etmek istiyorsanız işaretleyiniz."),
                                                    checkboxInput("regtestfullmodel", "Regresyon modeli sonuçları", FALSE),
                                                    helpText("Eğer regresyon modelinin bütün sonuçlarını görmek istiyorsanız işaretleyiniz.")
                                                    
                                             )
                                             
                                           )),
                                         p("Yayın yanlılığını tespit etmek amaçlı diğer metodlar için bknz: (Jin, Zhou, & He, 2015)"),
                                         h3("Referaslar"),
                                         p("Egger, M., Davey Smith, G., Schneider, M., & Minder, C. (1997). Bias in meta-analysis detected by a simple, graphical test. British Medical Journal, 315, 629--634."),
                                         p("Jin, Zhi-Chao, Zhou, Xiao-Hua & He, Jia (2015). Statistical methods for dealing with publication bias in meta-analysis. Statistics in Medicine, 34, 343-360."),
                                         p("Peters, J. L., Sutton, A. J., Jones, D. R., Abrams, K. R., & Rushton, L. (2008). Contour-enhanced meta-analysis funnel plots help distinguish publication bias from other causes of asymmetry. Journal of Clinical Epidemiology, 61(10), 991–-996."),
                                         p("Sterne, J. A. C., & Egger, M. (2001). Funnel plots for detecting bias in meta-analysis: Guidelines on choice of axis. Journal of Clinical Epidemiology, 54(10), 1046--1055."),
                                         br()
                                         
                                       )
                                       
                              ),
tabPanel("Ağırlıklandırmalı Model", icon = icon("chevron-right", lib = "font-awesome"),
         p(strong("Ağırlıklandırmalı Model Seçenekleri")),
         p("Modelde yer alması için en az bir p-değeri kesme noktası seçin. Seçeneklerde olmayan bir p-değeri için değeri girin ve enter'a basın"),
         p("Daha gelişmiş bir uygulama için yazarların yazılımına şurdan ulaşabilirsiniz: https://vevealab.shinyapps.io/WeightFunctionModel/"),
         selectizeInput(inputId = "steps", label="", 
                        choices=c(0.001,
                                  0.005,
                                  0.010,
                                  0.020,
                                  0.025,
                                  0.050,
                                  0.100,
                                  0.200,
                                  0.250,
                                  0.300,
                                  0.350,
                                  0.500,
                                  0.600,
                                  0.650,
                                  0.700,
                                  0.750,
                                  0.800,
                                  0.900,
                                  0.950,
                                  0.975,
                                  0.980,
                                  0.990,
                                  0.995,
                                  0.999),
                        multiple=TRUE,
                        selected=c(0.025), options=list(create=TRUE,openOnFocus=TRUE)
         ),
         h4("Referanslar"),
         p("Coburn, K. M. & Vevea, J. L. (2015). Publication bias as a function of study characteristics. Psychological Methods, 20(3), 310."),
         p("Vevea, J. L. & Hedges, L. V. (1995). A general linear model for estimating effect size in the presence of publication bias. Psychometrika, 60(3), 419-435."),
         p("Vevea, J. L. & Woods, C. M. (2005). Publication bias in research synthesis: Sensitivity analysis using a priori weight functions. Psychological Methods, 10(4), 428-443."),
         p("Coburn, K. M. & Vevea, J. L. (2017). weightr: Estimating Weight-Function Models for Publication Bias. R package version 1.1.2.
           https://CRAN.R-project.org/package=weightr"),
         br()
         
         ),

tabPanel("Eksik çalışma (File Drawer) Analizleri ", icon = icon("chevron-right", lib = "font-awesome"),
                                       
                                       radioButtons("filedraweranalysis", strong("Eksik Çalışma Analizleri"),
                                                    c("Rosenthal" = "Rosenthal",
                                                      "Orwin" = "Orwin",
                                                      "Rosenberg" = "Rosenberg"
                                                    ), selected = "Rosenthal"),
                                       p(h6('Eksik çalışma analizleri için metod seçimi. Aksi belirtilmedikçe Rosental yöntemi kullanılır.')),
                                       p('Rosenthal metodu için hesaplamalar Stouffer yönetimini kullanır (Rosenthal, 1979)'),
                                       p('Orwin (1983).'),
                                       p('Rosenberg (2005)'),
                                       
                                       
         
         
         
         
         
         
         
         verbatimTextOutput('filedraweranalysis.out'),
                                       h3("Referans"),
                                       p("Rosenthal, R. (1979). The file drawer problem and tolerance for null results. Psychological Bulletin, 86, 638--641."),
                                       p("Orwin, R. G. (1983). A fail-safe N for effect size in meta-analysis. Journal of Educational Statistics, 8, 157--159."),
                                       p("Rosenberg, M. S. (2005). The file-drawer problem revisited: A general weighted method for calculating fail-safe numbers in meta-analysis. Evolution, 59, 464--468.")
                                       
                                       
                                       )),
                   navbarMenu("Etki Büyüklüğü Hesaplaması", icon = icon("calculator", lib = "font-awesome"),
                              tabPanel("Ortalama, standard sapma(SS) ve örneklem sayısı", icon = icon("chevron-right", lib = "font-awesome"),
                                       
                                       verticalLayout(
                                         
                                         wellPanel(
                                           fluidRow(
                                             column(3,
                                                    p(strong("Grup 1:")),
                                                    
                                                    numericInput("nx", " Örneklem Sayısı (n)", 21),
                                                    
                                                    numericInput("mx", " Ortalama", 61.33),
                                                    
                                                    numericInput("sdx", " SS", 16.43),
                                                    
                                                    p(br())
                                             ),
                                             column(4, offset = 1,
                                                    p(strong("Grup 2:")),
                                                    
                                                    numericInput("ny", "Örneklem Sayısı (n)", 24),
                                                    
                                                    numericInput("my", " Ortalama", 59.79),
                                                    
                                                    numericInput("sdy", " SS", 18.50),
                                                    
                                                    p(br())
                                             ),
                                             column(4,
                                                    strong('Option:'),
                                                    
                                                    
                                                    checkboxInput("varequal", "Varyans eşitliği varsayımı ile t test", FALSE),
                                                    
                                                    
                                                    checkboxInput("vartest", "Varyans eşitliği testi sonuçları", FALSE),
                                                    helpText("Yenilemek için tıklayınız."),
                                                    submitButton("Yenile")
                                             )
                                             
                                           )),
                                         
                                         h3("Verileri kontrol etme"),
                                         tableOutput("values"),
                                         
                                         br(),
                                         
                                         h3("Ortalamaların farkı ve %95 güven aralığı"),
                                         verbatimTextOutput("difference.out"),
                                         
                                         br(),
                                         
                                         h3("t-test"),
                                         verbatimTextOutput("ttest.out"),
                                         h3(""),
                                         verbatimTextOutput("vartest.out"),
                                         
                                         br(),
                                         
                                         h3("Etki büyüklüğü"),
                                         verbatimTextOutput("es.out"),                   
                                         br()
                                         
                                       )
                                       
                              ),
                              tabPanel("ANCOVA F istatistiğinden etki büyüklüğü hesaplama", icon = icon("chevron-right", lib = "font-awesome"),
                                       verticalLayout(
                                         
                                         wellPanel(
                                           fluidRow(
                                             column(3,
                                                    p(strong("ANCOVA F istatistiğinden etki büyüklüğü")),
                                                    
                                                    numericInput("ancovaf", " ANCOVA nın F değeri", 21),
                                                    
                                                    numericInput("ancovafn1", " Uygulama grubu örneklem sayısı", 50),
                                                    
                                                    numericInput("ancovafn2", " Kontrol grubu örneklem sayısı", 50),
                                                    
                                                    p(br())
                                             ),
                                             column(4, offset = 1,
                                                    
                                                    numericInput("anovafcovar", " Yordayıcı-yordanan yada multiple korelasyon", 0.24),
                                                    
                                                    numericInput("anovafcovarnum", " Yordayıcı Sayısı", 3),
                                                    
                                                    numericInput("sdy", " SS", 18.50),
                                                    helpText("Yenilemek için tıklayınız."),
                                                    submitButton("Yenile"),
                                                    p(br())
                                             )
                                             
                                             
                                           )),
                                         
                                         
                                         h3("Etki Büyüklüğü"),
                                         verbatimTextOutput("ancovaf.out"),
                                         p(br())
                                         
                                       )
                                       
                              ),
                              tabPanel("ANCOVA düzeltilmiş (adjusted) ortalamalardan Etki büyüklüğü", icon = icon("chevron-right", lib = "font-awesome"),
                                       verticalLayout(
                                         
                                         wellPanel(
                                           fluidRow(
                                             column(3,
                                                    p(strong("ANCOVA düzeltilmiş ortalamalardan Etki büyüklüğü")),
                                                    
                                                    numericInput("ancovamean1", " ANCOVA ile düzeltilmiş uygulama grubu ortalaması", 21.7),
                                                    
                                                    numericInput("ancovamean2", " ANCOVA ile düzeltilmiş kontrol grubu ortalaması", 33.5),
                                                    
                                                    numericInput("ancovameansd", " Düzeltilmiş SS", 50),
                                                    
                                                    p(br())
                                             ),
                                             column(4, offset = 1,
                                                    
                                                    numericInput("ancovameann1", " Uygulama grubu örneklem sayısı", 142),
                                                    
                                                    numericInput("ancovameann2", " Kontrol grubu örneklem sayısı", 133),
                                                    
                                                    numericInput("ancovameancovar", " Yordayıcı-yordanan yada multiple korelasyon", 0.24),
                                                    
                                                    numericInput("ancovameancovarnumber", " Yordayıcı sayısı", 3),
                                                    
                                                    
                                                    helpText("Yenilemek için tıklayınız"),
                                                    submitButton("Yenile"),
                                                    p(br())
                                             )
                                             
                                             
                                           )),
                                         
                                         
                                         h3("Etki Büyüklüğü"),
                                         verbatimTextOutput("ancovamean.out"),
                                         p(br())
                                         
                                       )
                                       
                              ),
                              tabPanel("Chi-Squared istatistiğinden etki büyüklüğü", icon = icon("chevron-right", lib = "font-awesome"),
                                       verticalLayout(
                                         
                                         wellPanel(
                                           fluidRow(
                                             column(3,
                                                    p(strong("Chi-Squared istatistiğinden etki büyüklüğü")),
                                                    
                                                    numericInput("chisquaredstat", " Chi-Squared istatistiği.", 5.3),
                                                    
                                                    numericInput("chisquaredn1", " Örneklem Sayısı.", 50),
                                                    
                                                    p(br())
                                             ),
                                             column(4, offset = 1,
                                                    helpText("Yenilemek için tıklayınız"),
                                                    submitButton("Yenile"),
                                                    p(br())
                                             )
                                             
                                             
                                           )),
                                         
                                         
                                         h3("Etki Büyüklüğü"),
                                         verbatimTextOutput("chisquared.out"),
                                         p(br())
                                         
                                       )
                                       
                              ),
tabPanel("Bireysel grup sonuçlarından", icon = icon("chevron-right", lib = "font-awesome"),
                                       
                                       verticalLayout(
                                         
wellPanel(
fluidRow(
column(3,
p(strong("İkili değişkenler")),
                                                    
numericInput("xi", "Beklenen durumun frekansı", 6),
                                                    
numericInput("ni", "Örneklem büyüklüğü", 323),
                                                    
#numericInput("n1i", "Total", 121),
                                                    
                                                    p(br())
                                             ),
                                             column(4,
radioButtons("divari1", strong("girdi seçenekleri"),
c("ham yüzde" = "PR",
"log dönüşümlü yüzde" = "PLN",
"logit dönüşümlü yüzde (i.e., log odds)" = "PLO",
 "arcsine kök dönüşümlü yüzde (i.e., the angular transformation)" = "PAS",
"Freeman-Tukey çift arcsine dönüşümlü yüzde (Freeman & Tukey, 1950)." = "PFT"
), selected = "PR"),
submitButton("Yenile")
                                             )
                                             
                                           )),
                                         
                                         
h3("Etki Büyüklüğü ve Örneklem Varyansı (iç varyans)"),
verbatimTextOutput("divari1.out"),
                                         
                                         br()
                                         
                                       )
                                       
                              ),
                              tabPanel("İki grup kıyaslama", icon = icon("chevron-right", lib = "font-awesome"),
                                       
                                       verticalLayout(
                                         
                                         wellPanel(
                                           fluidRow(
                                             column(3,
                                                    p(strong("Grup 1:")),
                                                    
                                                    numericInput("ai", "Veri 1", 100),
                                                    
                                                    numericInput("bi", "Veri 2", 21),
                                                    
                                                    #numericInput("n1i", "Total", 121),
                                                    
                                                    p(br())
                                             ),
                                             column(4, offset = 1,
                                                    p(strong("Grup 2:")),
                                                    
                                                    numericInput("ci", "Veri 1", 120),
                                                    
                                                    numericInput("di", "Veri 2", 67),
                                                    
                                                    #numericInput("n2i", "Total", 187),
                                                    
                                                    p(br())
                                             ),
                                             column(4,
                                                    radioButtons("twoxtwovalue", strong("Indeks seçimi"),
                                                                 c("log risk oranı" = "RR",
                                                                   "log olasılık oranı" = "OR",
                                                                   "Risk farkı" = "RD",
                                                                   "arcsine karekök dönüşümlü risk farkı(Rücker et al., 2009)." = "AS",
                                                                   "Peto metodu ile tahminlenmiş olasılık oranı (Yusuf et al., 1985)." = "PETO"
                                                                 ), selected = "OR"),
                                                    submitButton("Yenile")
                                             )
                                             
                                           )),
                                         
                                         
h3("Etki Büyüklüğü and Örneklem Varyansı"),
                                         verbatimTextOutput("twobytwogroups.out"),
                                         
                                         br()
                                         
                                       )
                                       
                              ),
tabPanel("Korelasyon katsayısından (r) etki büyüklüğü", icon = icon("chevron-right", lib = "font-awesome"),
         verticalLayout(
           
           wellPanel(
             fluidRow(
               column(3,
                      p(strong(" ")),
                      
                      numericInput("corrcoeff", "Korelasyon (r)", .37, step = 0.01),
                      numericInput("corrcoeffn", "Toplam örneklem", 500),  
                      numericInput("corrcoefflevel", "Yüzde olarak güven aralığı", 95),
                      
                      p(br())
               ),
               column(4, offset = 1,
                      helpText("Hesaplamak için tıklayınız"),
                      submitButton("Yenile"),
                      p(br())
               )
               
               
             )),
           
           
           h3("Etki büyüklükleri"),
           verbatimTextOutput("corrcoeff.out"),
           p(br()),
           
           br()
           
         )
         
),
tabPanel("Yüzdelerden etki büyüklüğü", icon = icon("chevron-right", lib = "font-awesome"),
         verticalLayout(
           
           wellPanel(
             fluidRow(
               column(3,
                      p(strong("Yüzdelerden etki büyüklüğü")),
                      p("Birinci yüzde"),
                      numericInput("propp1", " Birinci yüzde", 43),
                      numericInput("propnab", " Toplam örneklem", 68),
                      p("İkinci yüzde"),
                      numericInput("propp2", " İkinci yüzde", 24),
                      numericInput("propcd", " Toplam örneklem", 48),
                      numericInput("proplevel", " Güven düzeyi", 95),
                      
                      p(br())
               ),
               column(4, offset = 1,
                      helpText("Hesaplamak için tıklayınız"),
                      submitButton("Yenile"),
                      p(br())
               )
               
               
             )),
           
           
           h3("Etki büyüklükleri"),
           verbatimTextOutput("prop.out"),
           p(br()),
           
           br()
           
         )
         
),
# tabPanel("Failure groups to Effect Size", icon = icon("chevron-right", lib = "font-awesome"),
#          verticalLayout(
#            
#            wellPanel(
#              fluidRow(
#                column(3,
#                       p(strong("Failure groups to Effect Size")),
#                       p("Proportion One"),
#                       numericInput("failB", " Treatment failure.", 5),
#                       numericInput("failN", " Non-treatment failure", 10),
#                       numericInput("failSS", " Treatment sample size", 30),
#                       numericInput("failCSS", " Control/comparison sample size", 30),
#                       #numericInput("faillevel", " Confidence level", 95),
#                       #numericInput("failCER", " Control group Event Rate", 0.20),
#                       
#                       p(br())
#                ),
#                column(4, offset = 1,
#                       helpText("Click here to update your results"),
#                       submitButton("Update View"),
#                       p(br())
#                )
#                
#                
#              )),
#            
#            
#            h3("Effect size indices"),
#            verbatimTextOutput("fail.out"),
#            p(br()),
#            
#            br()
#            
#          )
#          
# ),

                              tabPanel("p değerinden etki büyüklüğü hesaplama", icon = icon("chevron-right", lib = "font-awesome"),
                                       verticalLayout(
                                         
                                         wellPanel(
                                           fluidRow(
                                             column(3,
                                                    p(strong("p değerinden etki büyüklüğü hesaplama")),
                                                    
                                                    numericInput("pvaluenum", " p-değeri", 0.01),
                                                    numericInput("pvaluen1", " Uygulama grubu örneklem sayısı", 50),                      
                                                    numericInput("pvaluen2", " Kontrol grubu uygulama sayısı", 50),
                                                    
                                                    radioButtons("pvaluetail", strong("Tek yada çift taraflı p değeri"),
                                                                 c("tek" = "one",
                                                                   "Çift" = "two"
                                                                 ), selected = "two"),
                                                    
                                                    p(br())
                                             ),
                                             column(4, offset = 1,
                                                    helpText("Yenilemek için tıklayınız"),
                                                    submitButton("Yenile"),
                                                    p(br())
                                             )
                                             
                                             
                                           )),
                                         
                                         
                                         h3("Etki Büyüklüğü"),
                                         verbatimTextOutput("pvaluees.out"),
                                         p(br()),
                                         
                                         br()
                                         
                                       )
                                       
                              ),
                              tabPanel("Tek denekli dizayn", icon = icon("chevron-right", lib = "font-awesome"),
                                       verticalLayout(
                                         
                                         wellPanel(
                                           fluidRow(
                                             column(3,
                                                    p(strong("Tek denekli dizayn")),
                                                    
                                                    radioButtons("SCDtype", strong("Tek denekli dizayn tipi"),
                                                                 c("AB" = "AB",
                                                                   "ABA" = "ABA",
                                                                   "ABAB" = "ABAB",
                                                                   "Tamamen rassal desen" = "CRD",
                                                                   "Rassal Bloklu desen" = "RBD",
                                                                   "Alternatifli müdahele tasarısı" = "ATD",
                                                                   "Çok-kıyaslı AB tasarısı" = "MBD"
                                                                 ), selected = "AB"),
                                                    radioButtons("SCDes", strong("Etki Büyüklüğü"),
                                                                 c("Standardize ortalama farkı" = "SMD",
                                                                   "bileşkeli (pooled)  Standardize ortalama farkı" = "SMDpool",
                                                                   "Çakışık olmayan veri yüzdesi (Pozitif)" = "PND+",
                                                                   "Çakışık olmayan veri yüzdesi (Negatif)" = "PND-",
                                                                   "Ortancayı aşan veri yüzdesi  (Positif)" = "PND+",
                                                                   "Ortancayı aşan veri yüzdesi  (Negatif) " = "PND-"
                                                                 ), selected = "SMD"),
                                                    helpText("Yenilemek için tıklayınız"),
                                                    bsAlert("alert"),
                                                    submitButton("Yenile"),
                                                    
                                                    p(br())
                                             ),
                                             p(strong("Tek denekli desen veri girişi")),
                                             p("Sol sütun koşulu, sağ sütun puanları içermelidir. "),
                                             aceEditor("SCDdata", value="A, 9.523465\nA, 12.371462\nA, 13.265618\nA, 10.182837\nA, 10.987079\nA, 8.161392\nA, 10.655287\nA, 9.563863\nA, 9.381336\nA, 8.822936\nA, 10.227932\nA, 11.961484\nA, 9.425201\nA, 12.199128\nB, 16.212489\nB, 17.657583\nB, 18.45166\nB, 16.645105\nB, 14.618445\nB, 15.769643\nB, 16.017145\nB, 14.000921\nB, 17.081538\nB, 14.06722\nB, 20.423526\nB, 14.123096\nB, 16.728538", mode="r", theme="terminal"),
                                             p("Hesaplanmış etki büyüklüğünüz"),
                                             verbatimTextOutput('SCDES.out'),
                                             #     plotOutput('SCDGRAPH.out'),
                                             #     p(br()),
                                             #     p(br()),
                                             #     p(br()),
                                             #     p(br()),
                                             h3("Referans"),
                                             p("Bulte, I., & Onghena, P. (2008). An R package for single-case randomization tests. Behavior Research Methods, 40, 467--478."),
                                             p("Bulte, I., & Onghena, P. (2009). Randomization tests for multiple baseline designs: An extension of the SCRT-R package. Behavior Research Methods, 41, 477--485.")
                                             
  
                                            )),
                                          
                                          p(br())
                                          
                                        )
                                     
                              )
                              ),
                   navbarMenu("Hakkında", icon = icon("tag", lib = "font-awesome"),
                              tabPanel("Hakkında", icon = icon("bookmark", lib = "font-awesome"),
                                       
                                       HTML('<div style="clear: left;"><img src="http://kylehamilton.com/wp-content/uploads/2015/04/mavis3.png" alt="" style="float: top; margin-right:5px" /></div>'),
                                       br(),
                                       strong('aRma 1.1.3 Hakkında'),
                                       p("aRma, Meta Analyses via Shiny (MAVIS) yazılımının Türkçeleştirilmiş halidir. 
                                         MAVIS'in, dolayısıyla aRma'nın geliştirilme amacı, meta-analizlerin mümkün olduğunca kolay yapılabilmesidir. 
                                         Bu amaçla R programlama dilini Shiny arayüzü ile birleştirmeye çalışmıştır. aRma şu an test aşamasındadır. 
                                         Herhangi bir finansal destek almamaktadır ve almayacaktır, işe yaraması umuduyla küçük bir aRmağandır."),
                                     
                                       br(),
                                       strong("MAVIS Version 1.1.3"),
                                       p("Son güncelleme 11 Ekim 2017"),
                                       p("MAVIS'in aylık indirilme sayısı "),
                                       img(src = "http://cranlogs.r-pkg.org/badges/MAVIS", seamless=NA),
                                       
                                       
                                       br()
                                       
                                       ),
                              tabPanel("Yazarlar ve Katkıda bulunanlar", icon = icon("users", lib = "font-awesome"),
                                       
                                       strong('Yazarlar'),
                                       
                                       HTML('<div style="clear: left;"><img src="http://oi59.tinypic.com/2mnrcci.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
                                       p(a("Burak Aydın, PhD ", href="http://aydinburak.wix.com/test", target="_blank"),br(),
                                         p("İngilizce yazılımın geliştirilmesine yardım etmiş ve uygulamayı Türkçeye çevirmiştir.")
                                       ),
                                       
                                       br(),
                                                                           
                                       HTML('<div style="clear: left;"><img src="http://kylehamilton.com/wp-content/uploads/2014/11/kyle80.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
                                       p(a("William Kyle Hamilton - University of California, Merced", href="http://www.kylehamilton.com", target="_blank")),
                                       p("William Kyle Hamilton bu uygulamanın Ingilizce versiyonunun sahibi ve sorumlusudur. Kyle'ın websitesinden MAVIS'e ve kodlara ulaşabilirsiniz."),
                                       
                                       br(),
                                       HTML('<div style="clear: left;"><img src="http://kylehamilton.com/wp-content/uploads/2014/11/atsushi80.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
                                       p(a("Atsushi Mizumoto, PhD - Kansai University", href="http://mizumot.com", target="_blank"),br(),
                                         p("Atsushi Mizumoto bu uygulamanın temelini atmıştır; uygulamanın ham hali için :", a("bkn", href="https://github.com/mizumot/meta", target="_blank"))
                                         
                                         
                                       ),
                                       
                                       br(),
                                       
                                       strong('Katkıda bulunanlar '),
                                       
                                       HTML('<div style="clear: left;"><img src="http://kylehamilton.com/wp-content/uploads/2015/04/katie80.png" alt="" style="float: left; margin-right:5px" /></div>'),
                                       p(a("Kathleen Coburn - University of California, Merced", href="http://psychology.ucmerced.edu/content/kathleen-coburn", target="_blank"),br(),
                                         p("Kathleen Coburn teknik tavsiye vermiştir")
                                       ),
                                       br(),
                                       HTML('<div style="clear: left;"><img src="http://kylehamilton.com/wp-content/uploads/2014/11/nicole80.png" alt="" style="float: left; margin-right:5px" /></div>'),
                                       p(a("Nicole Zelinsky - University of California, Merced", href="http://psychology.ucmerced.edu/content/nicole-zelinsky", target="_blank"),br(),
                                         p("Nicole Zelinsky PAG modülüne katkıda bulunmuştur")
                                       ),
                                       br(),
                                       strong('Teşekkürler'),
                                       
p('W. Kyle Hamilton uygulamanın testi ve geridönütler için müteşekkirdir:',
a("Health Communications ve Interventions Lab at the University of California, Merced", href="http://cameronhcilab.com/", target="_blank"),
 've',
a("Kathleen Coburn", href="http://psychology.ucmerced.edu/content/kathleen-coburn", target="_blank")),
                                       
p('Atsushi Mizumoto destek ve geri dönüt için müteşekkirdir:',
a("Dr. Luke Plonsky", href="http://oak.ucc.nau.edu/ldp3/", target="_blank"), 'and',
a("Dr. Yo In'nami", href="https://sites.google.com/site/yoinnami/", target="_blank")),
                                       
                                       br(),
                                       
                                       br()
                              ),
#                               tabPanel("Bug Reports", icon = icon("bug", lib = "font-awesome"),
#                                        
#                                        strong('Bug Reports'),
#                                        
#                                        p("If you discover a problem with MAVIS please submit it to the project GitHub page", 
#                                          a("https://github.com/kylehamilton/MAVIS/issues", href="https://github.com/kylehamilton/MAVIS/issues", target="_blank"),br()),
#                                        
#                                        p("MAVIS is an Open Source project, you are more than welcome to submit patches or features and help the project grow."),
#                                        
#                                        
#                                        br()
#                                        
#                               ),
                              tabPanel("Geri Dönüt", icon = icon("comments", lib = "font-awesome"),
                                       
                                       strong('aRma için geri dönüt'),
                                  
                                       p("Görüş, öneri ve geliştirme istekleri için burakaydin@ufl.edu"),
                                    
                                       
                                       br()
                                       
                                       ),
                              
                              tabPanel("License", icon = icon("legal", lib = "font-awesome"),
                                       
                                       strong('License'),
                                       
                                       p("MAVIS: Meta Analysis via Shiny"),
                                       p(" Copyright 2016  W. Kyle Hamilton, Burak Aydin, and Atsushi Mizumoto"),
                                       
                                       p(" This program is free software you can redistribute it and or modify
                                         it under the terms of the GNU General Public License as published by
                                         the Free Software Foundation either version 3 of the License or
                                         at your option any later version."),
                                       
                                       p("This program is distributed in the hope that it will be useful,
                                         but WITHOUT ANY WARRANTY; without even the implied warranty of
                                         MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
                                         GNU General Public License for more details."),
                                       
                                       p("You should have received a copy of the GNU General Public License
                                         along with this program.  If not, see", a("http://www.gnu.org/licenses/gpl.html", href="http://www.gnu.org/licenses/gpl.html", target="_blank"),br()),
                                       img(src = "http://www.gnu.org/graphics/gplv3-127x51.png", seamless=NA),
                                       
                                       
                                       br(),
                                       
                                       strong('Futher Infomation'),
                                       p("If you would like to learn more about the GNU General Public License and what it means tl'dr legal has a simple explaination which can be found here", a("https://www.tldrlegal.com/l/gpl-3.0", href="https://www.tldrlegal.com/l/gpl-3.0", target="_blank"),br()),
                                       
                                       
                                       
                                       br()
                                       
                                       ),
                              
                              tabPanel("Support", icon = icon("chevron-right", lib = "font-awesome"),
                                       
                                       
                                       strong('Support'),
                                       
                                       p("If you're having problems with MAVIS feel free to refer to our GitHub wiki or the documentation available on CRAN."),
                                       a("CRAN page for MAVIS", href="http://cran.r-project.org/web/packages/MAVIS/index.html", target="_blank"),
                                       br(),
                                       a("GitHub Wiki page for MAVIS", href="https://github.com/kylehamilton/MAVIS/wiki", target="_blank"),
                                       br(),
                                       p("As always you are more than welcome to contact the project maintainer at kyle.hamilton@gmail.com"),
                                       br()
                                       
                              )
),
                   
                   #This is just so I can get ui.R to run, I'll fix this later
                   tabPanel(" ",
                            h5(" ")
                   ),
                   
                   p(br())
                   
                              )
                              )