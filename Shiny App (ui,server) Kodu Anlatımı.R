#SHINY KODU ANLATIMI


#ui kismi:

library(shiny)  #Gerekli paketler yuklenir.
library(shinythemes) 
ui<-shinyUI(fluidPage(theme=shinytheme("cyborg"),   #theme kodu ile Shiny app in arka plan rengi ayarlanir.
                      headerPanel(h1(                           #headerPanel ile baslik yazilir, h1 komutu ile baslik buyuklugu ayarlanir
                        "RASTGELE SUREKLI OLASILIK DAGILIMLARI"))
                      ,
                      fluidRow(    #Sayfa duzeni ayarlanmasi saglanir.
                        column(4,  #Kolonlarin genislikleri ayarlanir.
                               wellPanel(    #Sol tarafta ki panel duzeltmeleri.
                                 radioButtons("dist","SUREKLI DAGILIMLAR:",        #Coklu secim kutusunda tek bir secim yapilmasini , ile dagilimlari ekledik.
                                              list("Normal Dagilim"="norm","Duzgun Dagilim"="unif","t Dagilimi"="t","F Dagilimi"="F","Gamma Dagilimi"="gam","Ustel Dagilim"="exp","Ki-Kare Dagilimi"="chisq","Log-normal Dagilim"="lnorm","Beta Dagilimi"="beta","Cauchy Dagilimi"="cauchy","Weibull Dagilimi"="weibull")),
                                 sliderInput("n","Orneklem Buyuklugu:",1,1000,30),   #Orneklem buyuklugu degistirme cubugu eklenir.
                                 uiOutput("dist1"),  #on ucu tanimliyoruz.
                                 uiOutput("dist2"),
                                 checkboxInput("density","Frekans (Yogunluk) Egrisini Goster",FALSE),   #Tekli secim kutusu komutu ile egri eklenir.
                                 conditionalPanel(
                                   condition="input.density==true",
                                   numericInput("bw","Bant Genisligi:",1)
                                 ),
                                 downloadButton("dldat", "Orneklemi Indir", class="btn-dark")   #Ornekleri indirme butonu eklenir.
                               )
                        ),
                        column(8,
                               tabsetPanel(  #Sag tarafta ki panel duzeltmeleri.
                                 tabPanel("Plot",plotOutput("plot",height="600px")),  #Histogram icin sag ust tarafta panel acar,server ile kod bu panele aktarilinca calisir.
                                 tabPanel("Summary",verbatimTextOutput("summary")), #Grafik olusturdugumuz icin plotOutput kodunu yaziyoruz.Server da renderOutput yazarak tanimlamamiz gerekiyor.
                                 tabPanel("Table",tableOutput("table")),
                                 tabPanel("BoxPlot",plotOutput("boxplot",height="600px")),
                                 tabPanel("ViolinPlot",plotOutput("vioplot",height="600px"))
                               )
                        )
                      )
))





#server kismi:


library(vioplot)
library(shiny)


#Dagilim Parametreleri;

#Normal Dagilim: rnorm(n, mean = 0, sd = 1)
#Gama dagilimi: rgamma(n, shape, rate = 1, scale = 1/rate)
#Duzgun dagilim: runif(n, min = 0, max = 1)
#Ustel Dagilim(Exponential): rexp(n, rate = 1)
#Cauchy Dagilimi : rcauchy(n, location = 0, scale = 1)
#Weibull Dagilimi : rweibull(n, shape, scale = 1)
#Beta Dagilimi : rbeta(n, shape1, shape2, ncp = 0)
#Lognormal Dagilimi : rlnorm(n, meanlog = 0, sdlog = 1)


#Dagilimlarin eksik parametreleri eklendi.
rt2 <- function(n=1000,dft=15){ rt(n=n,df=dft) }
formals(rgamma)[1:2] <- c(1000,1)

rchisq2 <- function(n=1000,dfx=1){ rchisq(n=n,df=dfx) }
formals(rf)[1:3] <- c(1000,1,15)

rexp2 <- function(n=1000,rate2=1){ rexp(n=n,rate=rate2) }
formals(rbeta)[1:3] <- c(1000,2,2)

rcauchy2<-function(n=1000,location2=0,scale2=1){rcauchy(n=n,location=location2,scale=scale2) }
formals(rcauchy)[1:3]<-c(1000,2,2)

rweibull2<-function(n=1000,scale2=1){rweibull(n=n,scale=scale2) }
formals(rweibull)[1:3]<-c(1000,2,2)

shinyServer(function(input,output){
  dat <- reactive({
    dist <- switch(input$dist, #Dagilimlarin r daki genel kodlarini yaziyoruz,degistirdiklerimizi degistiriyoruz.
                   norm=rnorm,	unif=runif,	t=rt2, F=rf, gam=rgamma, exp=rexp2,	chisq=rchisq2, lnorm=rlnorm, beta=rbeta,cauchy=rcauchy2,weibull=rweibull2)
    
    def.args <- switch(input$dist, #Her bir dagilimdaki parametreleri cagirma.
                       norm=c(input$mean,input$sd), unif=c(input$min,input$max), t=c(input$dft), F=c(input$df1,input$df2),
                       gam=c(input$shape,input$rate), exp=c(input$rate2), chisq=c(input$dfx), lnorm=c(input$meanlog,input$sdlog), beta=c(input$shape1,input$shape2),cauchy=c(input$location2,input$scale2),weibull=c(input$shape,input$scale2))
    
    f <- formals(dist);	f <- f[names(f)!="n"]; len <- min(length(f),3-1); f <- f[1:len]
    argList <- list(n=input$n)
    for(i in 1:len) argList[[names(f)[i]]] <- def.args[i]
    return(list(do.call(dist,argList),names(f)))
  })
  
  output$dist1 <- renderUI({  #ui deki outputlari render kodu ile buraya ciktisini aliyoruz.
    input$dist
    isolate({
      lab <- switch(input$dist,   #Sol tarafa secilebilir degerleri atar.
                    norm="Ortalama:", unif="Minimum:", t="Serbestlik Derecesi:", F="Ust Serbestlik Derecesi:", gam="Shape:", exp="Oran:",
                    chisq="Serbestlik Derecesi:", lnorm="Ortalama(log):", beta="Alpha:",cauchy="location" , weibull="scale")
      ini <- switch(input$dist,
                    norm=0, unif=0, t=15, F=1, gam=1, exp=1, chisq=1, lnorm=0, beta=2,cauchy=0 , weibull=1) #Dagilimlarin r daki varsayim kodlarina gore ilk parametreyi girdik.
      numericInput(dat()[[2]][1],lab,ini)
    })
  })
  
  output$dist2 <- renderUI({
    input$dist
    isolate({
      lab <- switch(input$dist,
                    norm="Standart Sapma:", unif="Maksimum:", F="Alt Serbestlik Derecesi:", gam="Oran:", lnorm="Standart Sapma(log)", beta="Beta:",cauchy="Oran",weibull="Oran")
      ini <- switch(input$dist,
                    norm=1, unif=1, F=15, gam=1, lnorm=1, beta=2,cauchy=1,weibull=1)       #Dagilimlarin r daki varsayim kodlarina gore ikinci parametreyi girdik.
      if(any(input$dist==c("norm","unif","F","gam","lnorm","beta","cauchy","weibull"))) numericInput(dat()[[2]][2],lab,ini)
    })
  })
  
  output$dldat <- downloadHandler(  #Orneklem verilerini indirme butonu ekledik.
    filename = function() { paste(input$dist, '.csv', sep='') },
    content = function(file) {
      write.csv(data.frame(x=dat()[[1]]), file)
    }
  )
  
  
  output$plot <- renderPlot({  #Histogram kodunu render ile cizdirdik.(ui deki yerine)
    dist <- input$dist
    n <- input$n
    hist(dat()[[1]],main="HISTOGRAM GRAFIGI",xlab="GOZLEMLER",ylab="FREKANS",col="lightseagreen",cex.axis=1.2,cex.lab=1.2,prob=T)
    if(input$density) lines(density(dat()[[1]],adjust=input$bw),lwd=2)
  })
  
  
  output$boxplot <- renderPlot({  #Boxplot kodunu render ile cizdirdik.
    dist <- input$dist
    n <- input$n
    boxplot(dat()[[1]],main="KUTU GRAFIGI",ylab="FREKANS",col="palevioletred3")
    if(input$density) lines(density(dat()[[1]],adjust=input$bw),lwd=2)
  }) 
  
  output$vioplot <- renderPlot({ #Histogram kodunu render ile cizdirdik.(ui deki yerine)
    dist <- input$dist
    n <- input$n
    vioplot(dat()[[1]],main="VIOLIN GRAFIGI",ylab="FREKANS",col="salmon2")
    if(input$density) lines(density(dat()[[1]],adjust=input$bw),lwd=2)
  }) 
  
  output$summary <- renderPrint({  #Summary kodunu render ile cizdirdik.(ui deki yerine)
    summary(dat()[[1]])
  })
  
  output$table <- renderTable({  #Table kodunu render ile cizdirdik.(ui deki yerine)
    data.frame(x=dat()[[1]])
  })
  
  
})