setwd("C:/Users/USER/Desktop/basiceda")
bankrupt=read.csv("bankrupt.csv",na.strings = "?")
attach(bankrupt)

# Dectecting outliers
outlier <- c()
q_up <- c(); q_lw <- c(); iqr <- c()
for(i in 1:(dim(bankrupt)[2]-1)){
    q_up[i] <- fivenum(bankrupt[,i])[4]; q_lw[i] <- fivenum(bankrupt[,i])[2]
    iqr[i] <- q_up[i]-q_lw[i]
    outlier_n <- length(which(bankrupt[,i] > q_up[i] + iqr[i]*1.5 | bankrupt[,i] < q_lw[i] - iqr[i]*1.5))
    outlier <- append(outlier,outlier_n)
}
outlier <- as.data.frame(outlier)
colnames(outlier) <- c('Number')

library(ggplot2)
ggplot(outlier, aes(1:64,Number/dim(bankrupt)[1]))+geom_bar(stat='identity')+xlab('X')+ylab('Outlier ratio')+scale_x_continuous(breaks=1:64)

install.packages('packHV')
library(packHV)
outlier_loc <- function(i) which(bankrupt[,i] > q_up[i] + iqr[i]*1.5 | bankrupt[,i] < q_lw[i] - iqr[i]*1.5)

colnames(bankrupt)[1:64] <- paste('x',1:64,sep='')
attr.name <- colnames(bankrupt)[-65]
attach(bankrupt)

plots <- function(dat) {
    # We change the plotting parameters
    oldpar <- par(no.readonly = TRUE) # This stores the current parameters
    on.exit(par(oldpar))              # This makes sure to restore them once we are done
    par(mfrow = c(2,2))               # This sets the new parameters: 2 columns (plots)
    
    datt <- get(dat)
    # Plotting
    hist_boxplot(datt,main=paste('Hist and boxplot of',dat))
    hist_boxplot(datt[-outlier_loc(which(attr.name==dat))],main=paste('Hist and boxplot of',dat,'(Outlier deletion)'),
                 xlab='dat')
    boxplot(datt~class,notch=T,main=paste(dat,'~class',sep=''),ylab=dat)
    boxplot(datt[-outlier_loc(which(attr.name==dat))]~class[-outlier_loc(which(attr.name==dat))]
            ,notch=T,main=paste(dat,'~class',' (outlier deletion)',sep=''),ylab=dat,xlab='class')
}

# Making a dashboard

ui <- fluidPage(
    headerPanel("Basic EDA"),
    sidebarPanel(
        selectInput("xselect","Options",attr.name, selectize=TRUE)),
    mainPanel(plotOutput("plot"), width="100%")
)

server <- function(input, output) {
    bankrupt
    output$plot <- renderPlot({
        plots(input$xselect)
    }, height=800, width=800)
}

install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='duckdeok', token='2CFAF4C1BF138A709F8E317D7629BE61', secret='rsTtH3eyU4jpDBDX2lw8TV0jr/beEhCLWzZKB6dQ')

shinyApp(ui, server)