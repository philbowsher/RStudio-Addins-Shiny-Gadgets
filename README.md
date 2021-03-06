# Intro to RStudio Addins & Shiny Gadgets

Report here:

https://beta.rstudioconnect.com/content/1918/addins_gadgets.html

https://beta.rstudioconnect.com/content/3002/addins_gadgets.html

Example code for Intro to RStudio Addins & Shiny Gadgets.

Requires the following packages from CRAN:

```r
devtools::install_github("rstudio/addinexamples", type = "source")
devtools::install_github("thertrader/fidlr")
install.packages("ggThemeAssist")
``` 

An up-to-date version of RStudio is also recommended.

R 3.2.2 used for examples.

Links/examples reviewed in the following order:

## Introduction

R is a powerful programming language for statistical computing with many packages and tools.

The goal of this article is to arm you with tools and techniques for using addins and gadgets.

 - https://blog.rstudio.org/2016/02/09/new-release-of-rstudio-v0-99-878/
 
 - https://www.rstudio.com/resources/webinars/understanding-add-ins/

We'll cover two main approaches:

1. **Addins**

    RStudio addins provide a mechanism for executing R functions interactively from within the RStudio IDE—either through keyboard shortcuts, or through the Addins menu.

2. **Gadgets**
  
    Gadgets are interactive tools that enhance your R programming experience.

## Addins

### addinexamples

 - https://rstudio.github.io/rstudioaddins/
 - addinexamples
 
### Example - 1 -- `Subset a Dataframe` 

### Example - 2 -- `Find and Replace` 

## Gadgets

### Info

 - http://shiny.rstudio.com/articles/gadgets.html
 - http://shiny.rstudio.com/articles/gadget-ui.html

### NYC R Confernece

 - https://github.com/nwstephens/nyr2016

### Example - 1 -- `selectConn.R` 

### Example - 2 -- `variableProfiler.R` 

### rstudio/sol-eng-public

 - https://github.com/rstudio/sol-eng-public/tree/master/newTopics/04-Gadgets

### Example - 1 -- `lmGadget.R` 

### Example - 2 -- `pick_points.R` 

### ggThemeAssist

### Example - 1 -- `diamonds ggplot2`

- https://github.com/calligross/ggthemeassist
- https://github.com/philbowsher/Chicago-R-Meetup-Shiny-Aug-2016

```{r}

library(ggplot2)

#summary(diamonds)

#summary(diamonds$price)

aveSize <- round(mean(diamonds$carat), 4)
clarity <- levels(diamonds$clarity)

p <- qplot(carat, price, data = diamonds, color = clarity) + labs(x = "Carat", y = "Price") + labs(title = "Diamond Pricing")

p + theme(plot.title = element_text(size = 24))
```

### fidlr

### Example - 1 -- `fidlr & dygraphs`

- https://beta.rstudioconnect.com/content/1917/#
- http://www.thertrader.com/2016/04/21/introducing-fidlr-financial-data-loader/
- https://github.com/thertrader/fidlr


```{r setup, message=FALSE, , eval=FALSE}
library(dygraphs)
```


```{r, , eval=FALSE}
##let's load up some economic data the old-fashioned way
library(quantmod)
getSymbols(Symbols = "GDP",
                     src = "FRED",
                     env = globalenv())
```


```{r, eval=FALSE}

##let's use an add-in to load up some economic data
##first, install the package
devtools::install_github("thertrader/fidlr")

##now click on Addins at the top of the IDE, select fidlr
##in the source drop down, click on Fred
##in the instrument panel, type GDP
##click Run, then click Close
##in the Environment panel, there should be an xts object called GDP
##now run this code chunk by clicking on the green triangle at line 20

##use the US econ data
    dygraph(GDP) %>%
    dyRangeSelector(height = 20) %>%
    dyShading(from = "2001-03-01", to = "2001-11-01", color = "#FFE6E6") %>%
    dyShading(from = "2007-12-01", to = "2009-06-01", color = "#FFE6E6") %>% 
    dyEvent(x = "2008-09-15", label = "Lehman Fails", labelLoc = "top", color = "red") %>% 
    dyEvent(x = "1987-09-15", label = "Black Monday", labelLoc = "top", color = "black") %>% 
    dyEvent(x = "1982-12-01", label = "Volcker Eases", labelLoc = "top", color = "green")
    
```


```{r, eval=FALSE}
##use the add in to get some currency data
##open the addin
##select the data source Oanda
##type in GBP/EUR to pull in data on the British pound / Euro
##click Run, click Close
##now there's a data object called GBPEUR in the environment

##now let's use it in a dygraph
      dygraph(GBPEUR) %>%
      dyRangeSelector(height = 20) %>%
      dyAxis("y", label = "GBP/EUR", axisLabelFontSize = 10, axisLabelWidth = 70) %>% 
      dyEvent(x = "2016-06-23", label = "Brexit", labelLoc = "top", color = "blue")
```


## Interesting

### addinslist package

- https://github.com/daattali/addinslist
- http://deanattali.com/blog/addinslist-package/

### Sparklyr

 - https://rpubs.com/nwstephens/nyc-taxi-demo
 
### Interesting Addins

 - https://www.r-bloggers.com/a-gadget-for-tidyr/
 - http://data-laborer.eu/2016/04/ARIMA_Picker.html
 - https://github.com/bnosac/taskscheduleR
 - http://zachcp.org/blog/2016/sketcher_gadget/
 
### Joe Cheng's talk at EARL which included addins

 - https://www.youtube.com/watch?v=MrQVzz2POD4
 
### Mango's Presentation on addins

- http://www.rmanchester.org/presentations/2016/02/ManchesterR_-_Developing_Analysis_Tools_with_Shiny_Gadgets_-_Aimee_Gott_-_20160229.pdf