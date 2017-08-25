
## Box and whisker plot qpcr

d = read.table("LimeCps-CORRECT.txt", sep = "\t", row.names=1, header=T)

# run setFactorOrder function then run following to keep them in right order for plot. 

d[["NucType"]] <- setFactorOrder(d[["NucType"]], c("DNA", "cDNA"))

## load ggplot

library(ggplot2)
library(grid)
library(ggthemr)

##### set two colours i liked as palette using ggthemr (and for this one my fill was only going
## to be one of two colours (one for DNA and one for cDNA)


dark_cols <- c("lightslateblue", "firebrick")
DarkCols1 <- c("#555555", dark_cols)
# remove previous effects:
ggthemr_reset()
# Define colours for your figures with define_palette
darkCols <- define_palette(
  swatch = DarkCols1, # colours for plotting points and bars
  gradient = c(lower = DarkCols1[1L], upper = DarkCols1[2L]), #upper and lower colours for continuous colours
  background = "white" #defining a grey-ish background 
)
# set the theme for your figures:
ggthemr(darkCols)




## d = data
## aes(factor(what you want on x axis), whatyouwantonYaxis, fill=factor(treatmentforeg))


#ggplot(d, aes(factor(GrowthStage), invsimpson, fill = factor(LimeLoad))) +
Cps <- ggplot(d, aes(factor(GrowthStage), CopiesPerG, fill=factor(NucType))) +
  
  ## + geom_boxplot so it knows what type of plot
  
  geom_boxplot() +
  #scale_fill_viridis(discrete=TRUE) +
  
  ## scale_fill_manual to give different from default colour
  ## name argument gives legend title
  ## colours: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
  theme_classic() +
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) 


xx <- Cps + 
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(size=16, colour="black")) +
  labs(y = "Gene copies per g") +
  theme(axis.text.y=element_text(size=14, angle = 45, colour="black")) +
  theme(axis.title.y=element_text(size=18, colour="black")) +
  #theme(axis.text.y=element_text(size=9)) +
  theme(legend.title = element_text(size=21)) +
  theme(legend.text=element_text(size=16)) +
  labs(fill=" ") 
xx
loo <- xx + theme(legend.key.size = unit(2, "cm"))
loo



fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}




look <- loo + scale_y_continuous(labels=fancy_scientific, 
                                 breaks = scales::pretty_breaks(n = 12))
look

## Links used to make this code:
## for most:
## http://docs.ggplot2.org/0.9.3.1/geom_boxplot.html
## For changing themes, usual cheat sheet and this:
## http://docs.ggplot2.org/current/theme.html
## how to change colours: 
## http://stackoverflow.com/questions/8320462/ggplot2-how-to-adjust-fill-colour-in-a-boxplot-and-change-legend-text
