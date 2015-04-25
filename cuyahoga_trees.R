library(foreign) # for read.dbf function
library(dplyr) # general data manipulation
library(ggplot2) # visualization

## read in census tract level data
cuyTreesRaw <- read.dbf("tl_2010_tract10_Muni_CALC.dbf") 

cuyTrees <- cuyTreesRaw %>%
  select(GEOID10, PosTCPct, ExTCPct) %>%
  # create the difference variable
  mutate(diff = cuyTrees$PosTCPct - cuyTrees$ExTCPct)


incomeRaw <- read.csv("ACS_13_5YR_S1903_with_ann.csv", header = T, stringsAsFactors = F, skip = 1)

income <- incomeRaw %>%
  select(Id2, income = Median.income..dollars...Estimate..Households) %>% 
  # change the variable types so we can merge and visualize later on
  mutate(income = as.numeric(income),
         Id2 = factor(Id2))

cuyClean <- left_join(cuyTrees, income, by = c("GEOID10" = "Id2"))

## set up Belt Magazine themes
paneltheme <- theme(panel.background = element_blank(),
                        axis.ticks = element_blank()) 

texttheme <-theme(plot.title = element_text(size = 28,
                                                hjust = 0,
                                                vjust = 2.2,
                                                family = "Garamond", 
                                                face = "bold", 
                                                colour = "#252525")) +
  theme(axis.title = element_text(size = 16,
                                  family = "Avenir Medium",
                                  face = "bold", 
                                  colour = "#252525"),
        axis.title.x = element_text(vjust = -.5),
        axis.title.y = element_text(vjust = 1.2)) +
  theme(axis.text = element_text(size = 12,
                                 family = "Avenir Medium",
                                 colour = "#252525")) 


## tract-level visualizations

# scatter plot of ExTCPct (Existing Tree Canopy % of Area) and PoPLPct (Possible Planting Area Total as Percent of Area)
ggplot(cuyClean, aes(income, ExTCPct)) + 
  geom_point(color = "#0868ac", size = 3.5, alpha = .4, position = "jitter") + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8 ),
                     labels = c("0", "20%", "40%", "60%", "80%")) +
  scale_x_continuous(breaks = c(0, 40000, 80000, 120000, 160000, 200000),
                     labels = c("0","$40k", "$80k", "$120k", "$160k", "$200k")) +
  labs(title = "Income & Tree Coverage, Cuyahoga County",
       x = "Median Census Tract Income, Thousands",
       y = "Tree Coverage, % of Area") +
  geom_smooth(method = "lm", color = "black", size = 1.5, linetype = "dotted") +
  paneltheme + texttheme

ggsave("Tree Coverage and Income.png", width = 10.4, height = 7.32)

# scatter plot of income and gap between PoPLPct and ExTCPct 
ggplot(cuyClean, aes(income, diff)) + 
  geom_point(color = "#0868ac", size = 3.5, alpha = .4, position = "jitter") + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     labels = c("0", "20%", "40%", "60%", "80%", "100%")) +
  scale_x_continuous(breaks = c(0, 40000, 80000, 120000, 160000, 200000),
                     labels = c("0", "$40k", "$80k", "$120k", "$160k", "$200k")) +
  labs(title = "Tree Coverage Gap & Income, Cuyahoga County",
       x = "Median Census Tract Income",
       y = "Tree Coverage, % of Area") +
  geom_smooth(method = "lm", color = "black", size = 1.5, linetype = "dotted") +
  paneltheme + texttheme

ggsave("Income and Tree Gap.png", width = 10.4, height = 7.32)

## next read in the municipality-level data 
muniRaw <- read.dbf("Municipal_Boundary_CALC.dbf")

muniClean <- muniRaw %>%
  select(NAME, PosTCPct, ExTCPct) %>%
  # create the difference variable and reorder the muni names by difference
  mutate(diff = PosTCPct - ExTCPct,
         NAME = reorder(NAME, diff))

# dotplot showing the gap between the potential and actual tree canopy 
ggplot(muniClean, aes(x = PosTCPct, y = NAME)) +
  geom_segment(aes(x = ExTCPct, xend = PosTCPct, y = NAME, yend = NAME), linetype = "dashed", color = "#BFBFBF")+
  geom_point(color = "#0868ac", size = 4) + # blue(#0868ac) = potential
  geom_point(aes(x = ExTCPct, y = NAME), color = "#7bccc4", size = 4) + # green(#7bccc4) = actual
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", "25%", "50%", "75%", "100%")) +
  labs(title = "Gap between Actual & Potential Tree Coverage",
       x = "Tree Canopy Coverage, % of Area",
       y = "Municipality") +
  texttheme + paneltheme + theme(panel.grid = element_blank()) 

ggsave("Gap between Actual and Potential Coverage.png", width = 10.5, height = 12.5)




