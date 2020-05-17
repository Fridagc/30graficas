rm(list = ls())

library(tidyverse)
library(igraph)
library(ggraph)

data <- read_csv("inp/lword.csv")

c( as.character(data$from), as.character(data$to)) %>%
        as.tibble() %>%
        group_by(value) %>%
        summarize(n=n()) %>% 
        arrange(- n )  -> tempo


colnames(tempo) <- c("name", "n")

mygraph <- graph_from_data_frame( data, vertices = tempo, directed = FALSE )

# Grafica
ggraph(mygraph, layout="linear") + 
        geom_edge_arc(edge_colour="white", edge_alpha=0.5, edge_width=0.5, fold=TRUE) +
        geom_node_point(aes(size=n, color=as.factor(n), fill=n), alpha=0.8) +
        scale_size_continuous(range=c(0.5,6)) +
        geom_node_text(aes(label=name), angle=65, hjust=1, nudge_y = -0.5, size=3, color = "white") +
        theme_void() +
        theme(
                plot.background = element_rect(fill = "black"),
                legend.position="none",
                plot.margin=unit(c(0,0,0.1,0), "null"),
                panel.spacing=unit(c(0,0,3.4,0), "null"),
                plot.title = element_text(hjust = .5, color = "#c51b8a", size = 16),
                plot.subtitle = element_text(hjust = .5, color = "#c51b8a", size = 11)
        ) +
        labs(title = "Reinterpretación del chart de Alice Pieszecki",
             subtitle = "Interconexiones entre las protagonistas del The L Word") +
        expand_limits(x = c(-1.2, 5.2), y = c(-5.6, 6.2)) 

ggsave("graficas/lword.jpg", width = 6, height = 4.6)

