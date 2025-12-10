library(dplyr)
library(ggplot2)

rm(list=ls());cat('\f')
gc()


rygb8   <- c("rbyg8", "rybg8", 
             "byrg8", "ry8gb",
             "yr8gb", "rgy8b",
             "8gryb", "ry8bg")

randall <- c("bgry8", "bgy8r", 
             "8brgy", "8bgry", 
             "8rgby", "ryb8g", 
             "bg8yr", "bg8yr", 
             "y8rbg", "8ygrb", 
             "y8bgr", "8rbyg", 
             "b8rgy", "g8bry", 
             "g8yrb", "8byrg",
             "8rgby", "gr8by")

# 
# all.combos <- replicate(n = 1000, 
#                         paste(sample(c("b","g","r","y","8"), replace = F), sep = "", collapse = "")) %>%
#   unique()
# 
# unique(all.combos) %>% length
# 
# df.allcombos <- data.frame(combo = all.combos, 
#                            b8_1  = NA, 
#                            b8_2  = NA, 
#                            b8_3  = NA, 
#                            b8_4  = NA, 
#                            b8_5  = NA) %>%
#   as_tibble()
# 
# df.allcombos <- mutate(df.allcombos, 
#                        b8_1 = grepl(pattern = "^8....$", x = df.allcombos$combo), 
#                        b8_2 = grepl(pattern = "^.8...$", x = df.allcombos$combo),
#                        b8_3 = grepl(pattern = "^..8..$", x = df.allcombos$combo),
#                        b8_4 = grepl(pattern = "^...8.$", x = df.allcombos$combo),
#                        b8_5 = grepl(pattern = "^....8$", x = df.allcombos$combo)) 
# library(data.table)
# df.allcombos2 <- df.allcombos %>%
#   as.data.table() %>%
#   melt(., 
#      id.vars = "combo") %>%
#   group_by(variable) %>%
#   summarise(e_freq = sum(value)) %>% as_tibble()
# 
# 
# 
# df.allcombos2




simall <- replicate(n = 400, 
                    replicate(n = length(randall), 
                              which(sample(x = c("b","g","r","y","8"), replace = F) == "8")))




out.df <- NULL
for(i in 1:ncol(simall)){
  out.df <- rbind(out.df, 
                  data.frame(#grp = NA, 
                    col = i, 
                    b8loc = simall[,i]))
}

as_tibble(out.df)

out.df %>%
  as_tibble() %>%
  group_by(col) %>%
  summarise(avg_b8 = mean(b8loc), 
            sd_b8  = sd(b8loc)) %>%
  ggplot(data =., 
         aes(x = col, y = avg_b8)) + 
  geom_point()


ggplot() + 
  geom_histogram(data = out.df, 
                 aes(x = b8loc))

# does the size of the ball matter...----

# ...when throwing sequentially RYGB8?
ball8_nth_io <- rygb8 %>% strsplit(., "") %>%
  lapply(., `==`, "8") %>%
  lapply(., which) %>%
  unlist()


# ...when throwing randomly (dumping all at once)?
ball8_nth_rand <- randall %>% strsplit(., "") %>%
  lapply(., `==`, "8") %>%
  lapply(., which) %>%
  unlist()

fivenum(ball8_nth_io)
fivenum(ball8_nth_rand)

hist(ball8_nth_io)
hist(ball8_nth_rand)
