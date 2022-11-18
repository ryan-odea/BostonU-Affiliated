pacman::p_load(tidyverse,
               magrittr,
               ggforce,
               wesanderson,
               gganimate)
devtools::install_github("tylermorganwall/rayshader")
install.packages("devtools")


create_circle <- function(radius,x_shift, y_shift){
  DF <- tibble(
    x = radius*cos(seq(0,2*pi, length.out = 1000)) + x_shift ,
    y = radius*sin(seq(0,2*pi, length.out = 1000))+ y_shift
  ) 
  
  return(DF)
}
#diag
DF <- lapply(seq(0,40), function(i){
  create_circle(rnorm(1, 1, 1), i, i) %>%
    mutate(group = i)
})

DF <- apply(expand_grid(x = 1:10, y = 1:10), 2, function(i){
  create_circle(rnorm(1, 1, 1), i, i)
})

DF <- lapply(seq(1:10), function(a)
  lapply(seq(1:10), function(b)
    create_circle(rnorm(1,.5,1), a, b))) %>%
  bind_rows(.id = "name")

DF %<>% map(., as.data.frame)

DF <- lapply(seq(1:40), function(a){
  lapply(seq(1:40), function(b){
    create_circle(rnorm(1,1,1), a,b)
  })
})

wes_palettes
#changes colors? rerender when get home
what <- bind_rows(DF) %>%
  mutate(fill = round(runif(nrow(.), 1, 5)),
         animation_time = round(runif(nrow(.), 1, 100))) %>%
  ggplot(aes(x = x, y = y, fill = as.factor(fill))) + 
  geom_polygon() +
  theme_no_axes() +
  theme(legend.position = "none") +
  scale_fill_manual(values = wes_palette("Royal2")) + 
  transition_manual(animation_time)
animate(what, duration = 20, fps = 30, width = 2000, height = 2000, res = 300, renderer = gifski_renderer(loop = T))
anim_save("what.gif")
ggsave("landofchaos.png", height = 16, width = 16)
formation <- bind_rows(DF, .id = "group") %>%
  mutate(fill = round(runif(nrow(.), 1, 5)),
         animation_time = round(runif(nrow(.), 1, 3))) %>%
  ggplot(aes(x = x, y = y, group = group, fill = as.factor(fill))) + 
  geom_polygon(alpha = .5) +
  theme_no_axes() +
  theme(legend.position = "none") +
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  transition_reveal(animation_time)
animate(formation, duration = 3, fps = 144, width = 2000, height = 2000, res = 300, renderer = gifski_renderer(loop = T))
anim_save("chaos.gif")

?bind_rows
create_circle <- function(radius_vec, shift_vec){
  
  circle_gen <-
    expand(tibble(),
           radius  = radius_vec,
           x_shift = shift_vec, 
           y_shift = shift_vec
    )  
  
  map(
    1:nrow(circle_gen), function(row_var)
      
      tibble(
        x = circle_gen[row_var, ]$radius * 
          cos(seq(0,2*pi, length.out = 1000)) + 
          circle_gen[row_var, ]$x_shift,
        
        y = circle_gen[row_var, ]$radius * 
          sin(seq(0,2*pi, length.out = 1000)) + 
          circle_gen[row_var, ]$y_shift
      )
  )
}
circle <- create_circle(rnorm(10, 0, 2), 1:10) %>% 
  set_names(c(paste(1:10))) %>% 
  bind_rows(.id = "name")

DF %>% group_by(name) %>%
  mutate(fill = round(runif(1000, 1, 4)), 
         animation_time = round(runif(1000, 1, 3))) %>%
  ggplot(aes(x = x, y = y, group = name, fill = as.factor(fill))) + 
  geom_polygon(alpha = .5) +
  theme_no_axes() +
  theme(legend.position = "none") +
  scale_fill_manual(values = wes_palette("Darjeeling1"))

bee <- circle %>% 
  mutate(group = round(runif(nrow(.), 1, 1000)),
         fill = round(runif(nrow(.), 1, 4)),
         time = round(runif(nrow(.), 1, 5))) %>%
  filter(group %in% c(1, 10, 69, 420, 80, 8)) %>%
  ggplot(aes(x = x, y = y, group = name, fill = as.factor(fill))) + 
  geom_polygon(alpha = .5) + 
  theme_no_axes() + 
  scale_fill_manual(values = wes_palette("Moonrise1")) + 
  theme(legend.position = "none") + 
  transition_manual(time)
animate(bee, duration = 1, fps = 144, width = 2000, height = 2000, res = 300, renderer = gifski_renderer(loop = T))
anim_save("bee.gif")
ggsave("bee.png", width = 16, height = 16)
wes_palettes
#----------------------------------------------------------------------------------
#Contour--------------------------------------------------------------------------
#----------------------------------------------------------------------------------
f <- function(x,y){
  x^2 + cos(x) - sin(y)
}
x <- seq(-1, 5, length.out = 100)
y <- seq(-5, 1, length.out = 100)
fdat <- as.data.frame(expand.grid(x = x, y = y)) 
fdat %<>% mutate(z = f(x, y))

ggtest <- fdat %>% ggplot(aes(x = x, y= y)) + 
  geom_tile(aes(fill = z)) +
  geom_contour(aes(z = z, color = ..level..)) + 
  theme_void() + 
  theme(legend.position = "none") + 
  coord_polar()
