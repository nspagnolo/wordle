library(tidyverse)
library('openxlsx')
library(janitor)

word_list <- read.xlsx('dictionnary words.xlsx')

wordle<-word_list %>% 
  filter(count=="5")

letters <- wordle %>% 
  select(-count) %>% 
  mutate(a=str_detect(word, "a")) %>%
  mutate(b=str_detect(word, "b")) %>% 
  mutate(c=str_detect(word, "c")) %>%
  mutate(d=str_detect(word, "d")) %>%
  mutate(e=str_detect(word, "e")) %>%
  mutate(f=str_detect(word, "f")) %>% 
  mutate(g=str_detect(word, "g")) %>%
  mutate(h=str_detect(word, "h")) %>%
  mutate(i=str_detect(word, "i")) %>%
  mutate(j=str_detect(word, "j")) %>% 
  mutate(k=str_detect(word, "k")) %>%
  mutate(l=str_detect(word, "l")) %>%  
  mutate(m=str_detect(word, "m")) %>%
  mutate(n=str_detect(word, "n")) %>% 
  mutate(o=str_detect(word, "o")) %>%
  mutate(p=str_detect(word, "p")) %>%
  mutate(q=str_detect(word, "q")) %>%
  mutate(r=str_detect(word, "r")) %>% 
  mutate(s=str_detect(word, "s")) %>%
  mutate(t=str_detect(word, "t")) %>% 
  mutate(u=str_detect(word, "u")) %>%
  mutate(v=str_detect(word, "v")) %>% 
  mutate(w=str_detect(word, "w")) %>%
  mutate(x=str_detect(word, "x")) %>%
  mutate(y=str_detect(word, "y")) %>%
  mutate(z=str_detect(word, "z")) %>% 
  mutate(a=replace(a, a==TRUE, 1)) %>% 
  mutate(b=replace(b, b==TRUE, 1)) %>% 
  mutate(c=replace(c, c==TRUE, 1)) %>% 
  mutate(d=replace(d, d==TRUE, 1)) %>% 
  mutate(e=replace(e, e==TRUE, 1)) %>% 
  mutate(f=replace(f, f==TRUE, 1)) %>% 
  mutate(g=replace(g, g==TRUE, 1)) %>% 
  mutate(h=replace(h, h==TRUE, 1)) %>% 
  mutate(i=replace(i, i==TRUE, 1)) %>% 
  mutate(j=replace(j, j==TRUE, 1)) %>% 
  mutate(k=replace(k, k==TRUE, 1)) %>% 
  mutate(l=replace(l, l==TRUE, 1)) %>% 
  mutate(m=replace(m, m==TRUE, 1)) %>% 
  mutate(n=replace(n, n==TRUE, 1)) %>% 
  mutate(o=replace(o, o==TRUE, 1)) %>% 
  mutate(p=replace(p, p==TRUE, 1)) %>% 
  mutate(q=replace(q, q==TRUE, 1)) %>% 
  mutate(r=replace(r, r==TRUE, 1)) %>% 
  mutate(s=replace(s, s==TRUE, 1)) %>% 
  mutate(t=replace(t, t==TRUE, 1)) %>% 
  mutate(u=replace(u, u==TRUE, 1)) %>% 
  mutate(v=replace(v, v==TRUE, 1)) %>% 
  mutate(w=replace(w, w==TRUE, 1)) %>% 
  mutate(x=replace(x, x==TRUE, 1)) %>% 
  mutate(y=replace(y, y==TRUE, 1)) %>% 
  mutate(z=replace(z, z==TRUE, 1)) %>%
  adorn_totals() %>% 
  filter(word=="Total") %>% 
  select(-word) %>% 
  gather(a:z,key="letter", value="Total") %>% 
  group_by(letter) %>% view()


wordle %>%
  select(-count) %>%
  filter(str_detect(word,"a")) %>%
  filter(str_detect(word,"r")) %>%
  filter(str_detect(word,"o")) %>%
  filter(str_detect(word,"s")) %>%
  filter(str_detect(word,"e")) 
  
        
         
