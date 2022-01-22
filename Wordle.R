library(tidyverse)
library('openxlsx')
library(janitor)

word_list <- read.xlsx('Desktop/Data science projects/Wordle/dictionnary words.xlsx')

wordle<-word_list %>% 
  filter(count=="5") %>% 
  select(-count)

#find letter count
letters <- wordle %>% 
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
  group_by(letter) 

#replace letters with count to find total count per word
word_sum <- wordle %>% 
  separate("word", c('l0','l1','l2', 'l3', 'l4','l5'), sep="") %>% 
  select('l1':'l5')

word_sum[word_sum=="a"] <- "1690" 
word_sum[word_sum=="b"] <- "486"
word_sum[word_sum=="c"] <- "678"
word_sum[word_sum=="d"] <- "853"
word_sum[word_sum=="e"] <- "2009"
word_sum[word_sum=="f"] <- "376"
word_sum[word_sum=="g"] <- "477"
word_sum[word_sum=="h"] <- "570"
word_sum[word_sum=="i"] <- "1155"
word_sum[word_sum=="j"] <- "66"
word_sum[word_sum=="k"] <- "405"
word_sum[word_sum=="l"] <- "1115"
word_sum[word_sum=="m"] <- "571"
word_sum[word_sum=="n"] <- "952"
word_sum[word_sum=="o"] <- "1196"
word_sum[word_sum=="p"] <- "651"
word_sum[word_sum=="q"] <- "40"
word_sum[word_sum=="r"] <- "1370"
word_sum[word_sum=="s"] <- "2000"
word_sum[word_sum=="t"] <- "1094"
word_sum[word_sum=="u"] <- "732"
word_sum[word_sum=="v"] <- "237"
word_sum[word_sum=="w"] <- "368"
word_sum[word_sum=="x"] <- "87"
word_sum[word_sum=="y"] <- "574"
word_sum[word_sum=="z"] <- "65"


word_score <-as.data.frame(apply(word_sum, 2, as.numeric))
row_sums<-
  word_score %>% 
  rowSums() %>% view()

word_pts<-merge(row_sums,wordle)

         
