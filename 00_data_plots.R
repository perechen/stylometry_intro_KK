library(tidyverse)
library(tidytext)


### sampler function, without replacement

sampler = function(x, n_samples=1, sample_size=1) {
  
  available = x
  all=tibble()
  
  for (i in 1:n_samples) {
    current_sample = available %>%
      group_by(title) %>%
      sample_n(sample_size) %>% 
      mutate(no_sample = i)
    all = rbind(all, current_sample)
    available = available %>% anti_join(current_sample, by="id")
  }
  return(all)
}



## load corpus

files = list.files("estonian_1900_1930/soned/", full.names = T)

## choose Aspe & Tammsaare for examples

authors=files[str_detect(files,"(Aspe)|(Tammsaare)")]

## construct tidy corpus

corpus = tibble(title = authors, text = sapply(authors, read_file)) %>% ## full texts into rows
  mutate(title = str_extract(title,"(?<=//).*?(?=\\.utf8)")) %>% ## clean  titles
  mutate(author=str_extract(title,"^.*?(?=_)")) ## only authors

## most frequent words across corpus
total_mfw = corpus %>% 
  unnest_tokens(word,text) %>%
  count(word,sort=T)

## word frequency ranks
ranks = total_mfw %>% select(-n) %>% mutate(rank = row_number())


## Sample random sentences, count length

set.seed(144)
p_sent = corpus %>%
  mutate(text = str_split(text,"\\.")) %>% # assume full stop marks a sentence
  unnest(text) %>%
  group_by(title) %>% 
  filter(nchar(text) > 1) %>%
  sample_n(800) %>% 
  mutate(words = str_split(text, " ")) %>% 
  ungroup() %>% mutate(len = lengths(words)) %>%
  group_by(title,author)  %>%
  count(len) %>% 
  ggplot(aes(len,n, group=title, color=author)) + geom_line() + xlim(0,100) 

p_sent 

### word lengths

set.seed(144)
p_lenword = corpus %>% 
  unnest_tokens(word,text) %>% 
  group_by(title, author) %>% 
  sample_n(10000) %>%
  mutate(len=nchar(word)) %>% # how long are words
  group_by(author,title) %>%
  count(len) %>% 
  ggplot(aes(len, n,group=title,color=author)) + geom_line() + xlim(0,15) 


p_lenword


## words

total_mfw = corpus %>% 
  unnest_tokens(word,text) %>%
  count(word,sort=T)

sizes = corpus %>%
  unnest_tokens(word,text) %>%
  group_by(title) %>%
  summarise(n_total=length(word))

tidy_unnested = corpus %>%
  unnest_tokens(word,text)  %>%
  group_by(title) %>%
  mutate(id = paste0(title, "_", row_number()))



set.seed(144)
sampled_corpus = sampler(tidy_unnested, n_samples=3, sample_size = 5000)

## two most frequent words
p_fword = sampled_corpus %>%
  count(title,author,word,no_sample) %>% 
  filter(word %in% pull(total_mfw[c(1,2),1])) %>%
  mutate(n = n/5000) %>% 
  pivot_wider(names_from=word,values_from=n) %>%
  ggplot(aes(ja, ta, color=author)) + geom_point(size=3)

### letter freqs

set.seed(144)
sampled_corpus = sampler(tidy_unnested, n_samples=3, sample_size = 5000)


p_lett = sampled_corpus %>%
  mutate(a = str_extract_all(word,"a"),
         o = str_extract_all(word, "o")) %>%
  mutate(a= lengths(a), o=lengths(o)) %>% 
  group_by(title,author,no_sample) %>%  
  summarise(a=sum(a)/5000, o=sum(o)/5000) %>%
  ggplot(aes(a,o,color=author)) + geom_point(size=3)

save(p_sent,p_lenword, p_fword, p_lett,file="simple_features_plots.rda")



### z-scores in tables

ranks = total_mfw %>% 
  select(-n) %>% 
  mutate(rank = row_number())

### construct term-frequency matrix
pre_matrix = corpus %>%
  unnest_tokens(word, text) %>% 
  count(title, author, word) %>%
  group_by(title, author) %>% 
  mutate(n = n/sum(n)) %>% 
  left_join(ranks, by="word") %>%
  arrange(rank) %>% 
  filter(rank < 101) %>% 
  select(-rank) %>% 
  pivot_wider(names_from = word, values_from=n,values_fill=0)


names = pre_matrix$title

matrix = pre_matrix[,-c(1,2)] %>% as.matrix()

rownames(matrix) = names
matrix
matrix_z = scale(matrix)

save(matrix, matrix_z, file="scaled_example.rda")

### rel vs standard. viz

## relative frequencies
example_rel = corpus %>%
  unnest_tokens(word, text) %>%
  count(title, author, word) %>% 
  group_by(title, author) %>% mutate(n = round(n/sum(n), 3)) %>% 
  left_join(ranks, by="word") %>%
  arrange(rank) %>% 
  filter(rank < 21) 

set.seed(144)

### sample one texts by author
set_of_two = corpus %>%  
  group_by(author) %>%
  sample_n(1) %>% 
  pull(title) 


### scaled frequencies
example_z = example_rel %>%
  group_by(word) %>%
  mutate(z_score = round((n - mean(n))/sd(n),3)) %>%
  filter(title %in% set_of_two) #%>% pivot_wider(names_from = word, values_from=z_score)

## for shaded area
ribb =example_z %>% 
  select(-z_score, -title) %>%
  pivot_wider(names_from=author, values_from=n)


## relative frequency plot
p_rel = ggplot(ribb, aes(rank, Aspe)) + 
  geom_ribbon(data=. %>% filter(rank <= 25), aes(ymax=Aspe, ymin=Tammsaare),fill="grey80",alpha=0.3) +
  geom_line(aes(group=1), color="#CCCCCC",size=1) + 
  geom_line(aes(y=Tammsaare, group=1),color="#666666",size=1) + 
  scale_x_continuous(breaks=c(1:20),labels=ribb$word)  + labs(x="20 words by rank", y="relative frequency")

p_rel

## scaled plot
p_z = example_z %>% ggplot(aes(reorder(word,rank), z_score, group=author, fill=author)) + geom_col(width=0.5,position=position_dodge(width=0.5)) + geom_hline(yintercept = 0, size=0.1) + labs(y="Standartized frequencies (z-scores)",x="20 words by rank")
p_z
save(p_rel, p_z, file="diff_example_plots.rda")


## distance graphs


text_vectors = example_rel %>%
  filter(title %in% set_of_two,
         rank < 3) %>%
  select(-rank) %>%
  pivot_wider(names_from=word, values_from=n) %>%
  ungroup() %>%
  select(c(ja, ta)) %>%
  as.matrix()

differences = text_vectors[1,] - text_vectors[2,]

#angle = (differences[2])/sqrt(differences[1]^2 + differences[2]^2) %>% acos()
#differences[2]/(sqrt(differences[1]^2 + differences[2]^2))


## data for manhattan
manhattan_corner = example_rel %>%
  filter(title %in% set_of_two) %>%
  filter(rank < 3) %>% 
  select(-rank) %>%
  pivot_wider(names_from=word, values_from=n) %>%
  ungroup() %>%  add_row(ja=0.032, ta=0.033) %>%
  mutate(dist="Manhattan")

## data for euclidean
man_viz_p = example_rel %>%
  filter(title %in% set_of_two,
         rank < 3) %>%
  select(-rank) %>%
  pivot_wider(names_from=word, values_from=n) %>%
  ungroup() %>%
  mutate(dist="Euclidean") %>%
  bind_rows(manhattan_corner) %>%
  mutate(title=str_replace(title,"_.*", "_1")) %>%
  ggplot(aes(ja, ta)) +
    geom_line(aes(group=dist,color=dist),size=1.5) +
    geom_point(data = . %>%  filter(dist == "Euclidean"),size=5) +
    xlim(0.03,0.040)+ ylim(0.015,0.037) + 
    geom_text(aes(label=title,hjust=-0.15)) +
#    geom_path(data=view_vector,linetype=2,alpha=0.5) +
    annotate('text', x = 0.036, y = 0.024, label = "D[euc]==sqrt(x^{2} + y^{2}) ",parse = TRUE,size=5) +
    annotate('text', x = 0.0317, y = 0.025, label = "y==~ta[(Aspe_1)]~-~ta[(Tammsaare_1)]",parse = TRUE,size=3,angle=90) + 
    annotate('text', x = 0.0350, y = 0.0342, label = "x==~ja[(Aspe_1)]~-~ja[(Tammsaare_1)]",parse = TRUE,size=3) +
  annotate('text', x = 0.0350, y = 0.036, label = "D[manh]==~x~+~y",parse = TRUE,size=5)

save(man_viz_p, file="man_viz_p.rda")
#view_vector = manhattan_corner[-c(3),] %>%  add_row(ja=0.031,ta=0.015) %>%  mutate(seq=c(3,1,2)) %>% arrange(seq)
#view_vector


### multidimensional viz


est_names = c("Aspe_Aastate_pärast", "Aspe_Ennosaare_Ain", "Aspe_Kasuõde", "Tammsaare_Kärbes", "Tammsaare_Kõrboja_peremees", "Tammsaare_Tõde_ja_õigus_I")


dist_matrix = matrix_z %>% dist(method="manhattan")/100 
names(dist_matrix)  = est_names

save(dist_matrix, file="dist_matrix_fin.rda")


### Multi-Dimensional Scaling

mds = dist_matrix %>% cmdscale(eig = T)

p_mds = tibble(x=mds$points[,1],y=mds$points[,2]) %>% 
  mutate(author=rownames(matrix_z) %>%
           str_replace("_.*", ""), title=est_names) %>%
  ggplot(aes(x,y,color=author)) +
  geom_point(size=5) + 
  geom_text(aes(label = title),nudge_y = 0.05,nudge_x=0.05,size=3)  +
  xlim(-1.3, 1.3)

p_mds

## Clustering dendrogram

library(ggdendro)

load("dist_matrix_fin.rda")
load("scaled_example.rda")

## convert hclust to ggdendro object
tree = dist_matrix  %>% 
  as.dist() %>% # to dist object
  hclust(method="ward.D2") %>% 
  dendro_data()

## manipulate labels
tree$labels = tree$labels %>% mutate(author = str_remove(label, "_.*"), title=est_names, author = factor(author, levels=c("Tammsaare", "Aspe")))

## plot
p_dendro = ggplot() + 
  geom_segment(data=tree$segments, aes(x=x,y=y,xend=xend, yend=yend),size=1) +
  geom_text(data=tree$labels, aes(x=x, y=y, label=title, color=author),angle=90,size=3, hjust=1.1) +
  theme_dendro() + guides(color="none") + scale_y_continuous(limits=c(-2, 2), breaks=c(0, 0.5, 1, 1.5, 2))


## Network


relations = tibble()

distances = dist_matrix %>% as.matrix()
rownames(distances) = est_names
colnames(distances) = est_names

names = colnames(distances)

## find 3 closest neighbors of each text
for (i in 1:nrow(dist_matrix)) {
  
  
  tib = tibble(source = colnames(distances)[i], nb = sort(distances[,i][distances[,i] > 0])[1:3] %>% names(),score=c(3,2,1))
  relations = rbind(relations, tib)
  
  
}


## construct connections
df_graph =relations %>%
  group_by(source, nb) %>%
  mutate(edge_id = paste(sort(unique(c(source,nb))), collapse=" ")) %>%
  group_by(edge_id) %>%
  summarise(score = sum(score)) %>%
  separate(col=edge_id,into=c("source","target"),sep=" ")

df_graph

library(ggnetwork)
library(igraph)

## build a network from connection list

nodes = tibble(nd= unique(c(df_graph$source, df_graph$target)))
edges = df_graph
nt = graph.data.frame(edges,vertices=nodes) 

## plot a network
library(ggnetwork)
p_graph = ggnetwork(nt, layout = igraph::layout.fruchterman.reingold(nt)) %>%
  mutate(author = str_remove(name, "_.*")) %>%
  ggplot(aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(aes(size=score),color="grey80", alpha=0.7) +
  geom_nodes(aes(color=author),size=10) +
  geom_text(aes(label=name),vjust=2.8,hjust=0.7,size=2) + guides(color=F, size=F)

p_graph

save(p_dendro, p_mds, p_graph, file="dist_viz.rda")
