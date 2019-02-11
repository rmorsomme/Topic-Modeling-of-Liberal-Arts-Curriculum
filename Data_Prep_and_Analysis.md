Topic Modeling of Course Content
================
Raphaël Morsomme
2019-02-11

-   [Introduction](#introduction)
-   [Data preparation](#data-preparation)
    -   [Overview](#overview)
    -   [Importing](#importing)
    -   [Extracting course descriptions from course catalogues](#extracting-course-descriptions-from-course-catalogues)
    -   [Tidy text format](#tidy-text-format)
    -   [Stemming and stop word removal](#stemming-and-stop-word-removal)
-   [Analysis](#analysis)
    -   [Overview](#overview-1)
    -   [Key words](#key-words)
        -   [Results](#results)
    -   [Term Emergence](#term-emergence)
        -   [Results](#results-1)
    -   [Topic Modeling](#topic-modeling)
        -   [Estimating the ideal number of topics](#estimating-the-ideal-number-of-topics)
        -   [Topic labeling](#topic-labeling)
        -   [Results](#results-2)
-   [Conclusion](#conclusion)

``` r
library(tidyverse)
library(tidytext)

library(hunspell)    # Stemmer
library(topicmodels) # Latent Dirichlet Allocation algorithm
library(ldatuning)   # finding ideal number of topics for LDA
library(tm)          # Corpus()
```

Introduction
============

University departments often have little knowledge of the content of their study programs. Yet, having a good understanding of what each course of a program covers is paramount to offering an education of quality.

In this script, I analyze the content of the curriculum of the University College Maastricht (UCM), Maastricht University, the Netherlands. UCM offers a bachelor in Liberal Arts and Science. Its curriculum contains over two hundred courses on virtually every topic conceivable - ranging from artificial intelligence to Shakespeare, and terrorism - making it a great subject for this type of analysis.

To accomplish this, I analyze the 200-page course catalogue of UCM where each course possesses a description of one or two pages. The analysis is exploratory in nature: instead of answering a specific research question, I explore the textual data to obtain a better understanding of the content of UCM's curriculum. I conduct three analyses. First, I identify the key words of each course with the tf-idf. Second, I compare the evolution of the catalogues over time to identify themes that have emerged and declined. Finally, I fit a topic model on the most recent course catalogue (2018-2019). For this purpose, I use the Latent Dirichlet Allocation algorithm. The reader can use the [Shiny App](https://rmorsomme.shinyapps.io/shiny_app/) to visualize the results of these analyses.

Data preparation
================

Overview
--------

We starting by importing the course catalogues and a data set containing information at the course level. We then extract the descriptions of each course from the course catalogues. These descriptions are one to two pages long and form the textual data that we will analyze. Lastly, we transform the data into the *[tidy text format](https://www.tidytextmining.com/tidytext.html)*, stem the terms and remove the stop-words.

Importing
---------

We import two datasets: `corpus`, a corpus containing the five most recent course catalogues of UCM, and `d_course`, a tiblle indicating the code, name and cluster of each course (ourses are distributed among 17 clusters: International Relation, Cultural Studies, Biomedical Science, etc.).

``` r
corpus <- Corpus(
  x             = DirSource("Catalogues"),
  readerControl = list(reader = readPDF(control = list(text = "-layout")))
  )
```

``` r
d_course <- read_csv(
  file = "Course.csv",
  col_type = cols()
  )
```

Extracting course descriptions from course catalogues
-----------------------------------------------------

We extract the description of each course from the course catalogues. Since the code is a little longish (see appendix) and does not add much to the script, I only show its outcome.

``` r
print(d_description)
```

    ## # A tibble: 831 x 3
    ##    Code    `Calendar Year` Description                                     
    ##    <chr>   <chr>           <chr>                                           
    ##  1 COR1002 2014-2015       "COR1002 - Philosophy of Science\r\nCourse coor~
    ##  2 COR1003 2014-2015       "COR1003 - Contemporary World History\r\nCourse~
    ##  3 COR1004 2014-2015       "COR1004 - Political Philosophy\r\nCourse coord~
    ##  4 COR1005 2014-2015       "COR1005 - Modeling Nature\r\nCourse coordinato~
    ##  5 HUM1003 2014-2015       "HUM1003 - Cultural Studies I: Doing Cultural S~
    ##  6 HUM1007 2014-2015       "HUM1007 - Introduction to Philosophy\r\nCourse~
    ##  7 HUM1010 2014-2015       "HUM1010 - Common Foundations of Law in Europe\~
    ##  8 HUM1011 2014-2015       "HUM1011 - Introduction to Art; Representations~
    ##  9 HUM1012 2014-2015       "HUM1012 - Pop Songs and Poetry: Theory and Ana~
    ## 10 HUM1013 2014-2015       "HUM1013 - The Idea of Europe: The Intellectual~
    ## # ... with 821 more rows

Tidy text format
----------------

We save the course descriptions in the [tidy text format](https://www.tidytextmining.com/tidytext.html) with one row per combination of course, year and word

``` r
d_description <- unnest_tokens(
  tbl    = d_description, 
  output = word,
  input  = Description
  )
```

``` r
print(d_description)
```

    ## # A tibble: 340,594 x 3
    ##    Code    `Calendar Year` word       
    ##    <chr>   <chr>           <chr>      
    ##  1 COR1002 2014-2015       cor1002    
    ##  2 COR1002 2014-2015       philosophy 
    ##  3 COR1002 2014-2015       of         
    ##  4 COR1002 2014-2015       science    
    ##  5 COR1002 2014-2015       course     
    ##  6 COR1002 2014-2015       coordinator
    ##  7 COR1002 2014-2015       prof       
    ##  8 COR1002 2014-2015       dr         
    ##  9 COR1002 2014-2015       l          
    ## 10 COR1002 2014-2015       boon       
    ## # ... with 340,584 more rows

Stemming and stop word removal
------------------------------

Lastly, we stem the terms and remove the stop words. We use the stemmer from the `hunspell` package to build the stemming function `stem_hunspell()` which, given a term, returns its stem. We prefer the Hunspell stemmer over the usual Snowball stemmer because it offers a more precise stemming.

> **Trick: dictionary-based approach.** Since the function `stem_hunspell()` is not vectorized, it takes a very long time to apply it to all all `340,000` terms of the data set. We therefore create a dictionary that gives the stem of the `8,500` distinct terms present in `d_description_tidy` and then join it with the original data set

``` r
#
# Stemming function
stem_hunspell <- function(term) {
  
  # look up term in dictionary
  stems  <- hunspell_stem(term)[[1]]    
  n_stem <- length(stems)
  
  # if no stem in dictionary, return original term
  if(n_stem == 0) return(term)
  
  # otherwise, return last one (most basic)
  else            return(stems[[n_stem]]) 
  
}


#
# Create dictionary 
my_dictionary <- d_description %>%
  
  # identify distinct words
  distinct(word) %>%
  
  # only apply `stem_hunspell` to the distinct words of the data set
  mutate(
    word_stem = purrr::map_chr(
      .x = word,
      .f = stem_hunspell
      )
    )


#
# Join dictionary and d_description
d_description <- my_dictionary %>%
  full_join(
  d_description,
  by = "word"
    ) %>%
  rename(
    word_original = word,
    word          = word_stem
    )
  

  
#  
# Remove stop words
d_description <- d_description %>%
  filter(
    # remove common stop words
    !word %in% stop_words$word,
    # remove number up ti 1,000
    !word %in% as.character(1 : 1e3),
    # remove own stop words
    !word %in% c(
      "student", 
      "study"
      )
    )


rm(stem_hunspell, my_dictionary)
```

``` r
print(d_description) # See humanities (original) - humanity (stem)
```

    ## # A tibble: 167,961 x 4
    ##    word_original word    Code    `Calendar Year`
    ##    <chr>         <chr>   <chr>   <chr>          
    ##  1 cor1002       cor1002 COR1002 2014-2015      
    ##  2 cor1002       cor1002 SSC3023 2014-2015      
    ##  3 cor1002       cor1002 COR1002 2015-2016      
    ##  4 cor1002       cor1002 SSC3023 2015-2016      
    ##  5 cor1002       cor1002 COR1002 2016-2017      
    ##  6 cor1002       cor1002 SSC3023 2016-2017      
    ##  7 cor1002       cor1002 COR1002 2017-2018      
    ##  8 cor1002       cor1002 HUM3051 2017-2018      
    ##  9 cor1002       cor1002 SSC2067 2017-2018      
    ## 10 cor1002       cor1002 SSC3023 2017-2018      
    ## # ... with 167,951 more rows

Analysis
========

Overview
--------

We conduct three analyses. First, we identify the key words of each course and cluster with the tf-idf. Next, compare the curriculum across years to identify which themes have emerged and declined. Finally, we use the LDA algorithm (a popular technique for topic modeling) to build a topic model of the 2018-2019 curriculum.

Key words
---------

The tf-idf is a popular measure to identify the most important terms of documents belonging to a corpus. By penalizing terms that occur in many documents, the tf-idf allows us to focus on the terms that are specific to each document. In our case, terms such as "learn" or "student", which appear in most course descriptions, and are therefore uninformative, have a low tf-idf. This way, we can identify the most *distinctive* terms, i.e. the key words, of each course descriptions. We use the function `tidytext::bind_tf_idf()` to obtain the tf-idf of each term for the year 2018-2019.

``` r
tf_idf <- d_description %>%
  
  # Only analyze most recent year
  filter(
    `Calendar Year` == "2018-2019"
    ) %>%
  
  # Compute tf-idf
  count(
    Code,
    word,
    sort = TRUE
    ) %>%
  bind_tf_idf(
    term = word,
    document = Code,
    n = n
    ) %>%
  
  # Join with d_course to have additional information about the courses
  left_join(
    d_course, 
    by = "Code"
    )
  

print(tf_idf)
```

    ## # A tibble: 23,790 x 9
    ##    Code  word      n     tf   idf tf_idf `Course Title` Cluster Title_short
    ##    <chr> <chr> <int>  <dbl> <dbl>  <dbl> <chr>          <chr>   <chr>      
    ##  1 SSC3~ poli~    34 0.0783  1.82  0.142 Public Policy~ Govern~ Public Pol~
    ##  2 UGR3~ sear~    30 0.106   1.02  0.108 Undergraduate~ Methods Undergradu~
    ##  3 PRO1~ sear~    27 0.117   1.02  0.119 Research Proj~ Methods Research P~
    ##  4 SKI3~ conf~    26 0.0762  4.05  0.309 Preparing Con~ Skills  Preparing ~
    ##  5 SSC2~ law      26 0.124   2.10  0.262 Law and Socie~ Sociol~ Law and So~
    ##  6 SSC2~ conf~    26 0.102   2.26  0.230 Conflict Reso~ Int. R~ Conflict R~
    ##  7 SSC3~ trade    26 0.0539  3.54  0.191 International~ Intern~ Internatio~
    ##  8 HUM2~ memo~    25 0.0617  3.36  0.207 Cultural Reme~ Cultur~ Cult. Reme~
    ##  9 SKI2~ argu~    25 0.102   2.51  0.255 Argumentation~ Skills  Arg. I     
    ## 10 SSC2~ publ~    25 0.115   1.97  0.226 Public Health~ Govern~ Public Hea~
    ## # ... with 23,780 more rows

``` r
tf_idf_cluster <- tf_idf %>%
  group_by(
    Cluster, 
    word
    ) %>%
  summarise(
    tf_idf = sum(tf_idf),
    n      = sum(n)
    ) %>%
  ungroup
```

### Results

The reader can visualize the results on the [Shiny App](https://rmorsomme.shinyapps.io/shiny_app/). The tf-idf does a pretty good job at identifying the key words of each course and clusters. If the word clouds had no title, it would still be fairly easy to guess the name of the course/cluster based on the key words. We also observe interesting elements at the cluster level. While the cluster `History` gives a central place to the European continent (with key words like Europe and european), the cluster `International Relations` focuses more on China (chinese).

Term Emergence
--------------

We compare the content of the curriculum across years to identify terms and themes that have declined or emerged over time. To accomplish this, we compare the frequency of the terms in two different catalogues by taking their `log odds ratio`. A positive value indicates that the term is more frequent in the latter catalogue (the term has *emerged*) and a negative value indicates a *decline* in the use of the term in the latter catalogues.

### Results

Again, the reader can visualize the results on the [Shiny App](https://rmorsomme.shinyapps.io/shiny_app/). Let us compare the oldest year available (`2014-2015`) with the most recent one (`2018-2019`) to analyze the eveolution of the curriculum over a relatively long period of time. We observe that topics that have made the news these last few years such as religion, migration and climate have gained importance in the curriculum. This shows that the college has done a good job at adapting its curriculum to the development of the world's affairs. We also observe that the themes of sexuality (with terms like `sex`, `sexual` and `sexuality`) and terrorism (`terrorism`, `crime`) have declined. It is interesting to note that the terms `globalization`, `poverty` and `sustainability` have also become less important in the curriculum. As for the term `portal`, its "emergence" is due to the introduction of a new online student *portal* system in 2016. The terms is not mentioned once in 2014-2015 and appears `19` times in 2018-2019. This shows that human interpretation and a good knowledge of the data are crucial in such analysis.

Topic Modeling
--------------

Finally, we use the Latent Dirichlet Allocation (LDA) algorithm to fit a topic model to the corpus of course descriptions. Given a corpus of documents and a predetermined number of topics, the LDA algorithm outputs a topic model that gives the importance of each term to the topics (beta distribution) and the importance of each topic to the documents (gamma distribution). In other words, the LDA finds the mixture of words associated with each topic and the mixture of topics associated with each document. The advantage of the LDA algorithm over classic clustering methods is that it allows for overlap of terms across topics and of topics across documents, thereby offering a model that is closer to natural language.

### Estimating the ideal number of topics

We use the function `ldatunning::FindTopicsNumber()` to estimate the ideal number of topics for the topic model. The function computes four metrics that assess the goodness of fit of the model.

``` r
d_cast <- d_description %>%
  
  # Only consider course descriptions of the most recent year
  filter(
    `Calendar Year` == "2018-2019"
    ) %>%
  
  # Create a dtm
  count(
    Code, 
    word
    ) %>%
  cast_dtm(
    Code,
    word,
    n
    )
```

``` r
#
# Parameters of Gibbs optimizer
my_control <- list(
  
  nstart  = 10,
  seed    = c(1 : 10),
  best    = TRUE,
  
  burnin  = 500,
  iter    = 2000,
  thin    = 100
  
  )


#
# Simulations
results_n_topics <- d_cast %>%
  
  FindTopicsNumber(
    
    # Number of topics tried
    topics = seq(5, 95, by = 5),
  
    # metrics considered
    metrics = c(
      "Griffiths2004",
      "CaoJuan2009",
      "Arun2010",
      "Deveaud2014"
      ),
    
    # Method for fitting LDA
    method  = "Gibbs",
    
    # Parameters of Gibbs optimizer
    control = my_control,
    
    # number of CPU cores to use
    mc.cores = 2L
  
  )
```

``` r
FindTopicsNumber_plot(results_n_topics[c(1, 3 : 5)])
```

![](Data_Prep_and_Analysis_files/figure-markdown_github/LDA%20n%20topics%20plot-1.png)

``` r
# we do not include Griffiths2004 because it produces NaN for some reason.
```

Deveaud2014 is uninformative in our case. CaoJuan2009 and Arun2010 indicate that a topic model with between `25` and `45` topics offers a good fit of the data. For parsimony sake and in order to visualize the results more easily on the [Shiny App](https://rmorsomme.shinyapps.io/shiny_app/), we fit a model with `25` topics. For illustration, we also fit a model with `5` topics which we will label by hand.

``` r
#
# 25 topics (ideal number of topics)
LDA_25  <- LDA(
  
  x       = d_cast ,
  k       = 25      ,
  method  = "Gibbs",
  
  control = my_control
  )
```

``` r
#
# 5 topics (for illustration)
LDA_5  <- LDA(
  
  x       = d_cast ,
  k       = 5      ,
  method  = "Gibbs",
  
  control = my_control
  )
```

### Topic labeling

Since the LDA is an unsupervised algorithm, topics come out unlabeled. We provide labels for the topics of the model with 5 topics by hand for illustration. To accomplish this, we use the plot `Key Words of Each Topic` on the [Shiny App](https://rmorsomme.shinyapps.io/shiny_app/) and we try to find the common theme across the key terms of each topic. For instance, the first topic is characterized by the words `economic`, `law`, `policy`, `international` and `public`; I therefore label it `Policy`. The second topic is dominated by the terms `literature` and `art`; I label it `Arts`. The third topic contains the words `search`, `skill`, `method`, `write`; I label it `Academic Skills`, etc.

### Results

We present the two topic models (5 and 25 topics) with three plots each, both at the course and cluster level. The first plot presents the key words of each topic, the second plot the main courses and clusters of each topic and the third plot the most important topics of each course and cluster.

The first plot gives us an idea of the content of the topics. We have already used it to provide a label to the topics of the model with 5 topics.

The second plot shows how each topic is distributed in the curriculum i.e. whether a topic is concentrated in one course/cluster or spread across several. Looking at the model with 25 topics at the cluster level, we observe that around half of the topics are almost exclusively present in only one cluster. This may not be desirable for a Liberal Arts program whose objective is to provide students with a interdisciplinary perspective the matter(s) that interest them (for instance, approaching the topic of climate change from an ecological, political, economic, legal, physical and ethical perspective). Having topics covered by several clusters would discourage students to stick to one cluster (i.e. one discipline) and encourage them to explore several disciplines.

The third plot gives us an overview of the content of each course/cluster. Looking at the model with 5 (labeled) topics, we observe that courses and clusters contain the topics that we expect them to cover. For instance, the course *Contemporary World History* (C. W. Hist.), which cover the main social, economic and political developments of the world since 1945, is dominated by the topics `Society` and `Policy`; the course *Artifical Intelligence* (A. I.) is dominated by the topic `STEM`, *Argumentation II* (Arg. II) by `Academic Skills` and *Digital Media* (Dig. Med.) by `Arts` and `Society`. At the cluster level, *Economics*, *Governance*, *International Relations* and *International Law* are dominated by the topic `Policy`; *Globalization & Sustainable Development*, *History* and *Sociology* by `Society`, and *Cultural Studies* and *Literature* by `Arts`. Interestingly, the cluster *Psychology* (a socioal science) is dominated by the topic `STEM` which typically corresponds to natural sciences e.g. *Biomedial Sciences*, *Computer Science* and *Mathematics*. Also interesting is the fact that the cluster *Economic* is dominated by the topic `Policy`, and not `STEM`. Many people criticize the discipline of economics, saying that it has lost its social-science roots and has become too similar to the disciplines of physics and mathematics. Yet, our topics model indicates that (at UCM at least), economics resembles more the social sciences disciplines (Governance, International Law) than the natural sciences. Finally, I am surprised by the absence of the topic `Academic Skills` from the cluster *Biomedical Sciences*. The cluster contains the courses related to biology, chemistry and medicine and one would expect academic skills such as reseach skills or lab skills to be an integral part of these courses. The absence of the topic `Academic Skills` reflects the lack of research-related assignments, e.g. research proposal, research paper, essay, etc, in those classes which are heavily dominated by written exam; something the college may want to change.

Conclusion
==========

I have analyzed the curriculum of the Liberal Arts bachelor offered by the University College Maastricht, the Netherlands, in three different ways. First, wth the help of the tf-idf, we can provide key words for each courses. This way, students can browse more efficiently through the 200+ courses offered by the college, i.e. have a rapid understanding of what the courses covers. Second, by comparing the curricula across years with the log odd ratio, we can identify terms that have gained and lost importance over time. Third, with the help of the LDA algorithm, we fit a topic model to the curriculum which tells us which topics each course covers.
