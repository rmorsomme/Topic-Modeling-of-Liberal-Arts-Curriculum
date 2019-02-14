Topic Modeling of Course Content
================
Raphaël Morsomme
2019-02-14

-   [Introduction](#introduction)
-   [Data preparation](#data-preparation)
    -   [Overview](#overview)
    -   [Importing data](#importing-data)
    -   [Extracting course descriptions from course catalogues](#extracting-course-descriptions-from-course-catalogues)
    -   [Tidy text format](#tidy-text-format)
    -   [Stemming and stop word removal](#stemming-and-stop-word-removal)
-   [Analysis](#analysis)
    -   [Overview](#overview-1)
    -   [Key words](#key-words)
        -   [tf-idf](#tf-idf)
        -   [Results](#results)
    -   [Term emergence](#term-emergence)
        -   [Log odds ratio](#log-odds-ratio)
        -   [Results](#results-1)
    -   [Topic modeling](#topic-modeling)
        -   [The Latent Dirichlet Allocation (LDA) algorithm](#the-latent-dirichlet-allocation-lda-algorithm)
        -   [Estimating the ideal number of topics](#estimating-the-ideal-number-of-topics)
        -   [Fitting the topic model](#fitting-the-topic-model)
        -   [Topic labeling](#topic-labeling)
        -   [Results](#results-2)
-   [Conclusion](#conclusion)

``` r
library(tidyverse)
library(tidytext)

library(hunspell)    # Stemmer
library(topicmodels) # Latent Dirichlet Allocation algorithm
library(ldatuning)   # Finding ideal number of topics for LDA
library(tm)          # Corpus()
```

Introduction
============

University departments often have little knowledge of the content of their study programs. Yet, having a good understanding of what each course of a program covers is paramount to offering an education of quality.

In this script, I analyze the content of the curriculum of the [University College Maastricht](https://www.maastrichtuniversity.nl/education/bachelor/bachelor-university-college-maastricht) (UCM), Maastricht University, the Netherlands. UCM offers a bachelor in Liberal Arts. Its curriculum contains 150+ courses on virtually every topic conceivable - ranging from artificial intelligence to Shakespeare, and terrorism - making it a great subject for a content analysis.

To accomplish this, I analyze the course catalogues of the last five years where each course has a one-page description. The analysis is exploratory in nature: instead of answering a specific research question, I explore the textual data in order to obtain a better understanding of the content of UCM's curriculum.

First, with the help of the tf-idf, I identify a series of key words for each course. Key words would help students browse more efficiently through the 200+ courses offered by the college, i.e. have a faster understanding of the content of a course than by reading the one-page description in the catalogue. Second, I compare the catalogues across years to identify themes that have gained and lost importance over time. Finally, I fit a topic model to the course descriptions with the help of the Latent Dirichlet Allocation algorithm (a popular technique for topic modeling) that indicates which topics each course covers. These analyses, whose results can be visualized on the [Shiny App](https://rmorsomme.shinyapps.io/shiny_app/) provide important tools for the monitoring and management of the curriculum.

Data preparation
================

Overview
--------

We starting by importing the course catalogues and a data set containing information at the course level. We then extract the description of each course from the course catalogues; this is the textual data that we will analyze. Lastly, we transform these data into the *[tidy text format](https://www.tidytextmining.com/tidytext.html)*, stem the terms and remove the stop-words.

Importing data
--------------

We import two datasets: `corpus`, a corpus containing the five most recent course catalogues of the college, and `d_course`, a tiblle indicating the code, name and cluster of each course (courses are distributed among 17 clusters e.g. Biomedical Sciences, Cultural Studies, International Relation, etc).

``` r
corpus <- Corpus(
  x             = DirSource("Catalogues"),
  readerControl = list(reader = readPDF(control = list(text = "-layout")))
  )
```

``` r
d_course <- read_csv(
  file     = "Course.csv",
  col_type = cols()
  )

print(d_course)
```

    ## # A tibble: 191 x 4
    ##    Code   `Course Title`                         Cluster     Title_short   
    ##    <chr>  <chr>                                  <chr>       <chr>         
    ##  1 CAP30~ Capstone                               <NA>        Capstone      
    ##  2 COR10~ Philosophy of Science                  Philosophy  Philo. of Sci~
    ##  3 COR10~ Contemporary World History             History     C. W. Hist.   
    ##  4 COR10~ Political Philosophy                   Philosophy  Political Phi~
    ##  5 COR10~ Modeling Nature                        Methods     Modeling Natu~
    ##  6 HUM10~ Cultural Studies I: Doing Cultural St~ Cultural S~ Cult. Studies~
    ##  7 HUM10~ Introduction to Philosophy             Philosophy  Intro to Philo
    ##  8 HUM10~ Common Foundations of Law in Europe    Internatio~ Found. Law Eu~
    ##  9 HUM10~ Introduction to Art; Representations,~ Cultural S~ Intro. to Art 
    ## 10 HUM10~ Pop Songs and Poetry: Theory and Anal~ Cultural S~ Pop Songs & P~
    ## # ... with 181 more rows

Extracting course descriptions from course catalogues
-----------------------------------------------------

We extract the description of each course from the course catalogues. Since some descriptions are two-page long, the code to extract them is a little longish (see appendix) and does not add much to the script; I only show its outcome.

``` r
#
# data set with one row per course-year combination
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

We save the course descriptions in the [tidy text format](https://www.tidytextmining.com/tidytext.html) with one row per course-year-word combination.

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

Lastly, we stem the terms and remove stop words. We use the stemmer `hunspell::hunspell_stem()` to build the stemming function `my_stemming_f()` which, given a term, returns its most basic stem. We prefer the Hunspell stemmer over the usual Snowball stemmer because it offers a more precise stemming. Finally, we remove words that have little meaning: we exclude common stop words, numbers from one to one thousand and a list of our own stop words.

> **Trick: dictionary-based approach.** Since the function `my_stemming_f()` is not vectorized, it is painfully slow to apply it to all `340,000` terms of the data set. We therefore create a dictionary that gives the stem of the `8,500` distinct terms present in `d_description_tidy` and then join it with the original data set.

``` r
#
# Stemming function
my_stemming_f <- function(term) {
  
  # look up term in dictionary
  stems  <- hunspell_stem(term)[[1]]    
  n_stem <- length(stems)
  
  # if no stem in dictionary, return original term
  if(n_stem == 0) return(term)
  
  # otherwise, return last stem (most basic one)
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
      .f = my_stemming_f
      )
    )


#
# Join dictionary and d_description
d_description <- d_description %>%
  
  full_join(
    my_dictionary,
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


rm(my_stemming_f, my_dictionary)
```

``` r
print(d_description) # See humanities (original) - humanity (stem)
```

    ## # A tibble: 167,961 x 4
    ##    Code    `Calendar Year` word_original word       
    ##    <chr>   <chr>           <chr>         <chr>      
    ##  1 COR1002 2014-2015       cor1002       cor1002    
    ##  2 COR1002 2014-2015       philosophy    philosophy 
    ##  3 COR1002 2014-2015       science       science    
    ##  4 COR1002 2014-2015       coordinator   coordinator
    ##  5 COR1002 2014-2015       prof          prof       
    ##  6 COR1002 2014-2015       dr            dr         
    ##  7 COR1002 2014-2015       boon          boon       
    ##  8 COR1002 2014-2015       faculty       faculty    
    ##  9 COR1002 2014-2015       humanities    humanity   
    ## 10 COR1002 2014-2015       sciences      science    
    ## # ... with 167,951 more rows

Analysis
========

Overview
--------

I conduct three analyses. First, with the help of the tf-idf, I identify a series of key words for each course. Second, I compare the catalogues across years to identify themes that have gained and lost importance over time. Finally, I use the LDA algorithm to fit a topic model to the 2018-2019 curriculum. The reader can visualize the results of these analyses on the [Shiny App](https://rmorsomme.shinyapps.io/shiny_app/).

Key words
---------

### tf-idf

The tf-idf is a popular measure to identify the most important terms (the key words) of documents belonging to a corpus. By penalizing terms that occur in many documents, and which are therefore uninformative, the tf-idf identifies the terms that are frequent in and specific to each document. In our case, a term like *search*, which appears in most course descriptions (see tibble `tf_idf`printed below), has a relatively low tf-idf despite being the most frequent term of the descriptions of `UGR3001` and `PRO1012`. We use the function `tidytext::bind_tf_idf()` to obtain the tf-idf of each term for the year 2018-2019.

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
  tidytext::bind_tf_idf(
    term     = word,
    document = Code,
    n        = n
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
#
# tf-idf at the cluster level
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

The reader can visualize the results on the [Shiny App](https://rmorsomme.shinyapps.io/shiny_app/). The tf-idf does a pretty good job at identifying the key words of each course and cluster. If the word clouds had no title, it would still be fairly easy to identify the corresponding course/cluster based on the key words. Looking more closely at the words themselves, we can infer the themes covered by a course/cluster. For instance, while the cluster `History` gives a central place to the European continent (with key words like *Europe* and *european*), the cluster `International Relations` seems to be focussing more on China (*chinese*).

Term emergence
--------------

### Log odds ratio

We compare the course catalogues across years to identify themes that have gained and lost importance over time. To accomplish this, we compare the frequency of the terms in two different catalogues by taking their `log odds ratio`. A positive value indicates that the term is more frequent in the latter catalogue (the term has *gained* importance) and a negative value indicates a decline in the use of the term in the latter catalogues (the term has *lost* importance).

### Results

Again, the reader can visualize the results for any combination of years on the [Shiny App](https://rmorsomme.shinyapps.io/shiny_app/). Let us analyze the long-term evolution of the curriculum by comparing the oldest year available (`2014-2015`) with the most recent one (`2018-2019`). We observe that topics that have made the news these last few years such as religion, migration and climate have gained importance in the curriculum. This shows that the college has done a good job at adapting its curriculum to the development of the world's affairs. We also observe that the themes of "sexuality" (with terms like *sex*, *sexual* and *sexuality*) and "terrorism" (*terrorism*, *crime*) have declined. It is interesting to note that the terms *globalization*, *poverty* and *sustainability* have also become less important in the curriculum. As for the term *portal*, its "emergence" is due to the introduction of a new online student *portal* system in 2016. The terms is not mentioned once in 2014-2015 and appears nineteen times in 2018-2019. This shows that human interpretation and a good knowledge of the data are crucial in such analysis.

Topic modeling
--------------

### The Latent Dirichlet Allocation (LDA) algorithm

Finally, I use the LDA algorithm to fit a topic model to the 2018-2019 curriculum. Given a corpus of documents (in our case, course descriptions) and a predetermined number of topics, the LDA algorithm outputs a topic model that gives the importance of each term to the topics (beta distribution) and the importance of each topic to the documents (gamma distribution). In other words, the LDA finds the mixture of words associated with each topic and the mixture of topics associated with each document. The advantage of the LDA algorithm over classic clustering methods is that it allows for overlap of terms across topics and of topics across documents, thereby offering a model that is closer to natural language.

### Estimating the ideal number of topics

We use the function `ldatunning::FindTopicsNumber()` to estimate the ideal number of topics for the topic model. The function computes four metrics assessing the goodness of fit of the model.

``` r
d_cast <- d_description %>%
  
  # Only analyze most recent year
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

`Deveaud2014` is uninformative in our case. `CaoJuan2009` and `Arun2010` indicate that a topic model with between `25` and `45` topics offers a good fit of the data.

### Fitting the topic model

For parsimony sake and in order to visualize the results more easily, we fit a model with `25` topics. For illustration, we also fit a model with `5` topics which we label by hand.

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

Since the LDA is an unsupervised algorithm, topics come out unlabeled. We provide labels for the topics of the model with 5 topics by hand for illustration. To accomplish this, we use the plot `Key Words of Each Topic` on the [Shiny App](https://rmorsomme.shinyapps.io/shiny_app/) and we find the common theme across the key words of each topic. For instance, the first topic is characterized by the words *economic*, *law*, *policy*, *international* and *public*; I therefore label it `Policy`. The second topic is dominated by the terms *literature* and *art*; I label it `Arts`. The third topic contains the words *search*, *skill*, *method*, *write*; I label it `Academic Skills`, etc.

### Results

We present the two topic models (5 and 25 topics) with three plots each, both at the course and cluster level, on the [Shiny App](https://rmorsomme.shinyapps.io/shiny_app/).

The first plot `Key Words of Each Topic` presents the key words of each topic and thereby gives us an idea of their content. We have already used this plot to provide topic labels for the model with 5 topics.

The second and third plots (`Main Courses/Clusters of Each Topic` and `Main Topics per Courses/Cluster`) present the distribution of topics in the curriculum. The second plot indicates what the main courses/clusters of each topic are. Of interest in this plot is whether a topic is concentrated in one course/cluster or spreads across several. Looking at the model with 25 topics at the cluster level, we observe that around half of the topics are almost exclusively present in only one cluster e.g. topic `9` is almost only present in the cluster `Philosophy`. This may not be desirable for a Liberal Arts program whose objective is to provide students with a interdisciplinary education (being, for instance, able to approach a complex topic like climate change from the perspectives of ecology, politics, economics, law, physics or ethics). Having on the other hand topics spread across several clusters encourages students to explore several disciplines instead of sticking to one cluster (i.e. one discipline).

The third plot presents the main topics of each course/cluster. Looking at the model with 5 (labeled) topics, we observe that, in general, courses and clusters contain the topics that we expect them to cover. For instance, at the course level, `Contemporary World History` (C. W. Hist.), a course covering the main social, economic and political developments of the world since 1945, is dominated by the topics `Society` and `Policy`; `Artifical Intelligence` (A. I.) is dominated by the topic `STEM`, `Argumentation II` (Arg. II) by `Academic Skills`, and `Digital Media` (Dig. Med.) by `Arts` and `Society`. At the cluster level, the clusters `Economics`, `Governance`, `International Relations` and `International Law` are dominated by the topic `Policy`; `Globalization & Sustainable Development`, `History` and `Sociology` by `Society`, and `Cultural Studies` and `Literature` by `Arts`.

The third plot also shows that, interestingly, the cluster `Psychology` (a social science) is dominated by the topic `STEM` which typically corresponds to natural sciences (`Biomedial Sciences`, `Computer Science` and `Mathematics`). This is due to the limited number of topics of the model: the theme of *psychology* had to be integrated into the topic `STEM` (note that the term `psychology` is a key word of the topic `STEM`) instead of being a topic on its own. Also interesting is the fact that the cluster `Economics` is dominated by the topic `Policy`, and not `STEM`. Many people criticize the discipline of economics as losing its social-science roots and becoming too similar to physics or mathematics. Yet, our topic model indicates that, at UCM at least, economics is thaught more like a social science (topic `Policy`) than a natural sciences (topic `STEM`). Finally, I am surprised by the absence of the topic `Academic Skills` from the cluster `Biomedical Sciences`. The cluster contains the courses related to biology, chemistry and medicine, and one would expect academic skills (e.g. reseach skills or lab skills) to be an integral part of these courses. The absence of the topic `Academic Skills` in the cluster `Biomedical Sciences` reflects the lack of research-related assignments, e.g. research proposal, research paper, essay, etc, in those course; something the college may want to investigate.

Conclusion
==========

University departments often have little knowledge of the content of their study programs. Yet, having a good understanding of what each course of a program covers is paramount to offering an education of quality.

In this script, I analyze the content of the curriculum of the Liberal Arts bachelor offered by the University College Maastricht, the Netherlands. To accomplish this, I analyze the course catalogues of the last five years where each of the 150+ courses offered at the college has a one-page description. First, with the help of the tf-idf, I identify a series of key words for each course. Key words would help students browse more efficiently through the 200+ courses offered by the college, i.e. have a faster understanding of the content of a course than by reading the one-page description in the catalogue. Second, I compare the catalogues across years to identify themes that have gained and lost importance over time. Finally, I fit a topic model to the course descriptions with the help of the Latent Dirichlet Allocation algorithm (a popular technique for topic modeling) that indicates which topics each course covers. These analyses, whose results can be visualized on the [Shiny App](https://rmorsomme.shinyapps.io/shiny_app/) provide important tools for the monitoring and management of the curriculum.
