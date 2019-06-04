Examine Pilot Data
================
Aaron Erlich
03/06/2019

## Inter Coder Reliability

I need to get the same 10 which were coded by everyone. As far as I can
tell, it appears that only **two** of the coders coded these. I am not
really sure why…I hope it’s not a sampling thing. We should double check
that…

``` r
pacman::p_load(tidyverse, irr, kableExtra)
icr <- read_csv("./data_raw/Big Data y Acceso a Info en México_June 3, 2019_13.43.csv")
unique_codes <- read_csv("./data_raw/all_coders_coded.csv")

#remove gardage rows
icr <- icr %>% slice(-(1:5))
                     
#table(icr)

#deal with person who coded from a differet location (uggh)
icr <- icr %>% mutate(unique_coder = 
                        case_when(IPAddress %in% c("134.48.232.19", "174.103.168.235") ~ "Person 1",
                                  IPAddress %in% "174.60.143.107 " ~ "Person 2",
                                  IPAddress %in% "189.146.112.34" ~ "Person 3",
                                  IPAddress %in% "200.56.56.9" ~ "Person 4",
                                  IPAddress %in% "189.210.57.88" ~ "Person 5",
                                  IPAddress %in% "67.71.216.6" ~ "Person 6",
                                  IPAddress %in% "158.143.29.226" ~ "Person 7")) 


#get rid of Dan???
icr2 <- icr %>% filter(Q19 %in% unique_codes$folio_id & unique_coder != "Person 7")

icr2 <- mutate(icr2, Q14 = factor(Q14, levels = c("Poca o nada", 
                                                  "Menos de la mitad", 
                                                  "Aproximadamente la mitad",
                                                  "La mayoría",
                                                  "Toda")))

#table(icr$unique_coder)


data_wide <- reshape2::dcast(icr2,  Q19 ~ unique_coder, value.var="Q14")
```

Okay, now let’s look at agreement. At least for these two coders, it
doesn’t look great… If we condensed the categories it would look better.
I won’t do more until I figure how who the unique coders are

``` r
ratings <- select(data_wide, `Person 5`, `Person 6`)

table(ratings) %>% kable(.)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

La mayoría

</th>

<th style="text-align:right;">

Menos de la mitad

</th>

<th style="text-align:right;">

Poca o nada

</th>

<th style="text-align:right;">

Toda

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Aproximadamente la mitad

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:left;">

La mayoría

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Menos de la mitad

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Poca o nada

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:left;">

Toda

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

4

</td>

</tr>

</tbody>

</table>

``` r
agree(ratings)
```

    ##  Percentage agreement (Tolerance=0)
    ## 
    ##  Subjects = 10 
    ##    Raters = 2 
    ##   %-agree = 50

This is not a sufficient Kripp alpha.

``` r
kripp.alpha(t(ratings), method="ordinal")
```

    ##  Krippendorff's alpha
    ## 
    ##  Subjects = 10 
    ##    Raters = 2 
    ##     alpha = 0.366
