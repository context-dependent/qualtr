Introduction
------------

Tools for working with the Qualtrics API that prioritize portability,
convenience, and security.

### Right now, with qualtr, you can:

-   keep configuration details encrypted on disk
-   render surveys as elegant pdf documents
-   download response data with variable labels and appropriate factor
    levels
-   tabulate labelled summaries suitable for further analysis, or
    formatted for reporting

### In future, I’m hoping to integrate:

-   contact list retrieval and management
-   support for coding likert scale batteries
-   EDA oriented plotting functions

This package was inspired by Jasper Ginn’s qualtRics package, and a good
deal of the functionality overlaps.

Getting started
---------------

### Install the latest version of qualtr

```r
    # devtools::install_github("context-dependent/qualtr")

    library(qualtr)
    library(tidyverse)
```

### Encrypt configuration details in package root

Other packages ask you to store sensitive configuration details in
unencrypted yaml files. While this offers the obvious convenience of not
requiring a password, it is insufficiently secure for our needs.
`encrypt_conf` asks for your api\_token, datacentre url, and key. It
saves these details locally in an encrypted .rds file in the package
root folder. Run the following chunk in the console, replacing the
capitalized parameters with your account’s details

```r
    encrypt_conf(
      api_token = API_TOKEN,
      # base_url = BASE_URL, 
      key = PASSWORD
    )
```

### Confirm your saved configuration details by decrypting them

`decrypt_conf` is used internally to decrypt your configuration details,
stored locally in .conf.rds in the package root. You can call it at the
top level to confirm that the credentials are successfully stored.

```r
    decrypt_conf()
```

Finding surveys
---------------

`list_surveys` returns a list of all surveys associated with your API
key.

```r
    list_surveys()
```

By default, more recently edited surveys are displayed first, but if you
pass a search term to `list_surveys`, they will be ordered by their
proximity to that.

```r
    skills_catalyst_surveys <- list_surveys("Skills Catalyst")

    skills_catalyst_surveys
```

For the functions in the package that retrieve a survey’s questions or
responses, you can specify the survey either by id, passing it as a
quoted string, or by row in the last result of `list_surveys`. In this
case, for example, you could retrieve the same survey by either passing
the number 2 or “SV\_0OJXn0xdyrgkl9P”. Examples of use cases presented
later will make the application clear, but generally speaking, it’s ok
to refer to a survey by row number if you’re working with them
interactively, but better to use id if you’re coding its retrieval into
a script.

Download survey responses
-------------------------

```r
    skills_catalyst_js_exit_responses <- get_responses("SV_0OJXn0xdyrgkl9P")

    head(skills_catalyst_js_exit_responses)
```


Tabulate scale data
-------------------

### Generate raw table for further analysis

```r
    satisfaction_table_raw <- 
      
      skills_catalyst_js_exit_responses %>% 
      
      qt_raw(
        vars(matches("sat"))
      )


    satisfaction_table_raw
```


### Format raw table for printing

```r
    satisfaction_table_print <- 
      
      satisfaction_table_raw %>% 
      
      qt_print()

    satisfaction_table_print
```

### Tabulate by group

```r
    satisfaction_by_employment_table <- 
      
      skills_catalyst_js_exit_responses %>% 
      
        rename(employed = attach_4) %>% 
        group_by(employed) %>% 
        qt_raw(vars(matches("sat"))) %>% 
        qt_print()

    satisfaction_by_employment_table
```

Treating likert responses as numeric
------------------------------------

### Recode values

```r
    skills_catalyst_js_exit_responses %>% 
      
      scr_num(
        
        # .vars specifies the variables you want to recode using a tidyselect query
        # wrapped in a vars function
        .vars = vars(matches("sat_1")),
        
        # .rev specifies, among the variables selected in .vars, which will be reverse coded
        # by default, any variable that includes "_r" is reverse coded
        # .rev = vars(matches("_r")),
        scale = "agree"
      ) %>% 
      
      select(
        matches("sat_1")
      )
```


### Scoring scales

`score_scale()` returns a tibble with the items, total, and average
score per item. The `.vars`, `.rev`, and scale arguments work the same
way.

```r
    skills_catalyst_js_exit_responses %>% 
      
      score_scale(
        
        var_name = "sat_1",
        .vars = vars(matches("sat_1")), 
        .rev = vars(matches("_r")), 
        scale = "agree"
        
      )
```

Printing surveys
----------------

```r
    skills_catalyst_js_exit_questions <- get_survey("SV_0OJXn0xdyrgkl9P")


    print_survey(skills_catalyst_js_exit_questions, "prints/2019-02-27_scjs_exit.tex")
```