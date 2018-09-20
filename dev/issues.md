# QUALTR issue tracker

## Embedded data fields

## Flow logic

  DONE
  * identify block associated with each question
  * identify conditional branches
  * parse and present branches
  * develop template for blocks
  
  IN PROGRESS
  * develop template for logic
  
## Question types

  * Done: 
    - likert matrix
    - MC 
    - DB
    - FORM
  * Close: 
  * Far: 
    - Dropdowns? 


## Notes

  * There are different indicators of single / multiple answers for matrix and standard mc qs
  * consider incorporating exportColumnMap
  * Develop template for form (right justified text? )
  * Single DL questions not working
  * cb unknown for OBIP - how many people live in your household?


## 2018-09-07

  x No question for descriptive text
  x Add argument to print_survey to remove all red (internal) text
  x stand-alone dropdown list
  - Slider ~~
  x calendar graphic

## 2018-09-10

  - Comments 
  - display logic
  - BIG IDEA: parse_qsf should return a survey object in the same format as the 
    api response, but it should include display and skip logic items 
