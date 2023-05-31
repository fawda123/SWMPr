
# Test case for valid response and parent_in = 'row'
test_that("parser returns correct output for valid response and parent_in = 'row'", {
  resp <- '<?xml version="1.0" encoding="UTF-8"?>
           <data>
             <row>
               <name>John</name>
               <age>25</age>
             </row>
             <row>
               <name>Jane</name>
               <age>30</age>
             </row>
           </data>'
  expected_output <- data.frame(name = c("John", "Jane"), age = c("25", "30"), stringsAsFactors = FALSE)
  
  output <- parser(resp, parent_in = 'row')
  
  expect_equal(output, expected_output)
})

# Test case for invalid IP address response
test_that("parser throws an error for response with invalid IP address", {
  resp <- '<?xml version="1.0" encoding="UTF-8"?>
           <data>
             <error>Invalid ip address</error>
           </data>'
  
  expect_error(parser(resp))
})