#### Our data 2*2*3 table ####
x <- array(data = c(512, 88, 314, 19,
                    353, 17, 207, 8,
                    120, 168, 205, 325),
           dim = c(2, 2, 3),
           dimnames = list(Gender = c("Male", "Female"),
                           Admission = c("Admitted", "Not"),
                           Dept = c("D1", "D2", "D3")))

#### Conditional odds ratio ####
library(vcd)
(512 * 19) / (314 * 88) # Z = 1
oddsratio(x, log = FALSE)

#### Test homogenous association ####
library(DescTools)
BreslowDayTest(x) ## Breslow-Day test
 

#### Get marginal table ####
margin.table(x, margin = c("Gender", "Admission"))
margin.table(x, margin = c("Gender", "Dept"))

#### 3 * 3 table and all local odds ratio ####
x <- matrix(1 : 9, 3, 3)
x
oddsratio(x, log = FALSE)
(1 * 5) / (4 * 2)
(4 * 8) / (5 * 7)



