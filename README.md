# R Interface of BigObject Analytics

Linux: [![Travis-ci Status](https://travis-ci.org/macrodatalab/BigObjectRPkgs.svg?branch=master)](https://travis-ci.org/macrodatalab/BigObjectRPkgs)
Win : [![Build status](https://ci.appveyor.com/api/projects/status/rqguqyehk6bk51hs/branch/master?svg=true)](https://ci.appveyor.com/project/wush978/bigobjectrpkgs-9gc97/branch/master)
OS X: [![Travis-ci Status](https://travis-ci.org/macrodatalab/BigObjectRPkgs.svg?branch=osx)](https://travis-ci.org/macrodatalab/BigObjectRPkgs)


## Introduction

*BigObject Analytics* is an analytic database. It is designed to help users easily gain actionable insights from their data and build data-driven applications. By mining large sets of data, it allows developers and analysts to ask questions difficult to answer in SQL, for example, association discovery or meaningful data pattern recognition such as statistical significance, The 80-20 Rule, etc. More important, it empowers users to obtain the analytic results in a timely manner that the analysis can be truly interactive.

The philosophy behind this framework is Cross Link Analysis, where associative links are identified and measured to uncover similarities or correlations among entities for use in integrated analysis across multiple domains. These connections can reveal meaningful patterns that will help bring to light the hidden and valuable factors of your data.

> Intelligence is not only the ability to reason; it is also the ability to find relevant material in memory and to deploy attention when needed. Daniel Kahneman <Thinking, Fast and Slow>

BigObject Analytics delivers an analytic framework that unmask the intelligence out of your data.

### BigObject Analytics Community Edition

There is a BigObject Analytics Community Edition in Docker Hub: <https://hub.docker.com/r/macrodata/bigobject-community/>. 

The reader could follow the instruction in Docker Hub to setup an instance of BigObject Analytics Community Edition.

### R Packages for BigObject

- The package *RBigObject* provides a Database Interface (DBI) compliant clint for R to access BigObject Analytics. The implementation is based on the HTTP RESTful API.

- (Coming soon) The package *dplyrBigObject* provides a dplyr compliant client for R to access BigObject Analytics. It is based on *RBigObject*.

## RBigObject

### Installation

Please install these packages via:

```r
library(devtools)
# For RBigObject
install_github("macrodatalab/BigObjectRPkgs", subdir = "RBigObject")
```

### Getting Started

To connect to the BigObject Analytics, please use the following command:

```r
library(RBigObject)
drv <- dbDriver("BigObject")
con <- dbConnect(drv, ip = "127.0.0.1", port = "9090")
```

#### Table Related Functions

*RBigObject* implements the DBI interface, so the user can use `dbListTables` to list all available tables in the Objec

```r
dbListTables(con)
## [1] "Customer" "sales"    "Product"
```

The `dbExistsTable` can check the existence of the given table.

```r
dbExistsTable(con, "sales")
## [1] TRUE
```

The `dbWriteTable` write a data.frame into BigObject Analytics with given name.

```r
dbWriteTable(con, "iristest", iris)
```

The `dbReadTable` can read the entire data from BigObject Analytics into R.

```r
dbReadTable(con, "iristest")
```

As you can see, the data type are convert accordingly.

Here is the simple type mapping between BigObject Analytics and R:

```r
.type_mapping <- c(
  "STRING" = "character",
  "BYTE" = "character",
  "INT8" = "integer",
  "INT16" = "integer",
  "INT32" = "integer",
  "INT64" = "numeric",
  "FLOAT" = "numeric",
  "DOUBLE" = "numeric",
  "DATE32" = "POSIXct",
  "DATETIME32" = "POSIXct",
  "DATETIME64" = "POSIXct"
)
```

For example, the `STRING` in BigObject Analytics will be mapped to `character` in R. 

On the other hand, the following script shows how *RBigObject* converts R objects into BigObject Analytics:

```r
switch(class(obj)[1],
       "character" = "STRING",
       "integer" = "INT32",
       "numeric" = "DOUBLE",
       "POSIXct" = "DATETIME64",
       "factor" = "STRING"
)

```

The user can specify the required type in `dbWriteTable` via the argument `field.type`:

```r
field.type <- rep("STRING", 5)
names(field.type) <- colnames(iris)
dbWriteTable(con, "iristest2", iris, field.type)
tb <- dbReadTable(con, "iristest2")
sapply(tb, class)
## Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
## "character"  "character"  "character"  "character"  "character" 
```

After all, you can remove the tables via `dbRemoveTable`:

```r
dbRemoveTable(con, "iristest")
dbRemoveTable(con, "iristest2")
```

#### Send SQL Command to BigObject Analytics

The user can retrieve the result via SQL statement. Currently, all available SQL statement of BigObject Analytics are shown in <http://docs.bigobject.io/Data_Management/index.html>.

For example, the `dbGetQuery` will directly retrieve the result from BigObject Analytics:

```r
product <- dbGetQuery(con, "SELECT * FROM Product LIMIT 10")
head(product)
##   id                           name        brand brandOwner weightGrams weightOunce category price cost profit
## 1  1                   Fanta orange        Fanta  Coca-Cola           0        0.00     Soda  8.14 5.02   3.12
## 2  2                 Throwback Cola        Pepsi    Pepsico           0        0.00     Soda 12.01 3.11   8.90
## 3  3             Diet Code Red Soda Mountain Dew    Pepsico           0        0.00     Soda 13.71 1.23  12.49
## 4  4     Light Fruit Drink Lemonade  Minute Maid  Coca-Cola           0        0.00           7.48 1.94   5.54
## 5  5 Cheese Flavored Snacks Crunchy      Cheetos    Pepsico           0        2.37   Snacks 13.23 3.96   9.28
## 6  6           Coke Classic Bottles    Coca-Cola  Coca-Cola           0       12.00           5.43 1.73   3.70
```

If the statement will not return data, it will return `NULL`:

```r
dbGetQuery(con, "CREATE TABLE test (a STRING b INT64)")
```

The user can use `dbSendQuery` and `fetch` to extract part of data:

```r
rs <- dbSendQuery(con, "SELECT * FROM sales")
class(rs)
## [1] "BigObjectHandleResult"
## attr(,"package")
## [1] "RBigObject"
fetch(rs, 4)
##   order_id Customer.id Product.id channel_name                Date qty total_price
## 1        1        3226       2557        am/pm 2013-01-01 00:04:04   8       52.24
## 2        2        6691       2631        am/pm 2013-01-01 00:11:26   4       39.72
## 3        2        6691       1833        am/pm 2013-01-01 00:21:02   1        6.90
## 4        3        4138       1626        am/pm 2013-01-01 00:30:22   5       42.10
fetch(rs, 4)
##   order_id Customer.id Product.id channel_name                Date qty total_price
## 1        3        4138        375        am/pm 2013-01-01 00:35:44   6       67.26
## 2        3        4138       3336        am/pm 2013-01-01 00:45:12   8       41.68
## 3        3        4138        736          CVS 2013-01-01 00:55:34   6       56.40
## 4        4        1292       4434         7-11 2013-01-01 01:06:00   6       86.64
```

The fetch command will extract the data from the `BigObjectHandleResult` streamingly. That is to say, the second `fetch(rs, 4)` extract the next 4 rows instead of the 4 rows at the beginning.

#### Configuration

The user can enable `verbose` mode:

```r
bigobject_verbose(TRUE)
rs <- dbSendQuery(con, "SELECT * FROM sales")
## VERBOSE MESSAGE (body) BEGIN
## [1] "{\"Stmt\":\"SELECT * FROM sales\",\"Opts\":{\"Handle\":true}}"
## VERBOSE MESSAGE (body) END
## VERBOSE MESSAGE (retval) BEGIN
## $Content
## $Content$res
## [1] "a4420ceadbbb4dca"
## 
## 
## $Status
## [1] 0
## 
## $Err
## [1] ""
## 
## VERBOSE MESSAGE (retval) END
```

All communication through RESTful API will be printed to the console for debugging.

## TroubleShooting

Please report your error to <https://github.com/macrodatalab/BigObjectRPkgs/issues>. If possible, please report the error message and the verbose message under `verbose` mode.