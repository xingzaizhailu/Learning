## [SQL Tutorial](https://www.w3schools.com/sql)
Another Website: [https://sqlzoo.net/]()
**Keep in Mind That SQL keywords are NOT case sensitive: select is the same as SELECT.**

Some of The Most Important SQL Commands
- SELECT - extracts data from a database
- UPDATE - updates data in a database
- DELETE - deletes data from a database
- INSERT INTO - inserts new data into a database
- CREATE DATABASE - creates a new database
- ALTER DATABASE - modifies a database
- CREATE TABLE - creates a new table
- ALTER TABLE - modifies a table
- DROP TABLE - deletes a table
- CREATE INDEX - creates an index (search key)
- DROP INDEX - deletes an index

### Comments
-- and /\* \*/

### Select
``` sql
    SELECT column1, column2, ...
    FROM table_name;
```

#### Select Distinct Statement
``` sql
    SELECT DISTINCT column1, column2, ...
    FROM table_name;
```

### Where Clause
``` sql
    SELECT column1, column2, ...
    FROM table_name
    WHERE condition;
```
SQL requires single quotes around text values (most database systems will also allow double quotes).

#### Operators in The WHERE Clause
The following operators can be used in the WHERE clause:

- =            Equal
- <>           Not equal. Note: In some versions of SQL this operator may be written as !=
- >            Greater than
- <            Less than
- >=           Greater than or equal
- <=           Less than or equal
- BETWEEN AND  Between an inclusive range (values, texts, dates)
- LIKE         Search for a pattern: %, \_, [], ^ or !
- IN           To specify multiple possible values for a column
- IS NULL

e.g. 1
```
    SELECT * FROM Orders
    WHERE OrderDate BETWEEN #01/07/1996# AND #31/07/1996#;

    SELECT * FROM Orders
    WHERE OrderDate BETWEEN '1996-07-01' AND '1996-07-31';
```

e.g. 2
```
    SELECT name FROM world
    WHERE name LIKE 'Chi__' OR name LIKE 'United%'
```

e.g. 3
```
    SELECT name, population FROM world
    WHERE name IN ('Sweden', 'Norway', 'Denmark');
```

### And, Or, Not, Exclusive OR
#### AND Syntax
``` sql
    SELECT column1, column2, ...
    FROM table_name
    WHERE condition1 AND condition2 AND condition3 ...;
```
#### OR Syntax
``` sql
    SELECT column1, column2, ...
    FROM table_name
    WHERE condition1 OR condition2 OR condition3 ...;
```
#### NOT Syntax
``` sql
    SELECT column1, column2, ...
    FROM table_name
    WHERE NOT condition;
```

#### Exclusive OR Syntax
``` sql
    SELECT column1, column2, ...
    FROM table_name
    WHERE condition1 XOR condition2;
```

#### Combining AND, OR and NOT
``` sql
    SELECT * FROM Customers
    WHERE Country='Germany' AND (City='Berlin' OR City='München');

    SELECT * FROM Customers
    WHERE NOT Country='Germany' AND NOT Country='USA';
```

### Order By
``` sql
    SELECT column1, column2, ...
    FROM table_name
    ORDER BY column1, column2, ... ASC|DESC;
```

#### ORDER BY Several Columns
``` sql
    SELECT * FROM Customers
    ORDER BY Country, CustomerName;

    SELECT * FROM Customers
    ORDER BY Country ASC, CustomerName DESC;
```

### Select in Select

```
    SELECT name
      FROM world
     WHERE population >= ALL(SELECT population
                               FROM world
                              WHERE population>0)
```
You need the condition population>0 in the sub-query as some countries have null for population.

```
    SELECT continent, name FROM world AS x
    WHERE name <= ALL(
            SELECT name FROM world AS y
            WHERE x.continent = y.continent
    )
```

### Insert Into
``` sql
    INSERT INTO table_name (column1, column2, column3, ...)
    VALUES (value1, value2, value3, ...);

    // If you are adding values for all the columns of the table,
    // you do not need to specify the column names in the SQL query.
    INSERT INTO table_name
    VALUES (value1, value2, value3, ...);
```

### Null Values
#### How to Test for NULL Values?
It is not possible to test for NULL values with comparison operators, such as =, <, or <>.  
We will have to use the IS NULL and IS NOT NULL operators instead.

##### IS NULL Syntax
``` sql
    SELECT column_names
    FROM table_name
    WHERE column_name IS NULL
```
##### IS NOT NULL Syntax
``` sql
    SELECT column_names
    FROM table_name
    WHERE column_name IS NOT NULL;
```

### Update Statement
``` sql
    UPDATE table_name
    SET column1 = value1, column2 = value2, ...
    WHERE condition;
```

#### Update Warning
Be careful when updating records. If you omit the WHERE clause, ALL records will be updated!

### Delete
``` sql
    DELETE FROM table_name
    WHERE condition;
```
#### Delete All Records
``` sql
    DELETE FROM table_name;
    // or:
    DELETE * FROM table_name;
```
Note: Be careful when deleting records in a table! Notice the WHERE clause in the DELETE statement.
The WHERE clause specifies which record(s) that should be deleted. If you omit the WHERE clause, all
records in the table will be deleted!  

### Top, Limit or Rownum Clause
The SELECT TOP clause is used to specify the number of records to return.  
Not all database systems support the SELECT TOP clause.
MySQL supports the LIMIT clause to select a limited number of records, while Oracle uses ROWNUM.  

#### Server / MS Access Syntax:
``` sql
    SELECT TOP number|percent column_name(s)    // SELECT TOP 50 PERCENT * FROM Customers;
    FROM table_name
    WHERE condition;
```
#### MySQL Syntax:
``` sql
    SELECT column_name(s)
    FROM table_name
    WHERE condition
    LIMIT number;
```
#### Oracle Syntax:
``` sql
    SELECT column_name(s)
    FROM table_name
    WHERE ROWNUM <= number;
```

### [Functions](https://sqlzoo.net/wiki/FUNCTIONS)
MAX, MIN, COUNT, AVG, SUM, LENGTH(str), LEFT(str, len)

#### Round
ROUND(population, x)
ROUND(7253.86, 0)    ->  7254
ROUND(7253.86, 1)    ->  7253.9
ROUND(7253.86,-3)    ->  7000


### Alias
``` sql
    SELECT CustomerName, Address + ', ' + PostalCode + ' ' + City + ', ' + Country AS Address
    FROM Customers;
```

But if in MySQL use `CONCAT`:
``` sql
    SELECT CustomerName, CONCAT(Address, ', ', PostalCode, ', ', City, ', ', Country) AS Address
    FROM Customers;
```


### Joins
- (INNER) JOIN: Returns records that have matching values in both tables
- LEFT (OUTER) JOIN: Returns all records from the left table, and the matched records from the right table
- RIGHT (OUTER) JOIN: Returns all records from the right table, and the matched records from the left table
- FULL (OUTER) JOIN: Returns all records when there is a match in either left or right table

#### Inner Join
``` sql
    SELECT Orders.OrderID, Customers.CustomerName, Shippers.ShipperName
    FROM ((Orders
    INNER JOIN Customers ON Orders.CustomerID = Customers.CustomerID)
    INNER JOIN Shippers ON Orders.ShipperID = Shippers.ShipperID);
```

#### Left Join
The result is NULL from the right side, if there is no match.

#### Self Join
A self JOIN is a regular join, but the table is joined with itself.

### Union Operator
#### UNION Syntax
The UNION operator is used to combine the result-set of two or more SELECT statements.

- Each SELECT statement within UNION must have the same number of columns
- The columns must also have similar data types
- The columns in each SELECT statement must also be in the same order

#### UNION ALL Syntax
The UNION operator selects only distinct values by default. To allow duplicate values, use UNION ALL:

``` sql
    SELECT column_name(s) FROM table1
    UNION ALL
    SELECT column_name(s) FROM table2;
```
Note: The column names in the result-set are usually equal to the column names in the first SELECT statement in the UNION.


### Having Clause
The HAVING clause was added to SQL because the WHERE keyword could not be used with aggregate functions.

``` sql
    SELECT COUNT(CustomerID), Country
    FROM Customers
    WHERE ...
    GROUP BY Country
    HAVING COUNT(CustomerID) > 5
    ORDER BY COUNT(CustomerID) DESC;
```


### Exist Operator
The EXISTS operator is used to test for the existence of any record in a subquery.

```
    SELECT column_name(s)
    FROM table_name
    WHERE EXISTS
    (SELECT column_name FROM table_name WHERE condition);
```

### ANY and ALL Operators

### Select into statement
The SELECT INTO statement copies data from one table into a new table.  

```
    SELECT column1, column2, column3, ...
    INTO newtable [IN externaldb]
    FROM oldtable
    WHERE condition;
```

e.g 1
``` sql
    SELECT Customers.CustomerName, Orders.OrderID INTO CustomersBackup2017 IN 'Backup.mdb'
    FROM Customers
    LEFT JOIN Orders ON Customers.CustomerID = Orders.CustomerID;
    WHERE Country = 'Germany';
```

e.g. 2: used to create a new, empty table using the schema of another.
``` sql
    SELECT * INTO newtable
    FROM oldtable
    WHERE 1 = 0;
```


### Insert Into Select
The INSERT INTO SELECT statement copies data from one table and inserts it into another table.

- INSERT INTO SELECT requires that data types in source and target tables match
- The existing records in the target table are unaffected

``` sql
    INSERT INTO table2
    SELECT * FROM table1
    WHERE condition;

    INSERT INTO table2 (column1, column2, column3, ...)
    SELECT column1, column2, column3, ...
    FROM table1
    WHERE condition;
```


### CASE Statement
The CASE statement goes through conditions and returns a value when the first condition is met (like an IF-THEN-ELSE statement). So, once a condition is true, it will stop reading and return the result. If no conditions are true, it returns the value in the ELSE clause.  

If there is no ELSE part and no conditions are true, it returns NULL.

```
    CASE
        WHEN condition1 THEN result1
        WHEN condition2 THEN result2
        WHEN conditionN THEN resultN
        ELSE result
    END;
```

e.g. 1:
``` sql
    SELECT OrderID, Quantity,
    CASE
        WHEN Quantity > 30 THEN "The quantity is greater than 30"
        WHEN Quantity = 30 THEN "The quantity is 30"
        ELSE "The quantity is under 30"
    END AS QuantityText
    FROM OrderDetails;
```

e.g. 2:
``` sql
    SELECT CustomerName, City, Country
    FROM Customers
    ORDER BY
    (CASE
        WHEN City IS NULL THEN Country
        ELSE City
    END);
```

### NULL Functions
#### MySQL
The MySQL IFNULL() function lets you return an alternative value if an expression is NULL:

``` sql
    SELECT ProductName, UnitPrice * (UnitsInStock + IFNULL(UnitsOnOrder, 0))
    FROM Products;
```

or we can use the COALESCE() function, like this:
``` sql
    SELECT ProductName, UnitPrice * (UnitsInStock + COALESCE(UnitsOnOrder, 0))
    FROM Products;
```
#### SQL Server
The SQL Server ISNULL() function lets you return an alternative value when an expression is NULL:

``` sql
    SELECT ProductName, UnitPrice * (UnitsInStock + ISNULL(UnitsOnOrder, 0))
    FROM Products;
```
#### MS Access
The MS Access IsNull() function returns TRUE (-1) if the expression is a null value, otherwise FALSE (0):

``` sql
    SELECT ProductName, UnitPrice * (UnitsInStock + IIF(IsNull(UnitsOnOrder), 0, UnitsOnOrder))
    FROM Products;
```
#### Oracle
The Oracle NVL() function achieves the same result:

``` sql
    SELECT ProductName, UnitPrice * (UnitsInStock + NVL(UnitsOnOrder, 0))
    FROM Products;
```

### Stored Procedure
A stored procedure is a prepared SQL code that you can save, so the code can be reused over and over again.
You can also pass parameters to a stored procedure, so that the stored procedure can act based on the parameter value(s) that is passed.

Syntax
``` sql
    CREATE PROCEDURE procedure_name
    AS
    sql_statement
    GO;
```

Execute a Stored Procedure
``` sql
    EXEC procedure_name;
```

e.g.
``` sql
    CREATE PROCEDURE SelectAllCustomers @City nvarchar(30), @PostalCode nvarchar(10)
    AS
    SELECT * FROM Customers WHERE City = @City AND PostalCode = @PostalCode
    GO;

    EXEC SelectAllCustomers @City = "London", @PostalCode = "WA1 1DP";
```
