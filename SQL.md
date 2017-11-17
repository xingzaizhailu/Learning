## [SQL Tutorial](https://www.w3schools.com/sql)
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

### SQL Select
``` sql
    SELECT column1, column2, ...
    FROM table_name;
```

#### SQL Select Distinct Statement
``` sql
    SELECT DISTINCT column1, column2, ...
    FROM table_name;
```

### SQL Where Clause
``` sql
    SELECT column1, column2, ...
    FROM table_name
    WHERE condition;
```
SQL requires single quotes around text values (most database systems will also allow double quotes).

#### Operators in The WHERE Clause
The following operators can be used in the WHERE clause:

- =       Equal
- <>      Not equal. Note: In some versions of SQL this operator may be written as !=
- >       Greater than
- <       Less than
- >=      Greater than or equal
- <=      Less than or equal
- BETWEEN Between an inclusive range
- LIKE    Search for a pattern
- IN      To specify multiple possible values for a column

### SQL And, Or, Not
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

#### Combining AND, OR and NOT
``` sql
    SELECT * FROM Customers
    WHERE Country='Germany' AND (City='Berlin' OR City='München');

    SELECT * FROM Customers
    WHERE NOT Country='Germany' AND NOT Country='USA';
```

### SQL Order By
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

### SQL Insert Into
``` sql
    INSERT INTO table_name (column1, column2, column3, ...)
    VALUES (value1, value2, value3, ...);

    // If you are adding values for all the columns of the table,
    // you do not need to specify the column names in the SQL query.
    INSERT INTO table_name
    VALUES (value1, value2, value3, ...);
```

### SQL Null Values
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

### SQL Update Statement
``` sql
    UPDATE table_name
    SET column1 = value1, column2 = value2, ...
    WHERE condition;
```

#### Update Warning
Be careful when updating records. If you omit the WHERE clause, ALL records will be updated!

### SQL Delete
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

### SQL Top, Limit or Rownum Clause
The SELECT TOP clause is used to specify the number of records to return.  
Not all database systems support the SELECT TOP clause.
MySQL supports the LIMIT clause to select a limited number of records, while Oracle uses ROWNUM.  

#### SQL Server / MS Access Syntax:
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



















