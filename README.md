# SQL Formatter

CLI for SQL formatting

Developed as a [job test of Ecwid by Lightspeed](./task.md)

### As a result you receive:
1. formatted sql
2. implicit things (ASC, INNER, ','-join, AS) become explicit
3. brackets are added into expressions for explicit definition of the calculation order (considering precedence)
4. brackets are added into source clause for explicit definition of the join execution order

## Examples

Input:
```SQL
select  1 + ( 1+ (2 -1.0)) / count(b.*) + 3
AS c from some_table some, some_table some2, ((select * from table_a) a join (select * from table_b) b 
on a.id = b.id) where not - a.id = asdf or false;
```
Output:
```SQL
SELECT
    ((1 + ((1 + (2 - 1.0)) / count(b.*))) + 3) AS c
FROM ((some_table some
    CROSS JOIN some_table some2)
    CROSS JOIN ((
                    SELECT
                        *
                    FROM table_a
                ) AS a
    INNER JOIN (
                   SELECT
                       *
                   FROM table_b
               ) AS b ON (a.id = b.id)))
WHERE (NOT((-(a.id) = asdf))
    OR false);
```

[See more examples](sql-formatter/src/test/kotlin/io/github/paulpaulych/QueryFormatterTest.kt)

## Stack

- Language: Kotlin/JVM
- Build: Gradle
- Libs(test scope only): Kotest 

## Structure

- `parser.lib` - Parser combinators library core. Inspired by [Functional Programming in Kotlin](https://www.manning.com/books/functional-programming-in-kotlin) 
- `parser.json` - JSON parser. Used for debugging the library, painful to delete.
- `parser.sql` - SQL Query parser.
- `formatting` - SQL Query formatting.

## Model

Query model
```java
class Query {
	private List<String> columns;
	private List<Source> fromSources;
	private List<Join> joins;
	private List<WhereClause> whereClauses;
	private List<String> groupByColumns;
	private List<Sort> sortColumns;
	private Integer limit;
	private Integer offset;
}
```
..is good as a start point by does not represent actual structure of query:

1. `columns` actually are [Expression](sql-formatter/src/main/java/io/github/paulpaulych/parser/sql/Expr.kt)
2. `fromSource, joins` do not represent expression-based (tree-based) nature of query source.
   Brackets, default operator's order and operators' precedence should be taken into account.

For example, 
```SQL
    select *
    from (a, b),  (c, d),
        left join (e join f on false) on true
```
should be interpreted by parser as recursive tree structure:
```
    LeftJoin { 
        CrossJoin {
            CrossJoin { a, b },
            CrossJoin { c, d }
        },
        InnerJoin { e, f, on = false } ,
        on = false
    }
```

3. `WHERE` should contain single [Expression](sql-formatter/src/main/java/io/github/paulpaulych/parser/sql/Expr.kt)
4. `ORDER BY` contains list of expressions(not columns) with sort order

Look at [my Query model](sql-formatter/src/main/java/io/github/paulpaulych/parser/sql/Query.kt)

# Run

`Intellij IDEA -> main() -> Run`
or

[//]: # (TODO)

