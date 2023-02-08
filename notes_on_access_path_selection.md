## STATISITCS NEEDED

1. _For each relation T_

- `NCARD(T)` - the cardinality of relation T
- `TCARD(T)` - the number of pages in the segment that hold tuples of relation T.
- `P(T)` - the fraction of data pages in the segment that hold tuples of relation T.
  - P(T) = TCARD(T) / (no. of non-empty pages in the segment).

1. _For each index I on relation T_

- `ICARD(I)` number of distinct keys in index I
- `NINDX(I)` the number of pages in index I

Additionaly we can store number of distinct values if relation is reltively small.

## COST FOR SINGLE RELATION ACCESS PATHS

**_def_**: SQL query operating only on one relation e.g.`SELECT * FROM people where age > 5`.

### Selectivity factors

See the article
