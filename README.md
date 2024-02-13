# concat

An interpreter for a toy forth-like concatenative language.

Comments and escape sequences aren't supported yet.

## Examples

```
1 5 + .
>> 6

3 ["three is true"] when .
>> "three is true" .

3 [1 -] apply .
>> 2

"double" [2 *] def 7 double .
>> 14

"factorial" [
    dup 1 >
    [dup 1 - factorial *]
    [pop 1]
    if
] def
5 factorial .
>> 120

"magic" [
    "nothing strange here" .
    "magic" ["magic" .] def
] def

magic
>> "nothing strange here"
magic
>> "magic"
```
