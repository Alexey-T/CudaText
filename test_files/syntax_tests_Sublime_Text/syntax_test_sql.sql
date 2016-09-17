-- SYNTAX TEST "Packages/SQL/SQL.sublime-syntax"

SELECT 'Foo '' Bar';
--           ^ constant.character.escape.sql

SELECT "My "" Crazy Column Name" FROM my_table;
--         ^ constant.character.escape.sql

SELECT
(
SELECT CASE field
    WHEN 1
    THEN -- comment's say that
--                    ^ comment.line.double-dash
        EXISTS(
        select 1)
    ELSE NULL
) as result


/*
This is a
multiline comment
-- ^ source.sql comment.block.c
*/
select
