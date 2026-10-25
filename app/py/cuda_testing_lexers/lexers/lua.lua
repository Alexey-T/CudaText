#!/bin/lua

-- This is a single-line comment in Lua.
--[[
This is a multi-line comment.
It can span multiple lines.
--]]

--Str
--test
 s = 'ww\'ww', "ww\"ww", 'alo\n123"'
 a = [[alo
  123]] test
 a = [==[
  alo
  123]==] test

 n = 2 + 2.22 + 22.22e-2 + 0.22E1 + 0xF0 + .5 + 5.  
 ::name:: , goto name

function Test.mt.__lt (a,b)
  return a <= b
end

function Test:app (a, n)
  if n == 0 then
    coroutine.yield(a)
  else
    for i=1,n do
      a[n], a[i] = a[i], a[n]
    end
  end
end

function tst(a)
  local n = table.getn(a)
  return coroutine.wrap(function () permgen(a, n) end)
end

tst(dd)
tst

function printResult (a)
  for i,v in ipairs(a) do
    io.write(v, " ")
  end
  io.write("\n")
end

repeat
  print "aa"
until true 


-- 1. Variables and Data Types
-- Lua is dynamically typed, so you don't declare types explicitly.
local myNumber = 10           -- Number (integers and floats are the same type)
local myString = "Hello, Lua!" -- String (single or double quotes)
local isTrue = true           -- Boolean
local nilValue = nil          -- Nil (similar to null)
local myTable = {             -- Table (Lua's only data structure, like arrays/objects)
    key1 = "value1",
    [2] = "value2",           -- Numerical index
    "another value"           -- Implied numerical index (starts from 1)
}
local myFunction = function(arg1, arg2)
    -- Functions are first-class citizens
    return arg1 + arg2
end

-- Global variables (avoid these if possible, use 'local')
globalVariable = "I'm global!"

-- 2. Operators
-- Arithmetic
local sum = 10 + 5
local difference = 10 - 5
local product = 10 * 5
local quotient = 10 / 5
local exponent = 2 ^ 3
local modulo = 10 % 3

-- Relational
local isEqual = (10 == 10)
local isNotEqual = (10 ~= 5) -- or 10 != 5 (newer Lua versions)
local isLess = (5 < 10)
local isGreater = (10 > 5)
local isLessEqual = (10 <= 10)
local isGreaterEqual = (10 >= 5)

-- Logical
local andOp = (true and false)
local orOp = (true or false)
local notOp = (not true)

-- Concatenation
local combinedString = "Hello" .. " " .. "World"

-- Length operator
local stringLength = #"Lua"
local tableLength = #myTable -- For numerically indexed parts

-- 3. Control Structures

-- If-ElseIf-Else
if myNumber > 10 then
    print("Number is greater than 10")
elseif myNumber == 10 then
    print("Number is exactly 10")
else
    print("Number is less than 10")
end

-- While Loop
local i = 1
while i <= 5 do
    print("While loop iteration: " .. i)
    i = i + 1
end

-- Repeat-Until Loop (executes at least once)
local j = 1
repeat
    print("Repeat-until loop iteration: " .. j)
    j = j + 1
until j > 5

-- For Loop (numeric)
for k = 1, 5, 1 do -- start, end, step (step defaults to 1)
    print("Numeric for loop iteration: " .. k)
end
-- Or descending
for k = 5, 1, -1 do
    print("Numeric for loop (descending) iteration: " .. k)
end

-- For Loop (generic - iterating over tables)
-- ipairs for numerically indexed parts (1 to n)
local fruits = {"apple", "banana", "cherry"}
for index, value in ipairs(fruits) do
    print("Fruit " .. index .. ": " .. value)
end

-- pairs for all key-value pairs (order not guaranteed)
local person = {name = "Alice", age = 30, city = "New York"}
for key, value in pairs(person) do
    print(key .. ": " .. value)
end

-- 4. Functions
function greet(name)
    print("Hello, " .. name .. "!")
end
greet("Programmer")

-- Function with multiple return values
function getCoordinates()
    return 100, 200
end
local x, y = getCoordinates()
print("Coordinates: " .. x .. ", " .. y)

-- Anonymous functions (often used with higher-order functions)
local multiply = function(a, b)
    return a * b
end
print("Product: " .. multiply(5, 3))

-- Varargs (variable number of arguments)
function sumAll(...)
    local args = {...} -- Collect arguments into a table
    local total = 0
    for _, v in ipairs(args) do
        total = total + v
    end
    return total
end
print("Sum of numbers: " .. sumAll(1, 2, 3, 4, 5))

-- 5. Tables (more details)
local user = {}
user.name = "Bob"
user.age = 25
user["city"] = "London" -- Alternative way to access with string key

print(user.name)
print(user.age)

-- Nested tables
local company = {
    name = "Tech Corp",
    employees = {
        {name = "Alice", id = 1},
        {name = "Bob", id = 2}
    }
}
print(company.employees[1].name)

-- Table constructors can be very flexible
local mixedTable = {
    [1] = "first element",
    name = "mixed",
    100, -- will be at index 2
    "last element" -- will be at index 3
}
print(mixedTable[1])
print(mixedTable[2])
print(mixedTable.name)

-- 6. Modules (basic concept using `require`)
-- Lua uses `require` to load modules. A module is typically a table
-- returned by a Lua file.

-- Example of a simple module file (e.g., `my_module.lua`):
-- local M = {}
-- function M.sayHello()
--     print("Hello from my module!")
-- end
-- return M

-- How to use it in another file:
-- local myModule = require("my_module")
-- myModule.sayHello()

-- 7. Metatables and Metamethods (Advanced Concept)
-- Allows you to change the behavior of tables and other values.
local myObject = { value = 10 }
local myMetatable = {
    __add = function(a, b)
        return a.value + b.value
    end,
    __tostring = function(obj)
        return "My object with value: " .. obj.value
    end
}
setmetatable(myObject, myMetatable)

local anotherObject = { value = 20 }
setmetatable(anotherObject, myMetatable)

print(myObject + anotherObject) -- Uses __add metamethod
print(tostring(myObject))      -- Uses __tostring metamethod

-- 8. Error Handling
-- pcall (protected call) is used to catch errors
local success, result = pcall(function()
    error("Something went wrong!")
    return "This won't be reached"
end)

if not success then
    print("An error occurred: " .. result)
else
    print("Function executed successfully: " .. result)
end

-- assert (raises an error if condition is false)
local value = nil
-- assert(value, "Value cannot be nil!") -- This would stop execution

-- 9. Coroutines (Advanced Concept for Cooperative Multitasking)
local co = coroutine.create(function()
    for i = 1, 3 do
        print("Coroutine step " .. i)
        coroutine.yield() -- Pause execution
    end
end)

coroutine.resume(co) -- Resume execution
coroutine.resume(co)
coroutine.resume(co)
coroutine.resume(co) -- Trying to resume after completion returns false

-- 10. Environment (Globals)
-- Accessing global environment:
_G.myGlobal = "I'm in the global table"
print(_G.myGlobal)

-- Note: In modern Lua, `_ENV` is used for environment manipulation,
-- often implicitly when `local` is not used.

-- Example of a simple program flow
print("\n--- Program Flow Example ---")
local counter = 0
local function incrementAndPrint(limit)
    for i = 1, limit do
        counter = counter + 1
        print("Current counter: " .. counter)
        if counter % 2 == 0 then
            print("  (Even number!)")
        end
    end
end

incrementAndPrint(3)
print("Final counter value: " .. counter)