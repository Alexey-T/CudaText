# My Markdown Syntax Demonstration

This document showcases various **Markdown** formatting features. Markdown is often used for `README.md` files, documentation, and web content due to its simplicity.

---

## 1. Headings (H1 to H6)

# Heading 1 (using #)
## Heading 2 (using ##)
### Heading 3 (using ###)
#### Heading 4 (using ####)
##### Heading 5 (using #####)
###### Heading 6 (using ######)

Alternatively, you can use underlines for H1 and H2:

Heading 1 (using ===)
======================

Heading 2 (using ---)
----------------------

---

## 2. Text Formatting

This is *italic text* using a single asterisk.
This is _italic text_ using a single underscore.

This is **bold text** using double asterisks.
This is __bold text__ using double underscores.

This is ***bold and italic text***.
This is ___bold and italic text___.

This is `inline code` for small snippets.

This is ~~strikethrough text~~.

---

## 3. Lists

### Unordered Lists

* Item one (using asterisk)
* Item two
  * Nested item 2.1
  * Nested item 2.2
    * Double nested item 2.2.1
* Item three

- Item one (using hyphen)
- Item two

+ Item one (using plus sign)
+ Item two

### Ordered Lists

1. First item
2. Second item
   1. Nested ordered item 2.1
   2. Nested ordered item 2.2
3. Third item

---

## 4. Code Blocks

### Inline Code

Wrap code with backticks: `print("Hello, World!")` in Python.

### Fenced Code Blocks (recommended)

Use three backticks (```) or three tildes (~~~). You can specify the language for syntax highlighting.

```python
def factorial(n):
    if n == 0:
        return 1
    else:
        return n * factorial(n-1)

print(factorial(5))
```