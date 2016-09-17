| SYNTAX TEST "Packages/Markdown/Markdown.sublime-syntax"

# Heading
| <- markup.heading punctuation.definition.heading
|^^^^^^^^ markup.heading

## Second Heading
| <- markup.heading punctuation.definition.heading
|^^^^^^^^^^^^^^^^ markup.heading

Alternate Heading
=================
|^^^^^^^^^^^^^^^^ markup.heading.1 punctuation.definition

Paragraph of text that should be scoped as meta.paragraph.
|^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.paragraph
A [link](https://example.com), *italic text* and **bold**.
| ^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.link.inline
| ^ punctuation.definition.string.begin
|      ^ punctuation.definition.string.end
|       ^ punctuation.definition.metadata
|        ^^^^^^^^^^^^^^^^^^^ markup.underline.link
|                           ^ punctuation.definition.metadata
|                              ^^^^^^^^^^^^^ markup.italic
|                              ^ punctuation.definition.italic
|                                          ^ punctuation.definition.italic
|                                                ^^ punctuation.definition.bold
|                                                ^^^^^^^^ markup.bold
|                                                      ^^ punctuation.definition.bold

Inline `code sample`.
|      ^^^^^^^^^^^^^ markup.raw.inline
|      ^ punctuation.definition.raw
|                  ^ punctuation.definition.raw

Here is a [reference link][name].
|         ^^^^^^^^^^^^^^^^^^^^^^ meta.link.reference
|                         ^ punctuation.definition.constant.begin
|                          ^^^^ constant.other.reference.link
|                              ^ punctuation.definition.constant.end

Here is a [blank reference link][].
|         ^^^^^^^^^^^^^^^^^^^^^^ meta.link.reference
|                               ^ punctuation.definition.constant.begin
|                                ^ punctuation.definition.constant.end

Here is a ![Image Alt Text](htts://example.com/cat.git).
|         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.image.inline
|          ^ punctuation.definition.string.begin
|                         ^ punctuation.definition.string.end
|                          ^ punctuation.definition.metadata
|                           ^^^^^^^^^^^^^^^^^^^^^^^^^^ markup.underline.link.image
|                                                     ^ punctuation.definition.metadata

Here is a ![Image Ref Alt][1].
|         ^^^^^^^^^^^^^^^^^^^ meta.image.reference
|          ^ punctuation.definition.string.begin
|                        ^ punctuation.definition.string.end
|                         ^ punctuation.definition.constant
|                          ^ constant.other.reference.link
|                           ^ punctuation.definition.constant


  1. Ordered list item
|  ^ punctuation.definition.list_item
  2. Ordered list item #2
| ^^^^^^^^^^^^^^^^^^^^^^^ markup.list.numbered

Paragraph break.

  - Ordered list item
| ^ punctuation.definition.list_item
  - Ordered list item #2
| ^^^^^^^^^^^^^^^^^^^^^^ markup.list.unnumbered

Paragraph break.

  * Ordered list item
| ^ punctuation.definition.list_item
  + Ordered list item #2
| ^^^^^^^^^^^^^^^^^^^^^^ markup.list.unnumbered

Paragraph break.

> This is a block quote. It contains markup.
> Including things like *italics*
|^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ markup.quote
|                       ^^^^^^^^^ markup.italic

Paragraph break.

--------
|^^^^^^^ meta.separator

[1]: https://google.com
| <- meta.link.reference.def
|^^^^^^^^^^^^^^^^^^^^^^ meta.link.reference.def
|^ constant.other.reference.link
|  ^ punctuation.separator.key-value
}    ^^^^^^^^^^^^^^^^^^ markup.underline.link
