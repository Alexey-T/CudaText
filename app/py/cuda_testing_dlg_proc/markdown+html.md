
# Supported Markdown

# h1
## h2
### h3
#### h4
##### h5
###### h6

**bold** __text__

*italic* _text_

***bold and italic***

> blockquote


### ___ Code ___

inline  `code` like `alert(hw);`


```python
# Fenced Code Block
with open(file_name, 'r') as file: # 'r' for read
    content = file.read()
    print("\nFile content (read all at once):\n", content)

```


    // javascript: indented Code Block
    var hw = "Hello World!"
    alert(hw);
    

### ___ Table without alignment ___
| Syntax      | Description | Test Text     |
| ---         | ----        | ---           |
| Header      | Title       |  this |
| Paragraph   | Text        | more |


### ___ Unordered List ___
- George Washington
* John Adams
+ Thomas Jefferson


### ___ Ordered List ___
1. James Madison
2. James Monroe
3. John Quincy Adams


### ___ Nested Lists ___
1. First list item
   - First nested list item
     - Second nested list item


### ___ hr Horizontal Rule : --- and *** ___
---
***


### ___ br Break: Two spaces at the end of the first line: ___
line one  
line two

two lines
becomes one line


### ___ Escaping/Ignoring Markdown formatting ___
Let's rename \*our-new-project\* to \*our-old-project\*.




_______________________________________________________________________________________________________________

# Not Supported in our Markdown YET

~~Strikethrough~~

subscript <sub>subscript</sub>

superscript <sup>superscript</sup>


### ___ Table with alignment ___

| Syntax      | Description | Test Text     |
| :---        |    :----:   |          ---: |
| Header      | Title       |  this |
| Paragraph   | Text        | more |


### ___ Task lists ___
- [x] #739
- [ ] https://github.com/octo-org/octo-repo/issues/740
- [ ] Add delight to the experience when all tasks are complete :tada:


### ___ Emojis ___
@octocat :+1: :smile: :shipit:
  
  
### ___ Images ___
does not download from internet

![internet image](https://github.githubassets.com/favicons/favicon.png)

![local image](markdown.png)


### ___ Alerts ___
> [!NOTE]
> Useful information that users should know, even when skimming content.

> [!TIP]
> Helpful advice for doing things better or more easily.

> [!IMPORTANT]
> Key information users need to know to achieve their goal.

> [!WARNING]
> Urgent info that needs immediate user attention to avoid problems.

> [!CAUTION]
> Advises about risks or negative outcomes of certain actions.


### ___ Footnotes ___
Here is a simple footnote[^1].

A footnote can also have multiple lines[^2].

[^1]: My reference.
[^2]: To add line breaks within a footnote, prefix new lines with 2 spaces.
  This is a second line.
  
  
### ___ More ___

This exist in markdown but not supported in Github or Stackoverflow:
  
highlight ==very important words==

highlight <mark>very important words</mark>

math: $-b \pm \sqrt{b^2 - 4ac} \over 2a$

______________________________________

source:
  
[GitHub markdown](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)  
[Commonmark markdown](https://commonmark.org/help/)  
[Stackoverflow markdown](https://stackoverflow.com/editing-help)  
[Markdownguide](https://www.markdownguide.org/basic-syntax/)  



_____________________________________


# HTML inside markdown is not supported yet
Note that Stackoverflow only support a very strict subset of HTML! Github support more if not all , https://meta.stackexchange.com/questions/1777/what-html-tags-are-allowed


<p><strong>Strong</strong> _ <b>Bold</b> _ <em>emphasis</em> _ <i>italic</i> _ <u>unarticulated</u> _ <ins>inserted</ins> _ <del>deleted</del> _ <s>strikethrough</s> _ <strike>strike</strike> _ <sup>Superscript</sup> _ <sub>Subscript</sub> _ <small>small</small> _ <ruby><rb>ruby</rb><rt>annotation</rt></ruby> _ <abbr title="HyperText Markup Language">Abbreviation</abbr> _ <q cite="https://developer.mozilla.org/en-US/docs/HTML/Element/q">q cite</q> _ <cite>cite</cite> _ <dfn>definition</dfn> _ <mark>mark</mark> _ <var>variable</var> _ <time datetime="2013-04-06T12:32+00:00">time</time> _ <kbd>kbd</kbd> _ <code>&lt;div&gt;code&lt;/div&gt;</code> _ <samp>sample</samp> _ <bdo dir="rtl"> right to left :odb</bdo></p>
<pre>pre: P R E F O R M A T T E D   T E X T</pre>


<blockquote>
blockquote: A block quotation. <cite>cite: Said by me.</cite>
</blockquote>

<details>
<summary>Expand for details</summary>
<p>Lorem ipsum dolor sit amet consectetur adipisicing elit</p>
</details>

<address>
Address:<br>
Box 564, Disneyland
</address>

<h3>br: 2 break lines</h3>
111
<br>
222
<br>

<h3>hr: Horizontal rules</h3>
<hr>
