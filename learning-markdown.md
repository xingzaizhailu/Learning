[Learning Markdown](http://daringfireball.net/projects/markdown/syntax)
=======================================

## Some Tips
* Markdown formatting syntax is **not** processed within ***block-level*** HTML tags.
* Span-level HTML tags -e.g. `<span>`, `<cite>`, or `<del>`- can be used anywhere in a Markdown paragraph, list 
  item, or header.
* `<a>` or `<img>` tags are supported by Markdown
* Escape left angle bracket `<`， `——` and ampersands `&` as entities, e.g. `&lt;`, `&mdash;` and `&amp;`.

## Block Elements
### Paragraphs and Line Breaks:
A line containing nothing but spaces or tabs **is** considered blank.  
End a line with two or more spaces rather than `<br />`
### Headers:
1. #### Setext  
		This is an H1  
		=============
		This is an H2  
		-------------  
    **will be:**
    
    This is an H1
    =============
    This is an H2
    -------------
2. #### Atx  
    Use 1-6 hash caracters `#`  
    	# This is an H1  
    	## This is an H2  
    	###### This is an H6  

	**will be**    
	# This is an H1
	## This is an H2
	###### This is an H6
### Blockquotes:
* Use email-style `>`
* Blockquotes can be **nested** by adding additional >

		> This is the first level  
		> > This is nested blockquote  
		>
		> Back to the first level  
	**will be**  
	> This is the first level
	> > This is nested blockquote   
	> 
	> Back to the first level
* Blockquotes **can** contain other Markdown elements, including headers, lists, and code blocks:

		> ## This is a header  
		>  
		> 1.  This is the first list item.  
		> 2.  This is the Second list item.    
	**will be**  
	> ## This is a header  
	>  
	> 1.  This is the first list item.  
	> 2.  This is the Second list item.
### Lists
1. #### Unordered lists:
    Use asterisks, pluses, and hyphens    

		* Red  
		+ Yellow  
		- Blue  
2. #### Ordered lists:  
    Use numbers followed by periods

		1. McHale  
		2. Parish  
!: Actual numbers you use to mark the list have **no** effect on the HTML output  

If list items are seprated by blank lines, Markdown will wrap the items in `<tag>` tags.  
Multi lines or elements within a list should be indented. (And the code block needs to be indented twice)  
To avoid **"1986. What a great season."** becomes a ordered list, you can backslash-escape the perid like **"1986\\."**
### Code Blocks
Simply indent every line of the block by at least 4 spaces or 1 tab.  

	This is a normal parapraph    
		This is a code block.  
Within a code block, ampersands (&) and angle brackets (< and >) are automatically converted into HTML entities.
### Horizontal Rules
Produce a horizontal rule tag `<hr />` by placing three or more hypens, asterisks, or underscores on a line by themself.

## Span Elements
### Links
1. #### inline  
		[link text is delimited by [square brackets]](http://example.com/ "Title")  
	**will be**  
	[link text is delimited by [square brackets]](http://example.com/ "Title")  

	If you refering ot a local resource, you can use relative paths: `[](/a/local/direction/)`  

2. #### reference
    More readable.

    	This is [an example][id] reference-style link.
    	[id]: http://example.com/ "Optional Title Here"
    
    	[link text itself as the name of link][]
    	[link text itself as the name of link]: http://google.com/
      
  **will be**  

    This is [an example] [id] reference-style link.  
    [Link text itself as the name of link][]  
    [link text itself as the name of link]: http://google.com/
    [id]: http://example.com/ "Optional Title Here"  
    
**!:** Link definition names are **not** case sensitive
### Emphasis
	*single asterisks*  
	_single underscores_

	**double asterisks**  
	__double underscores__  

	un*frigging*believable  //used in midle of a word
**will be**  

*single asterisks*  
_single underscores_

**double asterisks**  
__double underscores__  

un*frigging*believable
### Code  
To indicate a span of code, wrap it with backtick quotes (`).  
To include a literal backtick character within a code span, you can use multiple backticks as the opening and closing delimiters:

	``There is a literal backtick (`) here.``
	`` ` ``
	`` `foo` ``  

**will be**  
``There is a literal backtick (`) here.``  
`` ` ``  
`` `foo` ``  
### Images  
1. #### inline

        ![Alt text](/path/to/img.jpg "Optional title")
2. #### reference

		![Alt text][id]
		[id]: url/to/image "Optional title attribute"  

## Miscellaneous
### Automatic Links  
	<http://example.com/>
**will be**  
	<http://example.com/>  
### Backslash Escapes
Markdown provides backslash escapes for the following characters:
	
	\   backslash
	`   backtick
	*   asterisk
	_   underscore
	{}  curly braces
	[]  square brackets
	()  parentheses
	#   hash mark
	+   plus sign
	-   minus sign (hyphen)
	.   dot
	!   exclamation mark


