## Regular Expression

### Basic Concepts
#### Boolean "or"
A vertical bar(`|`) separates alternatives.  
- gray|grey   # match "gray" or "grey"

#### Grouping
Parentheses are used to define the scope and precedence of the operators
- gr(a|e)y    # also match "gray" or "grey"

#### Quantification
A quantifier after a token (such as a character) or group specifies how often that preceding element is allowed to occur.  
- .     Matches any single character (many applications exclude newlines, and exactly which characters are considered newlines is flavor-, character-encoding-, and platform-specific, but it is safe to assume that the line feed character is included). Within POSIX bracket expressions, the dot character matches a literal dot. For example, a.c matches "abc", etc., but [a.c] matches only "a", ".", or "c".  
- ?         The question mark indicates **zero or one** occurrences of the preceding element. For example, colou?r matches both "color" and "colour".  
- *         The asterisk indicates **zero or more** occurrences of the preceding element. For example, ab\*c matches "ac", "abc", "abbc", "abbbc", and so on.  
- +         The plus sign indicates **one or more** occurrences of the preceding element. For example, ab+c matches "abc", "abbc", "abbbc", and so on, but not "ac".  
- {n}       The preceding item is matched exactly n times.  
- {min,}    The preceding item is matched min or more times.
- {min,max} The preceding item is matched at least min times, but not more than max times.
    - For example, a{3,5} matches only "aaa", "aaaa", and "aaaaa". This is not found in a few older instances of regexes. BRE mode requires \{m,n\}.  

### POSIX basic and extended
- ^     Matches the starting position within the string. In line-based tools, it matches the starting position of any line.  
- $     Matches the ending position of the string or the position just before a string-ending newline. In line-based tools, it matches the ending position of any line.  
- \b    word boundry. `nt\b` matches "nt" in "paint", but not in "pants".  
- \B    word non-boundry. `all\B` matches "all" in "ally", but not in "wall".  
- []    A bracket expression. Matches a single character that is contained within the brackets. For example, [abc] matches "a", "b", or "c". [a-z] specifies a range which matches any lowercase letter from "a" to "z". These forms can be mixed: [abcx-z] matches "a", "b", "c", "x", "y", or "z", as does [a-cx-z].
       The - character is treated as a literal character if it is the last or the first (after the ^, if present) character within the brackets: [abc-], [-abc]. Note that backslash escapes are not allowed. The ] character can be included in a bracket expression if it is the first (after the ^) character: []abc].
- [^]   Matches a single character that is not contained within the brackets. For example, [^abc] matches any character other than "a", "b", or "c". [^a-z] matches any single character that is not a lowercase letter from "a" to "z". Likewise, literal characters and ranges can be mixed.  
- ()    Defines a marked subexpression. The string matched within the parentheses can be recalled later (see the next entry, \n). A marked subexpression is also called a block or capturing group. BRE mode requires \( \).
- \n    Matches what the nth marked subexpression matched, where n is a digit from 1 to 9. This construct is vaguely defined in the POSIX.2 standard. Some tools allow referencing more than nine capturing groups.  

### Shortcuts
- \d    any digit: [0-9]  
- \D    any non-digit: [^0-9]  
- \w    any alphanumeric/underscore: [a-zA-Z0-9\_]  
- \W    any non-alphanumeric: [^a-zA-Z0-9\_]  
- \s    whitespace(space, tab): [ \r\t\n\f]
- \S    non-whitespace: [^ \r\t\n\f]


e.g.
- .at matches any three-character string ending with "at", including "hat", "cat", and "bat".
- [hc]at matches "hat" and "cat".
- [^b]at matches all strings matched by .at except "bat".
- [^hc]at matches all strings matched by .at other than "hat" and "cat".
- ^[hc]at matches "hat" and "cat", but only at the beginning of the string or line.
- [hc]at$ matches "hat" and "cat", but only at the end of the string or line.
- \[.\] matches any single character surrounded by "[" and "]" since the brackets are escaped, for example: "[a]" and "[b]".
- s.\* matches s followed by zero or more characters, for example: "s" and "saw" and "seed".
