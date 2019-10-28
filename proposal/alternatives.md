# Scheme 1
Naked `.lbl` is illegal.

## Lexer

A new lexeme *fieldid* is introduced.
<br/>
<br/>*lexeme* → *qvarid* | *qconid* | *qvarsym* | *qconsym*
| *literal* | *special* | *reservedop* | *reservedid* | *fieldid*
<br/>*fieldid* → *.varid{.varid}*

## Parser

#### Field selections

To support field selection the *fexp* production is extended.
<br/>
<br/>*fexp*	→ [ *fexp* ] *aexp* | *fexp* *fieldid*

#### Field updates

To support field update, the *aexp* production is extended.
<br/>
<br> *aexp* → *aexp⟨qcon⟩* { *pbind* , … , *pbind* }
<br/>*pbind* -> *qvar*=*exp* | *var* *fieldid*=*exp*

### Sections

To support sections (e.g. `(.foo.bar.baz)`), we generalize *aexp*.
<br/>
<br/>*aexp* →	( *infixexp* *qop* ) (left section)
          | ( *qop* *infixexp* )	 (right section)
          | ( *fieldid* )           (projection (right) section)

# Scheme 2
Naked `.lbl` means `(\x -> x.lbl)`.

## Lexer

A new lexeme *fieldid* is introduced.
<br/>
<br/>*lexeme* → *qvarid* | *qconid* | *qvarsym* | *qconsym*
| *literal* | *special* | *reservedop* | *reservedid* | *fieldid*
<br/>*fieldid* → *.varid{.varid}*

## Parser

### Sections

To support sections  (e.g. `.foo.bar.baz`), we generalize *aexp*.
<br/>
<br/>*aexp* → *fieldid*

## Field selections

To support field selections, the existing production *fexp* → *[fexp]* *aexp* is sufficient.

### Field updates

To support field updates, the *aexp* production is extended.
<br/>
<br> *aexp* → *aexp⟨qcon⟩* { *pbind* , … , *pbind* }
<br/>*pbind* -> *qvar*=*exp* | *var* *aexp*=*exp*

# Scheme 3
Naked `.lbl` is allowed in a function application.

## Lexer

A new lexeme *fieldid* is introduced.
<br/>
<br/>*lexeme* → *qvarid* | *qconid* | *qvarsym* | *qconsym*
| *literal* | *special* | *reservedop* | *reservedid* | *fieldid*
<br/>*fieldid* → *.varid*

## Parser

### Field selections

To support field selection the *fexp* production is extended.
<br/>
<br/>*fexp*	→ [ *fexp* ] *aexp* | *fexp* *fieldid*

### Field updates

To support field update, the *aexp* production is extended.
<br/>
<br> *aexp* → *aexp⟨qcon⟩* { *pbind* , … , *pbind* }
<br/>*pbind* -> *qvar*=*exp* | *var* *fieldids*=*exp*
<br/>*fieldids* -> *fieldids* *fieldid*

### Sections

To support sections (e.g. `(.foo.bar.baz)`), we generalize *aexp*.
<br/>
<br/>*aexp* →	( *infixexp* *qop* ) (left section)
          | ( *qop* *infixexp* )	 (right section)
          | ( *fieldids* )           (projection (right) section)
