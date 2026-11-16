---
title: Test post
date: 2026-11-16
summary: A throwaway post for checking the layout.
domo: nom.
---

Some text with a [link](/), some `inline code`,[^code] and enough words to
see how a paragraph wraps inside the post's body column.^[A margin note.
It sits beside the line it was written on.] Here is some more text after
the note, so the paragraph keeps going for a while and the note has
something to sit beside.

## A section

A code block[^lean]:^[Notes can have *emphasis*, `code` and [links](/).]

```lean
def x : Nat := 1
```

### A subsection

- one
- two, with a note inside a list^[Notes inside lists still reach the column.]
- three, with a picture in its note^[Domo, at his own size:
  ![Domo](/img/domo-open.svg) Images sit on their own line.]

## Another section

> A quote, to see how it sits in the post.^[And one inside a quote.]

Two notes close together^[The first, and this one runs on for several lines, so the note after it has to lie over the rest of its text. Only this first line stays clear; hover it to read the whole note.] overlap.^[The second, lying over the
rest of the first, whose text fades out behind it.] Hover either note to bring
it to the front.

[^code]: Inline code is set in the monospace font, a little smaller.
[^lean]: The language name in the bar comes from the code fence, here
  `lean`. Footnotes can run over several lines when the lines after the
  first are indented.
