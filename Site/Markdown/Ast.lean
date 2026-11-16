namespace Site.Markdown

inductive Inline where
  | text (s : String)
  | emph (xs : List Inline)
  | strong (xs : List Inline)
  | code (s : String)
  | link (label : List Inline) (url : String)
  | image (alt : String) (src : String)
  | lineBreak
  /-- A margin note, written `^[like this]`. -/
  | note (xs : List Inline)
  /-- A reference to a footnote, written `[^label]`. -/
  | footRef (label : String)

inductive Block where
  | heading (level : Nat) (xs : List Inline)
  | para (xs : List Inline)
  | codeBlock (lang : Option String) (body : String)
  | quote (bs : List Block)
  | ulist (items : List (List Inline))
  | olist (start : Nat) (items : List (List Inline))
  | hr
  /-- A footnote's text, written `[^label]: text` on its own line. -/
  | footnote (label : String) (xs : List Inline)

instance : Inhabited Inline := ⟨.text ""⟩
instance : Inhabited Block := ⟨.hr⟩

end Site.Markdown
