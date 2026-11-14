namespace Site.Markdown

inductive Inline where
  | text (s : String)
  | emph (xs : List Inline)
  | strong (xs : List Inline)
  | code (s : String)
  | link (label : List Inline) (url : String)
  | image (alt : String) (src : String)
  | lineBreak

inductive Block where
  | heading (level : Nat) (xs : List Inline)
  | para (xs : List Inline)
  | codeBlock (lang : Option String) (body : String)
  | quote (bs : List Block)
  | ulist (items : List (List Inline))
  | olist (start : Nat) (items : List (List Inline))
  | hr

instance : Inhabited Inline := ⟨.text ""⟩
instance : Inhabited Block := ⟨.hr⟩

end Site.Markdown
